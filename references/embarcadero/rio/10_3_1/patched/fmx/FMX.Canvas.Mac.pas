{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2018 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Canvas.Mac;

{$H+}

interface

uses
  System.Types, System.Math.Vectors, System.UIConsts, System.Classes, System.SysUtils, System.Math, System.UITypes,
  System.Character, System.Generics.Collections, Macapi.CocoaTypes, Macapi.CoreGraphics, Macapi.CoreFoundation,
  Macapi.ImageIO, Macapi.CoreText, Macapi.Helpers, FMX.Types, FMX.Platform, FMX.Printer, FMX.Printer.Mac, FMX.Consts,
  FMX.Forms, FMX.TextLayout, FMX.Surfaces, FMX.Graphics;

{$SCOPEDENUMS ON}

type

  { TQuartzCanvasSaveState }

  /// <summary>Canvas's state implementation for OS X Canvas</summary>
  TQuartzCanvasSaveState = class(TCanvasSaveState)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Assign(Source: TPersistent); override;
  end;

  { TBitmapCodecQuartz }

  /// <summary>OS X bitmap codec implementation class</summary>
  TBitmapCodecQuartz = class(TCustomBitmapCodec)
  protected
    /// <summary>Helper method that convert CGImageRef to TBitmapSurface. If size of image is bigger than MaxSize
    /// result surface is fitted to MaxSize area. </summary>
    function ImageToSurface(const Img: CGImageRef; const Bitmap: TBitmapSurface; const MaxSize: Cardinal): Boolean;
    /// <summary>Converting file extention to UTType enumeration</summary>
    function ExtensionToEncoder(const AExt: string): CFStringRef;
  public
    /// <summary>Returns size of image</summary>
    class function GetImageSize(const AFileName: string): TPointF; override;
    /// <summary>Validate stream if there is an image source.</summary>
    class function IsValid(const AStream: TStream): Boolean; override;
    /// <summary>Load bitmap surface from file</summary>
    function LoadFromFile(const AFileName: string; const Bitmap: TBitmapSurface;
      const MaxSizeLimit: Cardinal): Boolean; override;
    /// <summary>Load or create thumbnail of bitmap</summary>
    function LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: Single; const UseEmbedded: Boolean;
      const Bitmap: TBitmapSurface): Boolean; override;
    /// <summary>Save bitmap surface to file</summary>
    function SaveToFile(const AFileName: string; const Bitmap: TBitmapSurface;
      const SaveParams: PBitmapCodecSaveParams = nil): Boolean; override;
    /// <summary>Load bitmap surface from stream</summary>
    function LoadFromStream(const AStream: TStream; const Bitmap: TBitmapSurface;
      const MaxSizeLimit: Cardinal): Boolean; override;
    /// <summary>Load bitmap surface to stream</summary>
    function SaveToStream(const AStream: TStream; const Bitmap: TBitmapSurface; const Extension: string;
       const SaveParams: PBitmapCodecSaveParams = nil): Boolean; override;
  end;

  /// <summary>Internal repesentation of Bitmap, can be accesed from TBitmap.Handle.</summary>
  TQuartzBitmap = class
  private
    FData: Pointer;
    FContext: CGContextRef;
    FImage: CGImageRef;
  protected
    /// <summary>Return or create native represenation of bitmap.</summary>
    function GetImage: CGImageRef; virtual;
  public
    /// <summary>Native represenation of bitmap.</summary>
    property Image: CGImageRef read GetImage;
  end;

  { TCanvasQuartz }

  /// <summary>OS X implementation of canvas uses Quartz framework.</summary>
  TCanvasQuartz = class(TCanvas)
  private
    FFunc: CGFunctionRef;
    FBitmapRef: CGImageRef;
    FCallback: CGFunctionCallbacks;
    FShading: CGShadingRef;
    FContext: CGContextRef;
    FFontScale: Single;
  protected
    /// <summary>Set current clip region be set of rects.</summary>
    procedure SetClipRects(const ARects: array of TRectF); virtual;
    /// <summary>Apply fill settings</summary>
    procedure ApplyFill(const ABrush: TBrush; const ARect: TRectF; const AOpacity: Single); virtual;
    /// <summary>Remove fill settings</summary>
    procedure RemoveFill(const ABrush: TBrush; const ARect: TRectF; const AOpacity: Single); virtual;
    /// <summary>Apply stroke settings</summary>
    procedure ApplyStroke(const AStroke: TStrokeBrush; const ARect: TRectF; const AOpacity: Single); virtual;
    /// <summary>Create current canvas state.</summary>
    function CreateSaveState: TCanvasSaveState; override;
    /// <summary>Calculates the origin of the Canvas depending on the output device;
    /// the result is useful for the coordinate system inversion </summary>
    function CalcOrigin: TPointF; virtual;
    /// <summary>Return current scale factor</summary>
    function GetScaleFactor: TPointF; virtual;
    /// <summary>Adapts the coordinate system to make it usable as on Win32; for printers
    /// it adapts the coordinate system units to the selected printer resolution </summary>
    procedure AdaptCoordinateSystem; virtual;
    /// <summary>Setup canvas to drawing mode.</summary>
    function DoBeginScene(const AClipRects: PClipRects = nil; AContextHandle: THandle = 0): Boolean; override;
    /// <summary>Finishing canvas drawing.</summary>
    procedure DoEndScene; override;
    /// <summary>Return current canvas scale.</summary>
    function GetCanvasScale: Single; override;
    /// <summary>Initialize Canvas for form.</summary>
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
      const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault); override;
    /// <summary>Initialize Canvas for bitmap.</summary>
    constructor CreateFromBitmap(const ABitmap: TBitmap; const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault); override;
    /// <summary>Initialize Canvas for printer.</summary>
    constructor CreateFromPrinter(const APrinter: TAbstractPrinter); override;
    /// <summary>Initialize bitmap handle.</summary>
    class function DoInitializeBitmap(const Width, Height: Integer; const Scale: Single;
      var PixelFormat: TPixelFormat): THandle; override;
    /// <summary>Finalize bitmap handle.</summary>
    class procedure DoFinalizeBitmap(var Bitmap: THandle); override;
    /// <summary>Map bitmap to read/write.</summary>
    class function DoMapBitmap(const Bitmap: THandle; const Access: TMapAccess; var Data: TBitmapData): Boolean; override;
    /// <summary>UnMap bitmap after reading/writing.</summary>
    class procedure DoUnmapBitmap(const Bitmap: THandle; var Data: TBitmapData); override;
    /// <summary>Apply drawing matrix.</summary>
    procedure DoSetMatrix(const M: TMatrix); override;
    /// <summary>Fill rectangle using ABrush settings.</summary>
    procedure DoFillRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); override;
    /// <summary>Fill path using ABrush settings.</summary>
    procedure DoFillPath(const APath: TPathData; const AOpacity: Single; const ABrush: TBrush); override;
    /// <summary>Fill ellipse using ABrush settings.</summary>
    procedure DoFillEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); override;
    /// <summary>Draw rect of bitmap from SrcRect to DstRect in the canvas.</summary>
    procedure DoDrawBitmap(const ABitmap: TBitmap; const SrcRect, DstRect: TRectF; const AOpacity: Single;
      const HighSpeed: Boolean = False); override;
    /// <summary>Draw line using ABrush settings.</summary>
    procedure DoDrawLine(const APt1, APt2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    /// <summary>Draw rect using ABrush settings.</summary>
    procedure DoDrawRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    /// <summary>Draw path using ABrush settings.</summary>
    procedure DoDrawPath(const APath: TPathData; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    /// <summary>Draw ellipse using ABrush settings.</summary>
    procedure DoDrawEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    /// <summary>Link to native representation of Canvas's bitmap.</summary>
    property BitmapRef: CGImageRef read FBitmapRef;
    /// <summary>Link to native representation of Canvas.</summary>
    property Context: CGContextRef read FContext write FContext;
    /// <summary>Current font scale.</summary>
    property FontScale: Single read FFontScale write FFontScale;
  public
    /// <summary>Clear whole canvas using Color.</summary>
    procedure Clear(const Color: TAlphaColor); override;
    /// <summary>Clear rectangular area of canvas using Color.</summary>
    procedure ClearRect(const ARect: TRectF; const AColor: TAlphaColor = 0); override;
    /// <summary>Set current clipping area using intersection of current area and rectangle.</summary>
    procedure IntersectClipRect(const ARect: TRectF); override;
    /// <summary>Exclude rectangular area from current clipping area.</summary>
    procedure ExcludeClipRect(const ARect: TRectF); override;
    /// <summary>Returns true if APoint located in the path.</summary>
    function PtInPath(const APoint: TPointF; const APath: TPathData): Boolean; override;
    /// <summary> The X and Y factors by which the canvas is scaled </summary>
    property ScaleFactor: TPointF read GetScaleFactor;
  end;

  /// <summary>OS X TTextLayout implementation, which uses CoreText framework.</summary>
  TTextLayoutCT = class(TTextLayout)
  protected const
    /// <summary> Rotating matrix to simulate Italic font attribute </summary>
    ItalicMatrix: CGAffineTransform = (
      a: 1;
      b: 0;
      c: 0.176326981; //~tan(10 degrees)
      d: 1;
      tx: 0;
      ty: 0
    );
  private
    FCTFrame: CTFrameRef;
    FCTAttr: CFMutableAttributedStringRef;
    FCTPath: CGMutablePathRef;
    FTextWidth: Single;
    FTextLeft: Single;
    FTextHeight: Single;
    FLastColor: TAlphaColor;
    FLastOpacity: Single;
    FFontScale: Single;
    procedure CreateCT;
    procedure MeasureCT;
    procedure CreateCTFrame;
    function GetCTFontRef(const AFont: TFont): CTFontRef;
  protected
    /// <summary>Rendering text layout.</summary>
    procedure DoRenderLayout; override;
    /// <summary>Draw text layout to canvas.</summary>
    procedure DoDrawLayout(const ACanvas: TCanvas); override;
    /// <summary>Measure text layout width.</summary>
    function GetTextHeight: Single; override;
    /// <summary>Measure text layout height.</summary>
    function GetTextWidth: Single; override;
    /// <summary>Measure text layout rectangle.</summary>
    function GetTextRect: TRectF; override;
    /// <summary>Returns text position by point position.</summary>
    function DoPositionAtPoint(const APoint: TPointF): Integer; override;
    /// <summary>Return set of rectnagles for a characters in range.</summary>
    function DoRegionForRange(const ARange: TTextRange): TRegion; override;
  public
    constructor Create(const ACanvas: TCanvas = nil); override;
    destructor Destroy; override;
    /// <summary>Convert text outlines to path.</summary>
    procedure ConvertToPath(const APath: TPathData); override;
  end;

/// <summary>Register OS X Canvas classes.</summary>
procedure RegisterCanvasClasses;
/// <summary>Unregister OS X Canvas classes.</summary>
procedure UnregisterCanvasClasses;

implementation {===============================================================}

uses
  Macapi.PrintCore;

var
  GlobalColorSpace: CGColorSpaceRef;

function ColorSpace: CGColorSpaceRef;
begin
  if GlobalColorSpace = nil then
    GlobalColorSpace := CGColorSpaceCreateDeviceRGB;
  Result := GlobalColorSpace;
end;

{ TBitmapCodecQuartz }

class function TBitmapCodecQuartz.GetImageSize(const AFileName: string): TPointF;
var
  Path: CFStringRef;
  Url: CFURLRef;
  ImgSourceRef: CGImageSourceRef;
  ImgRef: CGImageRef;
begin
  Result := PointF(0, 0);
  Path := CFStringCreateWithCString(nil, MarshaledAString(UTF8Encode(AFileName)), kCFStringEncodingUTF8);
  try
    Url := CFURLCreateWithFileSystemPath(nil, Path, kCFURLPOSIXPathStyle, False);
    try
      ImgSourceRef := CGImageSourceCreateWithURL(Url, nil);
      if ImgSourceRef <> nil then
      try
        ImgRef := CGImageSourceCreateImageAtIndex(ImgSourceRef, 0, nil);
        if ImgRef <> nil then
        try
          Result := TPointF.Create(CGImageGetWidth(ImgRef), CGImageGetHeight(ImgRef));
        finally
          CGImageRelease(ImgRef);
        end;
      finally
        CFRelease(imgSourceRef);
      end;
    finally
      CFRelease(Url);
    end;
  finally
    CFRelease(Path);
  end;
end;

class function TBitmapCodecQuartz.IsValid(const AStream: TStream): Boolean;
var
  Provider: CGDataProviderRef;
  MemStream: TMemoryStream;
  ImgSourceRef: CGImageSourceRef;
  SavePosition: Integer;
begin
  Result := False;
  SavePosition := AStream.Position;
  try
    MemStream := TMemoryStream.Create;
    MemStream.CopyFrom(AStream, AStream.Size - AStream.Position);
    MemStream.Position := 0;

    Provider := CGDataProviderCreateWithData(nil, MemStream.Memory, MemStream.Size, nil);
    if Provider <> nil then
    try
      ImgSourceRef := CGImageSourceCreateWithDataProvider(Provider, nil);
      if ImgSourceRef <> nil then
      try
        Result := True;
      finally
        CFRelease(ImgSourceRef);
      end;
    finally
      CGDataProviderRelease(Provider);
    end;
    MemStream.Free;
  finally
    AStream.Position := SavePosition;
  end;
end;

function TBitmapCodecQuartz.ImageToSurface(const Img: CGImageRef; const Bitmap: TBitmapSurface;
  const MaxSize: Cardinal): Boolean;
var
  R: TRectF;
  CtxRef: CGContextRef;
  Width, Height: Cardinal;
begin
  Result := False;
  Width := CGImageGetWidth(Img);
  Height := CGImageGetHeight(Img);
  if (MaxSize > 0) and ((Width > MaxSize) or (Height > MaxSize)) then
  begin
    R := TRectF.Create(0, 0, Width, Height);
    R.Fit(TRectF.Create(0, 0, MaxSize, MaxSize));
    Width := Trunc(R.Width);
    Height := Trunc(R.Height);
  end;

  Bitmap.SetSize(Width, Height, TPixelFormat.RGBA);

  CtxRef := CGBitmapContextCreate(Bitmap.Bits, Bitmap.Width, Bitmap.Height, 8,
     Bitmap.Width * 4, ColorSpace, kCGImageAlphaPremultipliedLast);
  if CtxRef <> nil then
  try
    CGContextDrawImage(CtxRef, CGRectFromRect(RectF(0, 0, Bitmap.Width, Bitmap.Height)), Img);

    Result := True;
  finally
    CGContextRelease(CtxRef);
  end;
end;

function TBitmapCodecQuartz.LoadFromStream(const AStream: TStream; const Bitmap: TBitmapSurface;
  const MaxSizeLimit: Cardinal): Boolean;
var
  Provider: CGDataProviderRef;
  CopyStream: TMemoryStream;
  ImgSourceRef: CGImageSourceRef;
  ImgRef: CGImageRef;
begin
  Result := False;
  CopyStream := TMemoryStream.Create;
  try
    CopyStream.CopyFrom(AStream, AStream.Size);
    CopyStream.Position := 0;
    Provider := CGDataProviderCreateWithData(nil, CopyStream.Memory, CopyStream.Size, nil);
    if Provider <> nil then
    try
      ImgSourceRef := CGImageSourceCreateWithDataProvider(Provider, nil);
      if ImgSourceRef <> nil then
      try
        ImgRef := CGImageSourceCreateImageAtIndex(imgSourceRef, 0, nil);
        if ImgRef <> nil then
        try
          Result := ImageToSurface(ImgRef, Bitmap, MaxSizeLimit);
        finally
          CGImageRelease(ImgRef);
        end;
      finally
        CFRelease(ImgSourceRef);
      end;
    finally
      CGDataProviderRelease(Provider);
    end;
  finally
    CopyStream.Free;
  end;
end;

function streamPutBytesCallback(info: Pointer; buffer: {const} Pointer; count: Longword ): Longword; cdecl;
begin
  Result := TStream(info).Write(buffer^, count);
end;

function TBitmapCodecQuartz.SaveToStream(const AStream: TStream; const Bitmap: TBitmapSurface; const Extension: string;
  const SaveParams: PBitmapCodecSaveParams = nil): Boolean;
var
  Consumer: CGDataConsumerRef;
  ImgDestRef: CGImageDestinationRef;
  ImgRef: CGImageRef;
  CtxRef: CGContextRef;
  EncoderType: CFStringRef;
  Callback: CGDataConsumerCallbacks;
  Keys: array [0..10] of pointer;
  Value: array [0..10] of pointer;
  FloatVal: Single;
  Dict: CFDictionaryRef;
begin
  Result := False;
  Callback.putBytes := @streamPutBytesCallback;
  Callback.releaseConsumer := nil;
  Consumer := CGDataConsumerCreate(AStream, @Callback);
  try
    EncoderType := ExtensionToEncoder(Extension);
    if EncoderType = nil then
      EncoderType := kUTTypePng;

    ImgDestRef := CGImageDestinationCreateWithDataConsumer(consumer, encoderType, 1, nil);
    if ImgDestRef <> nil then
    try
      CtxRef := CGBitmapContextCreate(Bitmap.Bits, Bitmap.Width, Bitmap.Height, 8,
         Bitmap.Width * 4, ColorSpace, kCGImageAlphaPremultipliedLast);
      if CtxRef <> nil then
      try
        ImgRef := CGBitmapContextCreateImage(CtxRef);
        if imgRef <> nil then
        try
          if SaveParams <> nil then
          begin
            FloatVal := SaveParams.Quality / 100;
            Keys[0] := CFSTR('kCGImageDestinationLossyCompressionQuality');
            Value[0] := CFNumberCreate(nil, kCFNumberFloat32Type, @FloatVal);
            Dict := CFDictionaryCreate(nil, @Keys[0], @Value[0], 1, nil, nil);
            CGImageDestinationSetProperties(ImgDestRef, Dict);
          end
          else
            Dict := nil;
          CGImageDestinationAddImage(ImgDestRef, ImgRef, Dict);
          CGImageDestinationFinalize(ImgDestRef);
          Result := True;
          if Dict <> nil then
            CFRelease(Dict);
        finally
          CGImageRelease(ImgRef);
        end;
      finally
        CGContextRelease(CtxRef);
      end;
    finally
      CFRelease(ImgDestRef);
    end;
  finally
    CGDataConsumerRelease(consumer);
  end;
end;

function TBitmapCodecQuartz.LoadFromFile(const AFileName: string;
  const Bitmap: TBitmapSurface; const MaxSizeLimit: Cardinal): Boolean;
var
  Path: CFStringRef;
  Url: CFURLRef;
  ImgSourceRef: CGImageSourceRef;
  ImgRef: CGImageRef;
begin
  Result := False;
  Path := CFStringCreateWithCString(nil, MarshaledAString(UTF8Encode(AFileName)), kCFStringEncodingUTF8);
  try
    Url := CFURLCreateWithFileSystemPath(nil, Path, kCFURLPOSIXPathStyle, False);
    try
      ImgSourceRef := CGImageSourceCreateWithURL(Url, nil);
      if ImgSourceRef <> nil then
      try
        ImgRef := CGImageSourceCreateImageAtIndex(ImgSourceRef, 0, nil);
        if ImgRef <> nil then
        try
          Result := ImageToSurface(ImgRef, Bitmap, MaxSizeLimit);
        finally
          CGImageRelease(ImgRef);
        end;
      finally
        CFRelease(imgSourceRef);
      end;
    finally
      CFRelease(Url);
    end;
  finally
    CFRelease(Path);
  end;
end;

function TBitmapCodecQuartz.LoadThumbnailFromFile(const AFileName: string;
  const AFitWidth, AFitHeight: Single; const UseEmbedded: Boolean; const Bitmap: TBitmapSurface): Boolean;
var
  Path: CFStringRef;
  Url: CFURLRef;
  ImgSourceRef: CGImageSourceRef;
  ImgRef: CGImageRef;
  CtxRef: CGContextRef;
  Dict: CFDictionaryRef;
  Keys: array [0..10] of Pointer;
  Value: array [0..10] of Pointer;
  IntValue: Cardinal;
  R: TRectF;
begin
  Result := False;
  Path := CFStringCreateWithCString(nil, MarshaledAString(UTF8Encode(AFileName)), kCFStringEncodingUTF8);
  try
    Url := CFURLCreateWithFileSystemPath(nil, Path, kCFURLPOSIXPathStyle, False);
    try
      ImgSourceRef := CGImageSourceCreateWithURL(Url, nil);
      if ImgSourceRef <> nil then
      try
        if UseEmbedded then
        begin
          Keys[0] := CFSTR('kCGImageSourceCreateThumbnailFromImageAlways');
          Value[0] := kCFBooleanFalse;
          Keys[1] := CFSTR('kCGImageSourceCreateThumbnailFromImageIfAbsent');
          Value[1] := kCFBooleanFalse;
          Keys[2] := CFSTR('kCGImageSourceThumbnailMaxPixelSize');
          if AFitWidth > AFitHeight then
            IntValue := Trunc(AFitWidth)
          else
            IntValue := Trunc(AFitHeight);
          Value[2] := CFNumberCreate(nil, kCFNumberSInt32Type, @intValue);
          Dict := CFDictionaryCreate(nil, @Keys[0], @Value[0], 3, nil, nil);
          try
            ImgRef := CGImageSourceCreateThumbnailAtIndex(ImgSourceRef, 0, Dict);
          finally
            CFRelease(Dict);
          end;
        end
        else
          ImgRef := nil;

        try
          if ImgRef <> nil then
            Result := ImageToSurface(ImgRef, Bitmap, MaxInt)
          else
          begin
            // Create thumbnail manually
            ImgRef := CGImageSourceCreateImageAtIndex(ImgSourceRef, 0, nil);
            if ImgRef <> nil then
            begin
              R := TRectF.Create(0, 0, CGImageGetWidth(ImgRef), CGImageGetHeight(ImgRef));
              R.Fit(TRectF.Create(0, 0, AFitWidth, AFitHeight));

              Bitmap.SetSize(Trunc(R.Width), Trunc(R.Height), TPixelFormat.RGBA);

              CtxRef := CGBitmapContextCreate(Bitmap.Bits, Bitmap.Width, Bitmap.Height, 8,
                Bitmap.Width * 4, ColorSpace, kCGImageAlphaPremultipliedLast);
              if CtxRef <> nil then
              try
                CGContextDrawImage(CtxRef, CGRectFromRect(TRectF.Create(0, 0, Bitmap.Width, Bitmap.Height)), ImgRef);

                Result := True;
              finally
                CGContextRelease(CtxRef);
              end;
            end;
          end;
        finally
          if ImgRef <> nil then
            CGImageRelease(ImgRef);
        end;
      finally
        CFRelease(ImgSourceRef);
      end;
    finally
      CFRelease(Url);
    end;
  finally
    CFRelease(Path);
  end;
end;

function TBitmapCodecQuartz.ExtensionToEncoder(const AExt: string): CFStringRef;
begin
  if SameText(AExt, SJPGImageExtension) or SameText(AExt, SJPEGImageExtension) then
    Exit(kUTTypeJpeg);
  if SameText(AExt, SJP2ImageExtension) then
    Exit(kUTTypeJPEG2000);
  if SameText(AExt, SBMPImageExtension) then
    Exit(kUTTypeBmp);
  if SameText(AExt, SPNGImageExtension) then
    Exit(kUTTypePng);
  if SameText(AExt, STIFImageExtension) or SameText(AExt, STIFFImageExtension) then
    Exit(kUTTypeTiff);
  if SameText(AExt, SGIFImageExtension) then
    Exit(kUTTypeGif);
  Result := nil;
end;

function TBitmapCodecQuartz.SaveToFile(const AFileName: string;
  const Bitmap: TBitmapSurface; const SaveParams: PBitmapCodecSaveParams = nil): Boolean;
var
  Path: CFStringRef;
  Url: CFURLRef;
  ImgDestRef: CGImageDestinationRef;
  ImgRef: CGImageRef;
  CtxRef: CGContextRef;
  EncoderType: CFStringRef;
  Keys: array [0..10] of pointer;
  Value: array [0..10] of pointer;
  FloatVal: Single;
  Dict: CFDictionaryRef;
begin
  Result := False;
  Path := CFStringCreateWithCString(nil, MarshaledAString(UTF8Encode(AFileName)), kCFStringEncodingUTF8);
  try
    Url := CFURLCreateWithFileSystemPath(nil, Path, kCFURLPOSIXPathStyle, False);
    try
      EncoderType := ExtensionToEncoder(ExtractFileExt(AFileName));
      if EncoderType <> nil then
      begin
        ImgDestRef := CGImageDestinationCreateWithURL(url, encoderType, 1, nil);
        if ImgDestRef <> nil then
        try
          CtxRef := CGBitmapContextCreate(Bitmap.Bits, Bitmap.Width, Bitmap.Height, 8,
             Bitmap.Width * 4, ColorSpace, kCGImageAlphaPremultipliedLast);
          if CtxRef <> nil then
          try
            ImgRef := CGBitmapContextCreateImage(ctxRef);
            if imgRef <> nil then
            try
              if SaveParams <> nil then
              begin
                FloatVal := SaveParams.Quality / 100;
                Keys[0] := CFSTR('kCGImageDestinationLossyCompressionQuality');
                Value[0] := CFNumberCreate(nil, kCFNumberFloat32Type, @FloatVal);
                Dict := CFDictionaryCreate(nil, @Keys[0], @Value[0], 1, nil, nil);
                CGImageDestinationSetProperties(ImgDestRef, Dict);
              end
              else
                Dict := nil;

              CGImageDestinationAddImage(ImgDestRef, ImgRef, Dict);
              CGImageDestinationFinalize(ImgDestRef);
              Result := True;

              if Dict <> nil then
                CFRelease(Dict);
            finally
              CGImageRelease(imgRef);
            end;
          finally
            CGContextRelease(ctxRef);
          end;
        finally
          CFRelease(ImgDestRef);
        end;
      end;
    finally
      CFRelease(Url);
    end;
  finally
    CFRelease(Path);
  end;
end;

{ TQuartzBitmap }

function TQuartzBitmap.GetImage: CGImageRef;
begin
  if FImage = nil then
    FImage := CGBitmapContextCreateImage(FContext);
  Result := FImage;
end;

{ TCanvasQuartz }

procedure CGContextDrawTiledImage(const ContextRef: CGContextRef; const DestRect: TRectF; const Image: CGImageRef;
  const Bitmap: TBitmap);
var
  I, J: Integer;
  LRect: CGRect;
begin
  if not Bitmap.IsEmpty then
    for I := 0 to Trunc(DestRect.Width / Bitmap.Width)  do
      for J := 0 to Trunc(DestRect.Height / Bitmap.Height) do
      begin
        LRect := CGRectFromRect(TRectF.Create(DestRect.Left + (Bitmap.Width * I), DestRect.Top + (Bitmap.Height * J),
          DestRect.Left + (Bitmap.Width * (I + 1)), DestRect.Top + (Bitmap.Height * (J + 1))));
        LRect.origin.y := -DestRect.Top - (Bitmap.Height * (J + 1));
        CGContextDrawImage(ContextRef, LRect, Image);
      end;
end;

constructor TCanvasQuartz.CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
  const AQuality: TCanvasQuality);
begin
  inherited;
  FFontScale := 1;
end;

constructor TCanvasQuartz.CreateFromBitmap(const ABitmap: TBitmap; const AQuality: TCanvasQuality);
begin
  inherited;
  FFontScale := 1;
end;

constructor TCanvasQuartz.CreateFromPrinter(const APrinter: TAbstractPrinter);
begin
  inherited;
  FFontScale := 1;
end;

function TCanvasQuartz.CreateSaveState: TCanvasSaveState;
begin
  Result := TQuartzCanvasSaveState.Create;
end;

procedure TCanvasQuartz.Clear(const Color: TAlphaColor);
var
  LColor: TAlphaColorF;
begin
  if Context <> nil then
  begin
    CGContextClearRect(Context, CGRectFromRect(TRectF.Create(0, 0, FWidth, FHeight)));
    LColor := TAlphaColorF.Create(Color);
    CGContextSetRGBFillColor(Context, LColor.R, LColor.G, LColor.B, LColor.A);
    CGContextFillRect(Context, CGRectFromRect(TRectF.Create(0, 0, FWidth, FHeight)));
  end;
end;

procedure TCanvasQuartz.ClearRect(const ARect: TRectF; const AColor: TAlphaColor);
var
  LColor: TAlphaColorF;
begin
  if Context <> nil then
  begin
    CGContextClearRect(Context, CGRectFromRect(ARect));
    LColor := TAlphaColorF.Create(AColor);
    CGContextSetRGBFillColor(Context, LColor.R, LColor.G, LColor.B, LColor.A);
    CGContextFillRect(Context, CGRectFromRect(ARect));
  end;
end;

function TCanvasQuartz.DoBeginScene(const AClipRects: PClipRects = nil; AContextHandle: THandle = 0): Boolean;
begin
  if FContext = nil then
  begin
    if AContextHandle <> 0 then
      FContext := CGContextRef(AContextHandle)
    else if Bitmap <> nil then
    begin
      if Bitmap.Handle <> 0 then
      begin
        if TQuartzBitmap(Bitmap.Handle).FImage <> nil then
        begin
          CGImageRelease(TQuartzBitmap(Bitmap.Handle).FImage);
          TQuartzBitmap(Bitmap.Handle).FImage := nil;
        end;
        FContext := CGBitmapContextCreate(TQuartzBitmap(Bitmap.Handle).FData, Bitmap.Width, Bitmap.Height, 8,
          Bitmap.Width * 4, ColorSpace, kCGImageAlphaPremultipliedLast)
      end;
    end
    else if FPrinter is TPrinterMac then
      PMSessionGetCGGraphicsContext(TPrinterMac(FPrinter).PrintInfo.PMPrintSession, @FContext);
  end;

  FFontScale := 1;
  if FPrinter <> nil then
  begin
    if TPrinterMac(FPrinter).ActivePrinter.ActiveDPIIndex < 0 then
      TPrinterMac(FPrinter).ActivePrinter.ActiveDPIIndex := 0;
    if TPrinterMac(FPrinter).ActivePrinter.ActiveDPIIndex >= 0 then
      FFontScale := TPrinterMac(FPrinter).ActivePrinter.ActiveDPI.X / 96;
  end;

  Result := inherited DoBeginScene(AClipRects) and (FContext <> nil);
  if Result and (AClipRects <> nil) then
    SetClipRects(AClipRects^);
end;

procedure TCanvasQuartz.DoEndScene;
begin
  inherited DoEndScene;
  if Bitmap <> nil then
    CGContextRelease(FContext);
  if (Parent <> nil) or (Bitmap <> nil) or (FPrinter <> nil) then
    FContext := nil;
end;

function TCanvasQuartz.GetCanvasScale: Single;
begin
  if Bitmap <> nil then
    Result := Bitmap.BitmapScale
  else
    Result := DefaultScale;
end;

function TCanvasQuartz.GetScaleFactor: TPointF;
var
  PrinterMac: TPrinterMac;
begin
  Result := TPointF.Create(Scale, -Scale);
  if FPrinter <> nil then
  begin
    PrinterMac := TPrinterMac(FPrinter);
    // Larger coordinates factor means more coordinates, which in turn means that figures are smaller since we have
    // a bigger canvas.
    Result.X := Result.X / PrinterMac.CoordFactor.X;
    Result.Y := Result.Y / PrinterMac.CoordFactor.Y;
  end;
end;

function TCanvasQuartz.CalcOrigin: TPointF;
var
  Padding: TRectF;
begin
  if FPrinter <> nil then
  begin
    Padding := TPrinterMac(FPrinter).PaperMargins;
    Result.X := Padding.Left;
    Result.Y := FHeight + Padding.Bottom;
  end
  else
  begin
    Result.X := 0;
    Result.Y := FHeight;
  end;
end;

procedure TCanvasQuartz.DoSetMatrix(const M: TMatrix);
var
  LMatrix: TMatrix;
  Transform: CGAffineTransform;
begin
  if Context <> nil then
  begin
    Transform := CGContextGetCTM(Context);
    LMatrix := TMatrix.Identity;
    LMatrix.m11 := Transform.a;
    LMatrix.m12 := Transform.b;
    LMatrix.m21 := Transform.c;
    LMatrix.m22 := Transform.d;
    LMatrix.m31 := Transform.tx;
    LMatrix.m32 := Transform.ty;
    LMatrix := LMatrix.Inverse;

    CGContextConcatCTM(Context, CGAffineTransformMake(LMatrix.m11, LMatrix.m12, LMatrix.m21, LMatrix.m22, LMatrix.m31,
      LMatrix.m32));
    AdaptCoordinateSystem;
    CGContextConcatCTM(Context, CGAffineTransformMake(M.m11, M.m12, M.m21, M.m22, M.m31, M.m32));
  end;
end;

procedure TCanvasQuartz.SetClipRects(const ARects: array of TRectF);
var
  I: Integer;
  LRects: array of CGRect;
begin
  if Context <> nil then
  begin
    SetLength(LRects, Length(ARects));
    for I := 0 to High(ARects) do
      LRects[I] := CGRectFromRect(ARects[I]);
    CGContextClipToRects(Context, @LRects[0], Length(LRects));
  end;
end;

procedure TCanvasQuartz.IntersectClipRect(const ARect: TRectF);
begin
  if Context <> nil then
    CGContextClipToRect(Context, CGRectFromRect(ARect));
end;

procedure TCanvasQuartz.ExcludeClipRect(const ARect: TRectF);
var
  LRect: array[0..3] of CGRect;
begin
  if Context <> nil then
  begin
    LRect[0] := CGRectFromRect(TRectF.Create(-FWidth, -FWidth, ARect.Left, FHeight));
    LRect[1] := CGRectFromRect(TRectF.Create(ARect.Right, -FHeight, FWidth, FHeight));
    LRect[2] := CGRectFromRect(TRectF.Create(ARect.Left, -FHeight, ARect.Right, ARect.Top));
    LRect[3] := CGRectFromRect(TRectF.Create(ARect.Left, ARect.Bottom, ARect.Right, FHeight));
    CGContextClipToRects(Context, @LRect[0], 4);
  end;
end;

procedure TCanvasQuartz.AdaptCoordinateSystem;
var
  PrinterMac: TPrinterMac;
  Origin: TPointF;
begin
  // calculate the scaling factor for the printer canvas
  if FPrinter <> nil then
  begin
    PrinterMac := TPrinterMac(FPrinter);
    SetSize(PrinterMac.PageWidth, PrinterMac.PageHeight);
  end;

  Origin := CalcOrigin;
  CGContextTranslateCTM(Context, Origin.X, Origin.Y);
  CGContextScaleCTM(Context, ScaleFactor.X, ScaleFactor.Y);
end;

var
  GradientEvaluateAlpha: Single = 0;

procedure GradientEvaluateCallback(info: Pointer; inData: PSingle; outData: PAlphaColorF); cdecl;
begin
  if info <> nil then
    outData^ := TAlphaColorF.Create(MakeColor(TGradient(info).InterpolateColor(inData^), GradientEvaluateAlpha));
end;

procedure TCanvasQuartz.ApplyFill(const ABrush: TBrush; const ARect: TRectF; const AOpacity: Single);
const
  DefaultInputRange: array[0..1] of Single = (0, 1);
var
  LColor: TAlphaColorF;
  RCenter: TPointF;
  BrushBitmap: TBitmap;
begin
  if Context = nil then
    Exit;

  if (ABrush.Kind = TBrushKind.Resource) and (ABrush.Resource <> nil) and (ABrush.Resource.Brush <> nil) then
    ABrush.Assign(ABrush.Resource.Brush);

  case ABrush.Kind of
    TBrushKind.Solid:
      begin
        LColor := TAlphaColorF.Create(MakeColor(ABrush.Color, AOpacity));
        CGContextSetRGBFillColor(Context, LColor.R, LColor.G, LColor.B, LColor.A);
      end;
    TBrushKind.Gradient:
      begin
        FCallback.version := 0;
        FCallback.evaluate := @GradientEvaluateCallback;
        FCallback.releaseInfo:= nil;
        GradientEvaluateAlpha := AOpacity;
        FFunc := CGFunctionCreate(ABrush.Gradient, 1, @DefaultInputRange, 4, nil, @FCallback);
        if ABrush.Gradient.Style = TGradientStyle.Linear then
        begin
          FShading := CGShadingCreateAxial(ColorSpace, CGPoint(TPointF.Create(ARect.Left +
            ABrush.Gradient.StartPosition.X * ARect.Width, ARect.Top + ABrush.Gradient.StartPosition.Y *
            ARect.Height)), CGPoint(TPointF.Create(ARect.Left + ABrush.Gradient.StopPosition.X * ARect.Width,
            ARect.Top + ABrush.Gradient.StopPosition.Y * ARect.Height)), FFunc, 1, 1);
        end
        else
        begin
          RCenter := TPointF.Create(ABrush.Gradient.RadialTransform.RotationCenter.X * ARect.Width,
            ABrush.Gradient.RadialTransform.RotationCenter.Y * ARect.Height) + ARect.TopLeft;
          FShading := CGShadingCreateRadial(ColorSpace, CGPoint(RCenter), ARect.Width / 2, CGPoint(RCenter), 0, FFunc,
            1, 1);
        end;
      end;
    TBrushKind.Bitmap:
      begin
        BrushBitmap := ABrush.Bitmap.Bitmap;
        if not BrushBitmap.IsEmpty and BrushBitmap.HandleAllocated then
        begin
          CGContextSetAlpha(Context, AOpacity);
          FBitmapRef := TQuartzBitmap(BrushBitmap.Handle).GetImage;
        end;
      end;
  else
    CGContextSetRGBFillColor(Context, 0, 0, 0, 0);
  end;
end;

procedure TCanvasQuartz.RemoveFill(const ABrush: TBrush; const ARect: TRectF; const AOpacity: Single);
begin
  if Context <> nil then
  begin
    case ABrush.Kind of
      TBrushKind.Gradient:
        begin
          CGShadingRelease(FShading);
          CGFunctionRelease(FFunc);
        end;
      TBrushKind.Bitmap:
        CGContextSetAlpha(Context, 1);
    end;
  end;
  FShading := nil;
  FBitmapRef := nil;
end;

procedure TCanvasQuartz.ApplyStroke(const AStroke: TStrokeBrush; const ARect: TRectF; const AOpacity: Single);
var
  Dash: TDashArray;
  I: Integer;
  LColor: TAlphaColorF;
begin
  if Context = nil then
    Exit;

  if (AStroke.Kind = TBrushKind.Resource) and (AStroke.Resource <> nil) and (AStroke.Resource.Brush <> nil) then
    AStroke.Assign(AStroke.Resource.Brush);

  case AStroke.Kind of
    TBrushKind.Solid,
    TBrushKind.Bitmap:
      begin
        LColor := TAlphaColorF.Create(MakeColor(AStroke.Color, AOpacity));
        CGContextSetRGBStrokeColor(Context, LColor.R, LColor.G, LColor.B, LColor.A);
      end;
    TBrushKind.Gradient:
      begin
        LColor := TAlphaColorF.Create(0);
        for I := 0 to AStroke.Gradient.Points.Count - 1 do
          LColor := LColor + TAlphaColorF.Create(MakeColor(AStroke.Gradient.Points[I].Color, AOpacity));
        LColor := LColor.Clamp;
        CGContextSetRGBStrokeColor(Context, LColor.R, LColor.G, LColor.B, LColor.A);
      end;
  else
    CGContextSetRGBStrokeColor(Context, 0, 0, 0, 0);
  end;

  case AStroke.Cap of
    TStrokeCap.Flat:
      CGContextSetLineCap(Context, kCGLineCapButt);
    TStrokeCap.Round:
      CGContextSetLineCap(Context, kCGLineCapRound);
  end;

  if Length(AStroke.DashArray) > 0 then
  begin
    // select the proper dash array for the printer
    if FPrinter <> nil then
      if AStroke.Dash <> TStrokeDash.Custom then
        Dash := TStrokeBrush.StdDash[TStrokeBrush.TDashDevice.Printer, AStroke.Dash].DashArray
      else
        Dash := AStroke.DashArray
    else // adjust the line dashes for the screen
    begin
      SetLength(Dash, Length(AStroke.DashArray));
      for I := 0 to High(AStroke.DashArray) do
      begin
        Dash[I] := AStroke.DashArray[I] * AStroke.Thickness;
        if AStroke.Cap = TStrokeCap.Round then
        begin
          if Odd(I) then
            Dash[I] := (AStroke.DashArray[I] + 1) * AStroke.Thickness
          else
            Dash[I] := (AStroke.DashArray[I] - 1) * AStroke.Thickness;
        end;
      end;
    end;
    CGContextSetLineDash(Context, AStroke.DashOffset, @Dash[0], Length(AStroke.DashArray));
  end
  else
    CGContextSetLineDash(Context, 0, nil, 0);

  case AStroke.Join of
    TStrokeJoin.Miter:
      CGContextSetLineJoin(Context, kCGLineJoinMiter);

    TStrokeJoin.Round:
      CGContextSetLineJoin(Context, kCGLineJoinRound);

    TStrokeJoin.Bevel:
      CGContextSetLineJoin(Context, kCGLineJoinBevel);
  end;

  CGContextSetLineWidth(Context, AStroke.Thickness);
end;

procedure TCanvasQuartz.DoDrawLine(const APt1, APt2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  if Context <> nil then
  begin
    ApplyStroke(ABrush, TRectF.Create(APt1.X, APt1.Y, APt2.X, APt2.Y), AOpacity);
    CGContextBeginPath(Context);
    CGContextMoveToPoint(Context, APt1.X, APt1.Y);
    CGContextAddLineToPoint(Context, APt2.X, APt2.Y);
    CGContextStrokePath(Context);
  end;
end;

procedure TCanvasQuartz.DoDrawRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  if Context <> nil then
  begin
    ApplyStroke(ABrush, ARect, AOpacity);
    CGContextStrokeRect(Context, CGRectFromRect(ARect));
  end;
end;

procedure TCanvasQuartz.DoFillRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush);
var
  BrushBitmap: TBitmap;
begin
  if Context = nil then
    Exit;

  CGContextSaveGState(Context);
  ApplyFill(ABrush, ARect, AOpacity);
  CGContextBeginPath(Context);
  CGContextAddRect(Context, CGRectFromRect(ARect));
  CGContextClosePath(Context);

  if FBitmapRef <> nil then
  begin
    BrushBitmap := ABrush.Bitmap.Bitmap;
    CGContextClip(Context);
    case ABrush.Bitmap.WrapMode of
      TWrapMode.Tile:
        begin
          CGContextScaleCTM(Context, 1, -1);
          CGContextDrawTiledImage(Context, ARect, FBitmapRef, BrushBitmap);
        end;
      TWrapMode.TileOriginal:
        begin
          CGContextScaleCTM(Context, 1, -1);
          CGContextDrawImage(Context, CGRectFromRect(RectF(ARect.Left, ARect.Top, ARect.Left + BrushBitmap.Width,
            -ARect.Top - BrushBitmap.Height)), FBitmapRef);
        end;
      TWrapMode.TileStretch:
        begin
          CGContextScaleCTM(Context, 1, -1);
          CGContextDrawImage(Context, CGRectFromRect(RectF(ARect.Left, ARect.Top, ARect.Right, -ARect.Bottom)),
            FBitmapRef);
        end;
    end;
  end
  else
  if FShading <> nil then
  begin
    CGContextClip(Context);
    CGContextDrawShading(Context, FShading)
  end
  else
    CGContextFillPath(Context);

  RemoveFill(ABrush, ARect, AOpacity);
  CGContextRestoreGState(Context);
end;

procedure TCanvasQuartz.DoDrawEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  if Context <> nil then
  begin
    ApplyStroke(ABrush, ARect, AOpacity);
    CGContextStrokeEllipseInRect(Context, CGRectFromRect(ARect));
  end;
end;

procedure TCanvasQuartz.DoFillEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush);
var
  BrushBitmap: TBitmap;
begin
  if Context = nil then
    Exit;

  CGContextSaveGState(Context);
  ApplyFill(ABrush, ARect, AOpacity);
  CGContextBeginPath(Context);
  CGContextAddEllipseInRect(Context, CGRectFromRect(ARect));
  CGContextClosePath(Context);

  if (FBitmapRef <> nil) then
  begin
    BrushBitmap := ABrush.Bitmap.Bitmap;
    CGContextClip(Context);
    case ABrush.Bitmap.WrapMode of
      TWrapMode.Tile:
        begin
          CGContextScaleCTM(Context, 1, -1);
          CGContextDrawTiledImage(Context, ARect, FBitmapRef, BrushBitmap);
        end;
      TWrapMode.TileOriginal:
        begin
          CGContextScaleCTM(Context, 1, -1);
          CGContextDrawImage(Context, CGRectFromRect(TRectF.Create(ARect.Left, ARect.Top, ARect.Left +
            BrushBitmap.Width, -ARect.Top - BrushBitmap.Height)), FBitmapRef);
        end;
      TWrapMode.TileStretch:
        begin
          CGContextScaleCTM(Context, 1, -1);
          CGContextDrawImage(Context, CGRectFromRect(TRectF.Create(ARect.Left, ARect.Top, ARect.Right, -ARect.Bottom)),
            FBitmapRef);
        end;
    end;
  end
  else
  if FShading <> nil then
  begin
    CGContextClip(Context);
    CGContextDrawShading(Context, FShading)
  end
  else
    CGContextFillPath(Context);

  RemoveFill(ABrush, ARect, AOpacity);
  CGContextRestoreGState(Context);
end;

procedure TCanvasQuartz.DoDrawPath(const APath: TPathData; const AOpacity: Single; const ABrush: TStrokeBrush);
var
  I: Integer;
  CurvePoint1, CurvePoint2: TPointF;
begin
  if Context = nil then
    Exit;

  ApplyStroke(ABrush, APath.GetBounds, AOpacity);

  CGContextSaveGState(Context);
  CGContextBeginPath(Context);

  I := 0;
  while I < APath.Count do
  begin
    case APath[I].Kind of
      TPathPointKind.MoveTo:
        CGContextMoveToPoint(Context, APath[I].Point.X, APath[I].Point.Y);
      TPathPointKind.LineTo:
        CGContextAddLineToPoint(Context, APath[I].Point.X, APath[I].Point.Y);
      TPathPointKind.CurveTo:
        begin
          CurvePoint1 := APath[I].Point;
          Inc(I);
          CurvePoint2 := APath[I].Point;
          Inc(I);
          CGContextAddCurveToPoint(Context, CurvePoint1.X, CurvePoint1.Y, CurvePoint2.X, CurvePoint2.Y,
            APath[I].Point.X, APath[I].Point.Y);
        end;
      TPathPointKind.Close:
        CGContextClosePath(Context);
    end;
    Inc(I);
  end;

  CGContextStrokePath(Context);
  CGContextRestoreGState(Context);
end;

procedure TCanvasQuartz.DoFillPath(const APath: TPathData; const AOpacity: Single; const ABrush: TBrush);
var
  I: Integer;
  LRect: TRectF;
  CurvePoint1, CurvePoint2: TPointF;
  BrushBitmap: TBitmap;
begin
  if Context = nil then
    Exit;

  LRect := APath.GetBounds;
  ApplyFill(ABrush, LRect, AOpacity);

  CGContextSaveGState(Context);
  CGContextBeginPath(Context);

  I := 0;
  while I < APath.Count do
  begin
    case APath[I].Kind of
      TPathPointKind.MoveTo:
        CGContextMoveToPoint(Context, APath[I].Point.X, APath[I].Point.Y);
      TPathPointKind.LineTo:
        CGContextAddLineToPoint(Context, APath[I].Point.X, APath[I].Point.Y);
      TPathPointKind.CurveTo:
        begin
          CurvePoint1 := APath[I].Point;
          Inc(I);
          CurvePoint2 := APath[I].Point;
          Inc(I);
          CGContextAddCurveToPoint(Context, CurvePoint1.X, CurvePoint1.Y, CurvePoint2.X, CurvePoint2.Y,
            APath[I].Point.X, APath[I].Point.Y);
        end;
      TPathPointKind.Close:
        CGContextClosePath(Context);
    end;
    Inc(I);
  end;

  if FBitmapRef <> nil then
  begin
    BrushBitmap := ABrush.Bitmap.Bitmap;
    CGContextClip(Context);
    case ABrush.Bitmap.WrapMode of
      TWrapMode.Tile:
        begin
          CGContextScaleCTM(Context, 1, -1);
          CGContextDrawTiledImage(Context, LRect, FBitmapRef, BrushBitmap);
        end;
      TWrapMode.TileOriginal:
        begin
          CGContextScaleCTM(Context, 1, -1);
          CGContextDrawImage(Context, CGRectFromRect(TRectF.Create(LRect.Left, LRect.Top, LRect.Left +
            BrushBitmap.Width, -LRect.Top - BrushBitmap.Height)), FBitmapRef);
        end;
      TWrapMode.TileStretch:
        begin
          CGContextScaleCTM(Context, 1, -1);
          CGContextDrawImage(Context, CGRectFromRect(TRectF.Create(LRect.Left, LRect.Top, LRect.Right, -LRect.Bottom)),
            FBitmapRef);
        end;
    end;
  end
  else
  if FShading <> nil then
  begin
    CGContextClip(Context);
    CGContextDrawShading(Context, FShading)
  end
  else
    CGContextEOFillPath(Context);

  RemoveFill(ABrush, LRect, AOpacity);
  CGContextRestoreGState(Context);
end;

function TCanvasQuartz.PtInPath(const APoint: TPointF; const APath: TPathData): Boolean;
var
  I: Integer;
  LRect: TRectF;
  CurvePoint1, CurvePoint2: TPointF;
  ContextRef: CGContextRef;
begin
  if APath.IsEmpty then
     Exit(False);
  LRect := APath.GetBounds;
  ContextRef := CGBitmapContextCreate(nil, 1, 1, 8, 1 * 4, ColorSpace, kCGImageAlphaPremultipliedLast);
  try
    CGContextTranslateCTM(ContextRef, LRect.Left, LRect.Top);
    CGContextBeginPath(ContextRef);
    I := 0;
    while I < APath.Count do
    begin
      case APath[I].Kind of
        TPathPointKind.MoveTo:
          CGContextMoveToPoint(ContextRef, APath[I].Point.X, APath[I].Point.Y);
        TPathPointKind.LineTo:
          CGContextAddLineToPoint(ContextRef, APath[I].Point.X, APath[I].Point.Y);
        TPathPointKind.CurveTo:
          begin
            CurvePoint1 := APath[I].Point;
            Inc(I);
            CurvePoint2 := APath[I].Point;
            Inc(I);
            CGContextAddCurveToPoint(ContextRef, CurvePoint1.X, CurvePoint1.Y, CurvePoint2.X, CurvePoint2.Y,
              APath[I].Point.X, APath[I].Point.Y);
          end;
        TPathPointKind.Close:
          CGContextClosePath(ContextRef);
      end;
      Inc(I);
    end;
    Result := CGContextPathContainsPoint(ContextRef, CGPoint(APoint), kCGPathFillStroke) > 0;
  finally
    CGContextRelease(ContextRef);
  end;
end;

procedure TCanvasQuartz.DoDrawBitmap(const ABitmap: TBitmap; const SrcRect, DstRect: TRectF; const AOpacity: Single;
  const HighSpeed: Boolean);
var
  NewDestRect, SubRect: CGRect;
  ImageRef, SubImageRef: CGImageRef;
begin
  if (Context = nil) or (ABitmap = nil) then
    Exit;

  NewDestRect := CGRectFromRect(DstRect);

  if ABitmap.HandleAllocated then
  begin
    if (SrcRect.Left = 0) and (SrcRect.Top = 0) and (SrcRect.Right = ABitmap.Width) and
      (SrcRect.Bottom = ABitmap.Height) then
    begin
      ImageRef := TQuartzBitmap(ABitmap.Handle).GetImage;
      CGContextSaveGState(Context);
      CGContextSetAlpha(Context, AOpacity);

      if HighSpeed then
        CGContextSetInterpolationQuality(Context, kCGInterpolationNone)
      else
        CGContextSetInterpolationQuality(Context, kCGInterpolationDefault);

      NewDestRect.origin.y := -DstRect.Bottom;
      CGContextScaleCTM(Context, 1, -1);

      CGContextSetAllowsAntialiasing(Context, Ord(False));
      CGContextDrawImage(Context, NewDestRect, ImageRef);
      CGContextSetAllowsAntialiasing(Context, Ord(True));

      CGContextRestoreGState(Context);
    end
    else
    begin
      SubRect := CGRectFromRect(SrcRect);

      ImageRef := TQuartzBitmap(ABitmap.Handle).GetImage;
      SubImageRef := CGImageCreateWithImageInRect(ImageRef, SubRect);
      if SubImageRef <> nil then
      begin
        CGContextSaveGState(Context);
        CGContextSetAlpha(Context, AOpacity);

        if HighSpeed then
          CGContextSetInterpolationQuality(Context, kCGInterpolationNone)
        else
          CGContextSetInterpolationQuality(Context, kCGInterpolationDefault);

        NewDestRect.origin.y := -DstRect.Bottom;
        CGContextScaleCTM(Context, 1, -1);

        CGContextDrawImage(Context, NewDestRect, SubImageRef);
        CGImageRelease(SubImageRef);

        CGContextRestoreGState(Context);
      end;
    end;
  end;
end;

class function TCanvasQuartz.DoInitializeBitmap(const Width, Height: Integer; const Scale: Single;
  var PixelFormat: TPixelFormat): THandle;
var
  LHandle: TQuartzBitmap;
begin
  LHandle := TQuartzBitmap.Create;
  GetMem(LHandle.FData, Width * Height * 4);
  FillChar(LHandle.FData^, Width * Height * 4, 0);
  LHandle.FContext := CGBitmapContextCreate(LHandle.FData, Width, Height, 8, Width * 4, ColorSpace,
    kCGImageAlphaPremultipliedLast);
  Result := THandle(LHandle);
  PixelFormat := TPixelFormat.RGBA;
end;

class procedure TCanvasQuartz.DoFinalizeBitmap(var Bitmap: THandle);
var
  LHandle: TQuartzBitmap;
begin
  LHandle := TQuartzBitmap(Bitmap);
  if LHandle.FImage <> nil then
    CGImageRelease(LHandle.FImage);
  LHandle.FImage := nil;
  if LHandle.FContext <> nil then
    CGContextRelease(LHandle.FContext);
  FreeMem(LHandle.FData);
  LHandle.FData := nil;
  LHandle.Free;
  Bitmap := THandle(nil);
end;

class function TCanvasQuartz.DoMapBitmap(const Bitmap: THandle; const Access: TMapAccess;
  var Data: TBitmapData): Boolean;
var
  LHandle: TQuartzBitmap;
begin
  LHandle := TQuartzBitmap(Bitmap);
  if (Access <> TMapAccess.Read) and (LHandle.FImage <> nil) then
  begin
    CGImageRelease(LHandle.FImage);
    LHandle.FImage := nil;
  end;
  Data.Data := LHandle.FData;
  Data.Pitch := CGBitmapContextGetWidth(TQuartzBitmap(Bitmap).FContext) * 4;
  Result := Data.Data <> nil;
end;

class procedure TCanvasQuartz.DoUnmapBitmap(const Bitmap: THandle; var Data: TBitmapData);
begin
end;

{ TQuartzCanvasSaveState }

procedure TQuartzCanvasSaveState.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TCanvasQuartz then
    CGContextSaveGState(TCanvasQuartz(Source).Context);
end;

procedure TQuartzCanvasSaveState.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TCanvasQuartz then
    CGContextRestoreGState(TCanvasQuartz(Dest).Context);
end;

{ TTextLayoutCT }

procedure PathApplierFunction(info: Pointer; const element: PCGPathElement); cdecl;
var
  P, P1, P2: PPointF;
begin
  P := PPointF(element^.points);
  case element.type_ of
    kCGPathElementMoveToPoint:
      TPathData(info).MoveTo(P^);
    kCGPathElementAddLineToPoint:
      TPathData(info).LineTo(P^);
    kCGPathElementAddQuadCurveToPoint:
      begin
        P1 := P;
        Inc(P1);
        TPathData(info).QuadCurveTo(P^, P1^);
      end;
    kCGPathElementAddCurveToPoint:
      begin
        P1 := P;
        Inc(P1);
        P2 := P1;
        Inc(P2);
        TPathData(info).CurveTo(P^, P1^, P2^);
      end;
    kCGPathElementCloseSubpath:
      TPathData(info).ClosePath;
  end;
end;

procedure TTextLayoutCT.ConvertToPath(const APath: TPathData);
var
  i, j, k: Integer;
  LFontRef: CTFontRef;
  Line: CTLineRef;
  Lines: CFArrayRef;
  NumLines: CFIndex;
  glyphPath: CGPathRef;
  Runs: CFArrayRef;
  Run: CTRunRef;
  RunGlyphCount: CFIndex;
  glyph: CGGlyph;
  position:  CGPoint;
  glyphMatrix: CGAffineTransform;
  M: TMatrix;
  TextR: TRectF;
  LinePath: TPathData;
  LinesHeight, LineHeight: Single;
  Ascent, Descent, Leading: CGFloat;
  FrameRect: CGRect;
begin
  if (FCTAttr = nil) or (APath = nil) then
    Exit;

  CreateCTFrame;
  // Creating font for paragraph
  LFontRef := GetCTFontRef(Font);
  Lines := CTFrameGetLines(FCTFrame);
  NumLines := CFArrayGetCount(Lines);
  FrameRect := CGPathGetBoundingBox(FCTPath);
  LinesHeight := 0;
  for i := 0 to NumLines - 1 do
  begin
    LinePath := TPathData.Create;
    Line := CTLineRef(CFArrayGetValueAtIndex(Lines, i));
    // Getting line dimensions to calculate each line vertical shift
    CTLineGetTypographicBounds(Line, @Ascent,  @Descent, @Leading);
    if i > 0 then
    begin
      CTFrameGetLineOrigins(FCTFrame, CFRangeMake(i, 1), @position);
      LineHeight := CGRectGetMaxY(FrameRect) - position.y + descent - LinesHeight;
      LinesHeight := CGRectGetMaxY(FrameRect) - position.y + descent;
      APath.Translate(0, LineHeight);
    end
    else
    begin
      CTFrameGetLineOrigins(FCTFrame, CFRangeMake(i, 1), @position);
      LinesHeight := CGRectGetMaxY(frameRect) - position.y + descent;
    end;
    Runs := CTLineGetGlyphRuns(Line);
    for j := 0 to CFArrayGetCount(Runs) - 1 do
    begin
      Run := CFArrayGetValueAtIndex(Runs, j);
      RunGlyphCount := CTRunGetGlyphCount(Run);
      for k := 0 to RunGlyphCount - 1 do
      begin
        CTRunGetGlyphs(Run, CFRangeMake(k, 1), @glyph);
        CTRunGetPositions(run, CFRangeMake(k, 1), @position);
                                                                                      
        glyphMatrix := CGAffineTransformTranslate(CGAffineTransformIdentity,
          position.x, position.y);
        glyphPath := CTFontCreatePathForGlyph(LFontRef, glyph, @glyphMatrix);
        if glyphPath <> nil then
        begin
          CGPathApply(glyphPath, LinePath, @PathApplierFunction);
    	  CFRelease(glyphPath);
        end;
      end;
    end;
    // Adding line path to all path
    TextR := LinePath.GetBounds;
    case HorizontalAlign of
      TTextAlign.Center:
        begin
          OffsetRect(TextR, -TextR.Left, 0);
          OffsetRect(TextR, (MaxSize.X - Padding.Left - Padding.Right - TextR.Width) / 2, 0);
          OffsetRect(TextR, TopLeft.X, 0);
        end;
      TTextAlign.Leading:
        begin
          OffsetRect(TextR, -TextR.Left, 0);
          OffsetRect(TextR, TopLeft.X, 0);
        end;
      TTextAlign.Trailing:
        begin
          OffsetRect(TextR, -TextR.Left, 0);
          OffsetRect(TextR, (MaxSize.X - Padding.Left - Padding.Right - TextR.Width), 0);
          OffsetRect(TextR, TopLeft.X, 0);
        end;
    end;
    //Only horizontal alignment
    LinePath.Translate(TextR.Left, 0);
    APath.AddPath(LinePath);
    FreeAndNil(LinePath);
  end;
  CFRelease(LFontRef);
  //
  TextR := APath.GetBounds;

  M := TMatrix.Identity;
  M.m22 := -1;
  M.m32 := TextR.Height;
  APath.ApplyMatrix(M);

  TextR := APath.GetBounds;

  case VerticalAlign of
    TTextAlign.Center:
      begin
        OffsetRect(TextR, 0, -TextR.Top);
        OffsetRect(TextR, 0, (MaxSize.Y - Padding.Top - Padding.Bottom - TextR.Height) / 2);
        OffsetRect(TextR, 0, TopLeft.Y);
      end;
    TTextAlign.Leading:
      begin
        OffsetRect(TextR, 0, -TextR.Top);
        OffsetRect(TextR, 0, TopLeft.Y);
      end;
    TTextAlign.Trailing:
      begin
        OffsetRect(TextR, 0, -TextR.Top);
        OffsetRect(TextR, 0, (MaxSize.Y - Padding.Top - Padding.Bottom - TextR.Height));
        OffsetRect(TextR, 0, TopLeft.Y);
      end;
  end;
  APath.Translate(0, TextR.Top);
end;

procedure AddColorAttribute(const AAttribute: CFMutableAttributedStringRef; const AColor: TAlphaColor;
  const AOpacity: Single; const AStart, ALength: Integer);
var
  LColor: TAlphaColorF;
  TextColor: CGColorRef;
begin
  LColor := TAlphaColorF.Create(MakeColor(AColor, AOpacity));
  TextColor := CGColorCreate(ColorSpace, @LColor);
  CFAttributedStringSetAttribute(AAttribute, CFRangeMake(AStart, ALength), kCTForegroundColorAttributeName, TextColor);
  CFRelease(TextColor);
end;

constructor TTextLayoutCT.Create(const ACanvas: TCanvas);
begin
  inherited;
  if ACanvas <> nil then
    FFontScale := TCanvasQuartz(ACanvas).FFontScale
  else
    FFontScale := 1;
end;

procedure TTextLayoutCT.CreateCT;

  procedure AddFontAttribute(AAttribute: CFMutableAttributedStringRef;
    AFont: TFont; const AStart, ALength: Integer);
  var
    LFontRef: CTFontRef;
    LUnderline: CFNumberRef;
    LValue: Cardinal;
  begin
    // Font
    LFontRef := GetCTFontRef(AFont);
    if LFontRef <> nil then
    try
      CFAttributedStringSetAttribute(AAttribute, CFRangeMake(AStart, ALength),
        kCTFontAttributeName, LFontRef);
    finally
      CFRelease(LFontRef);
    end;
    // Underline
    if TFontStyle.fsUnderline in AFont.Style then
    begin
      LValue := kCTUnderlineStyleSingle;
      LUnderline := CFNumberCreate(nil, kCFNumberSInt32Type, @LValue);
      try
      CFAttributedStringSetAttribute(AAttribute, CFRangeMake(AStart, ALength),
          kCTUnderlineStyleAttributeName, LUnderline);
      finally
        CFRelease(LUnderline);
      end;
    end;
  end;

var
  LStringRef: CFStringRef;
  LValue: Cardinal;
  Ligature: CFNumberRef;
  Alignment: Byte;
  Direction: Byte;
  Wrapping: Byte;
  Settings: array of CTParagraphStyleSetting;
  ParagraphStyle: CTParagraphStyleRef;
  Bounds: CGRect;
  i: Integer;
  LText: string;
begin
  if FCTFrame <> nil then
    CFRelease(FCTFrame);
  FCTFrame := nil;
  if FCTAttr <> nil then
    CFRelease(FCTAttr);
  FCTAttr := nil;
  if FCTPath <> nil then
    CFRelease(FCTPath);
  FCTPath := nil;
  if (LayoutCanvas <> nil) and not SameValue(FFontScale, TCanvasQuartz(LayoutCanvas).FFontScale, Epsilon) then
    FFontScale := TCanvasQuartz(LayoutCanvas).FFontScale;
  if SameValue(FFontScale, 0, Epsilon) then
    FFontScale := 1.0;


  FCTPath := CGPathCreateMutable();
  Bounds := CGRectMake(0, 0,
    MaxSize.X - Padding.Left - Padding.Right, $FFFF);
  CGPathAddRect(FCTPath, nil, Bounds);

  if Text.Length = 0 then
    LText := ' '
  else
    LText := Text;
  LStringRef := CFStringCreateWithCharacters(kCFAllocatorDefault, PChar(LText),
    LText.Length);
  try
    FCTAttr := CFAttributedStringCreateMutable(kCFAllocatorDefault, 0);
    CFAttributedStringReplaceString(FCTAttr, CFRangeMake(0, 0), LStringRef);

    CFAttributedStringBeginEditing(FCTAttr);
    try
      // Ligature
      LValue := 0;
      Ligature := CFNumberCreate(nil, kCFNumberSInt32Type, @LValue);
      try
        CFAttributedStringSetAttribute(FCTAttr, CFRangeMake(0, LText.Length),
          kCTLigatureAttributeName, Ligature);

        // Font
        AddFontAttribute(FCTAttr, Font, 0, LText.Length);

        // Align
        SetLength(Settings, Length(Settings) + 1);
        case HorizontalAlign of
          TTextAlign.Center:
            Alignment := kCTCenterTextAlignment;
          TTextAlign.Leading:
            Alignment := kCTLeftTextAlignment;
          TTextAlign.Trailing:
            Alignment := kCTRightTextAlignment;
        end;
        Settings[High(Settings)].spec := kCTParagraphStyleSpecifierAlignment;
        Settings[High(Settings)].valueSize := sizeof(Alignment);
        Settings[High(Settings)].value := @Alignment;

        // Word wrap and trimming
        SetLength(Settings, Length(Settings) + 1);
      if WordWrap then
        Wrapping := kCTLineBreakByWordWrapping
      else
        // https://quality.embarcadero.com/browse/RSP-16648
        case Trimming of
          TTextTrimming.None: Wrapping := kCTLineBreakByClipping; // Lines are simply not drawn past the edge of the frame.
          TTextTrimming.Character: Wrapping := kCTLineBreakByCharWrapping; // Wrapping occurs before the first character that doesn't fit.
          TTextTrimming.Word: Wrapping := kCTLineBreakByWordWrapping; // Wrapping occurs at word boundaries unless the word itself doesn't fit on a single line.
        end;
      Settings[High(Settings)].spec := kCTParagraphStyleSpecifierLineBreakMode;
      Settings[High(Settings)].valueSize := SizeOf(Wrapping);
        Settings[High(Settings)].value := @Wrapping;

        // Right to left text
        SetLength(Settings, Length(Settings) + 1);
        if RightToLeft then
          Direction := kCTWritingDirectionRightToLeft
        else
          Direction := kCTWritingDirectionLeftToRight;
        Settings[High(Settings)].spec := kCTParagraphStyleSpecifierBaseWritingDirection;
        Settings[High(Settings)].valueSize := SizeOf(Direction);
        Settings[High(Settings)].value := @Direction;

        ParagraphStyle := CTParagraphStyleCreate(@Settings[0], Length(Settings));
        try
          CFAttributedStringSetAttribute(FCTAttr, CFRangeMake(0, CFStringGetLength(LStringRef)), kCTParagraphStyleAttributeName, ParagraphStyle);
        finally
          CFRelease(ParagraphStyle);
        end;

        // Applying attributes
        for i := 0 to AttributesCount - 1 do
          if Attributes[i].Attribute.Font <> nil then
          // Checking for changes or we will get an SIGSERV error
          if (Attributes[i].Attribute.Font.Family <> Font.Family) or
             not SameValue(Attributes[i].Attribute.Font.Size, Font.Size, Epsilon) or
             (Attributes[i].Attribute.Font.Style <> Font.Style) then
            AddFontAttribute(FCTAttr, Attributes[i].Attribute.Font,
              Attributes[i].Range.Pos, Attributes[i].Range.Length);
      finally
        CFRelease(Ligature);
      end;
    finally
      CFAttributedStringEndEditing(FCTAttr);
    end;
  finally
    CFRelease(LStringRef);
  end;
end;

procedure TTextLayoutCT.CreateCTFrame;
var
  FrameSetter: CTFramesetterRef;
  i: Integer;
  LAttr: CFMutableAttributedStringRef;
begin
  if FCTFrame = nil then
  begin
    LAttr := CFAttributedStringCreateMutableCopy(kCFAllocatorDefault, 0, CFAttributedStringRef(FCTAttr));
    FLastColor := Color;
    FLastOpacity := Opacity;
    // Applying color attributes
    CFAttributedStringBeginEditing(LAttr);
    try
      AddColorAttribute(LAttr, Color, Opacity, 0, Text.Length);
      for i := 0 to AttributesCount - 1 do
        if (Color <> Attributes[i].Attribute.Color) then
          AddColorAttribute(LAttr, Attributes[i].Attribute.Color, Opacity,
            Attributes[i].Range.Pos, Attributes[i].Range.Length);
    finally
      CFAttributedStringEndEditing(LAttr);
    end;
    FrameSetter := CTFramesetterCreateWithAttributedString(CFAttributedStringRef(LAttr));
    CFRelease(LAttr);
    FCTFrame := CTFramesetterCreateFrame(FrameSetter, CFRangeMake(0, 0), FCTPath, nil);
    CFRelease(FrameSetter);
  end;
end;

destructor TTextLayoutCT.Destroy;
begin
  if FCTFrame <> nil then
    CFRelease(FCTFrame);
  if FCTAttr <> nil then
    CFRelease(FCTAttr);
  if FCTPath <> nil then
    CFRelease(FCTPath);
  inherited;
end;

procedure TTextLayoutCT.DoDrawLayout(const ACanvas: TCanvas);
var
  { strikeout }
  leftLines: CFArrayRef;
  origins: array of CGPoint;
  lineIndex: NSInteger;
  i, j: Integer;
  runs: CFArrayRef;
  lineBounds: CGRect;
  oneLine: CTLineRef;
  offset: CGFloat;
  oneRun: CTRunRef;
  ascent: CGFloat;
  descent: CGFloat;
  rwidth: CGFloat;
  bounds: CGRect;
  y: CGFloat;
  LColor: TAlphaColorF;
begin
  if (FCTAttr = nil) or (TCanvasQuartz(ACanvas).Context = nil) or Text.IsEmpty then
    Exit;

  if not SameValue(TCanvasQuartz(ACanvas).FFontScale, FFontScale) then
  begin
    // Recreate layout (usually by Printer.SelectDPI)
    FFontScale := TCanvasQuartz(ACanvas).FFontScale;
    try
      DoRenderLayout;
    finally
      FFontScale := 1.0;
    end;
  end;

  CGContextSaveGState(TCanvasQuartz(ACanvas).Context);
  try
    CGContextClipToRect(TCanvasQuartz(ACanvas).Context,
      CGRectMake(TopLeft.X, TopLeft.Y, MaxSize.X, MaxSize.Y));

    CGContextSetTextMatrix(TCanvasQuartz(ACanvas).Context,
      CGAffineTransformMakeScale(1.0, 1.0));

    CGContextTranslateCTM(TCanvasQuartz(ACanvas).Context,
      TopLeft.X, TopLeft.Y + MaxSize.Y);
    CGContextScaleCTM(TCanvasQuartz(ACanvas).Context, 1, -1);

    CGContextTranslateCTM(TCanvasQuartz(ACanvas).Context,
      0, -($FFFF - MaxSize.Y));

    // Applying vertical align
    case VerticalAlign of
      TTextAlign.Center:
        CGContextTranslateCTM(TCanvasQuartz(ACanvas).Context,
          0, -(MaxSize.Y - FTextHeight + Padding.Top - Padding.Bottom) / 2);
      TTextAlign.Leading:
        CGContextTranslateCTM(TCanvasQuartz(ACanvas).Context, 0, -Padding.Top);
      TTextAlign.Trailing:
        CGContextTranslateCTM(TCanvasQuartz(ACanvas).Context,
          0, -(MaxSize.Y - FTextHeight - Padding.Bottom));
    end;
    if (FCTFrame <> nil) and ((Color <> FLastColor) or (Opacity <> FLastOpacity)) then
    begin
      CFRelease(FCTFrame);
      FCTFrame := nil;
    end;
    CreateCTFrame;

    CTFrameDraw(FCTFrame, TCanvasQuartz(ACanvas).Context);
    { strikeout }
    if TFontStyle.fsStrikeOut in Font.Style then
    begin
      CGContextSetTextPosition(TCanvasQuartz(ACanvas).Context, 0, 0);
      leftLines := CTFrameGetLines(FCTFrame);
      SetLength(origins, CFArrayGetCount(leftLines));
      CTFrameGetLineOrigins(FCTFrame, CFRangeMake(0, 0), @origins[0]);
      lineIndex := 0;
      for i := 0 to High(origins) do
      begin
        oneLine := CFArrayGetValueAtIndex(leftLines, i);
        runs := CTLineGetGlyphRuns(oneLine);
        lineBounds := CTLineGetImageBounds(oneLine, TCanvasQuartz(ACanvas).Context);
        lineBounds.origin.x := lineBounds.origin.x + origins[lineIndex].x;
        lineBounds.origin.y := lineBounds.origin.y + origins[lineIndex].y;
        lineIndex := lineIndex + 1;
        offset := 0;
        for j := 0 to CFArrayGetCount(runs) - 1 do
        begin
          oneRun := CFArrayGetValueAtIndex(runs, j);
          ascent := 0;
          descent := 0;
          rwidth := CTRunGetTypographicBounds(oneRun, CFRangeMake(0, 0), @ascent, @descent, nil);
          bounds := CGRectFromRect(RectF(lineBounds.origin.x + offset, lineBounds.origin.y, lineBounds.origin.x + offset + rwidth,
            lineBounds.origin.y + Font.Size * FFontScale - descent));
          if (bounds.origin.x + bounds.size.width > CGRectGetMaxX(lineBounds)) then
            bounds.size.width := CGRectGetMaxX(lineBounds) - bounds.origin.x;
          LColor := TAlphaColorF.Create(MakeColor(Color, Opacity));
          CGContextSetRGBStrokeColor(TCanvasQuartz(ACanvas).Context, LColor.r, LColor.g, LColor.b, LColor.a);
          y := round(bounds.origin.y + bounds.size.height / 2.0);
          CGContextMoveToPoint(TCanvasQuartz(ACanvas).Context, bounds.origin.x, y);
          CGContextAddLineToPoint(TCanvasQuartz(ACanvas).Context, bounds.origin.x + bounds.size.width, y);
          CGContextStrokePath(TCanvasQuartz(ACanvas).Context);
          offset := offset + rwidth;
        end;
      end;
    end;
  finally
    CGContextRestoreGState(TCanvasQuartz(ACanvas).Context);
  end;
end;

procedure TTextLayoutCT.DoRenderLayout;
begin
  CreateCT;
  MeasureCT;
end;

function TTextLayoutCT.GetCTFontRef(const AFont: TFont): CTFontRef;
var
  LFontRef, NewFontRef: CTFontRef;
  Matrix: PCGAffineTransform;
begin
  Result := nil;
  Matrix := nil;
  LFontRef := CTFontCreateWithName(CFSTR(AFont.Family), AFont.Size * FFontScale, nil);
  try
    if TFontStyle.fsItalic in AFont.Style then
    begin
      NewFontRef := CTFontCreateCopyWithSymbolicTraits(LFontRef, 0, nil,
        kCTFontItalicTrait, kCTFontItalicTrait);
      if NewFontRef <> nil then
      begin
        CFRelease(LFontRef);
        LFontRef := NewFontRef;
      end
      else
      begin
        Matrix := @ItalicMatrix;
        // Font has no Italic version, applying transform matrix
        NewFontRef := CTFontCreateWithName(CFSTR(AFont.Family), AFont.Size * FFontScale, @ItalicMatrix);
        if NewFontRef <> nil then
        begin
          CFRelease(LFontRef);
          LFontRef := NewFontRef;
        end;
      end;
    end;
    if TFontStyle.fsBold in AFont.Style then
    begin
      NewFontRef := CTFontCreateCopyWithSymbolicTraits(LFontRef, 0, Matrix,
        kCTFontBoldTrait, kCTFontBoldTrait);
      if NewFontRef <> nil then
      begin
        CFRelease(LFontRef);
        LFontRef := NewFontRef;
      end;
    end;
    Result := LFontRef;
  except
    CFRelease(LFontRef);
  end;
end;

function TTextLayoutCT.GetTextHeight: Single;
begin
  Result := FTextHeight;
end;

function TTextLayoutCT.GetTextRect: TRectF;
begin
  Result := TRectF.Create(FTextLeft, 0, FTextLeft + FTextWidth, FTextHeight);
  Result.Offset(TopLeft);
  case VerticalAlign of
    TTextAlign.Center:
      Result.Offset(0, (MaxSize.Y - FTextHeight + Padding.Top - Padding.Bottom) / 2);
    TTextAlign.Leading:
      Result.Offset(0, Padding.Top);
    TTextAlign.Trailing:
      Result.Offset(0, (MaxSize.Y - FTextHeight - Padding.Bottom));
  end;
end;

function TTextLayoutCT.GetTextWidth: Single;
begin
  Result := FTextWidth;
end;

procedure TTextLayoutCT.MeasureCT;
var
  LFrameRect: CGRect;
  Lines: CFArrayRef;
  NumLines: CFIndex;
  MaxWidth: CGFloat;
  TextHeight: CGFloat;
  LastLineIndex: CFIndex;
  Index: CFIndex;
  LAscent, LDescent, LLeading, LWidth: CGFloat;
  Line: CTLineRef;
  LineOrigin, LastLineOrigin: CGPoint;
begin
  FTextHeight := 0;
  FTextWidth := 0;
  FTextLeft := MaxSingle;
  if FCTAttr <> nil then
  begin
    CreateCTFrame;
    LFrameRect := CGPathGetBoundingBox(FCTPath);
    Lines := CTFrameGetLines(FCTFrame);
    NumLines := CFArrayGetCount(Lines);
    MaxWidth := 0;
    TextHeight := 0;
    LastLineIndex := NumLines - 1;

    if NumLines > 0 then
      for Index := 0 to NumLines - 1 do
      begin
        Line := CTLineRef(CFArrayGetValueAtIndex(Lines, Index));
        LWidth := CTLineGetTypographicBounds(Line, @LAscent,  @LDescent, @LLeading);
        MaxWidth := Max(LWidth, MaxWidth);
        if Index = LastLineIndex then
        begin
          CTFrameGetLineOrigins(FCTFrame, CFRangeMake(LastLineIndex, 1), @LastLineOrigin);
          TextHeight := CGRectGetMaxY(LFrameRect) - LastLineOrigin.y + LDescent;
        end;
        CTFrameGetLineOrigins(FCTFrame, CFRangeMake(Index, 1), @LineOrigin);
        FTextLeft := Min(FTextLeft, LineOrigin.x);
      end
    else
      FTextLeft := 0;
    FTextWidth := MaxWidth;
    FTextHeight := TextHeight;
  end;
end;

function TTextLayoutCT.DoPositionAtPoint(const APoint: TPointF): Integer;
var
  Line: CTLineRef;
  Lines: CFArrayRef;
  NumLines: CFIndex;
  i: Integer;
  Index: CFIndex;
  LineOrigin: CGPoint;
  Ascent, Descent, Leading, Width: CGFloat;
  LRect: TRectF;
  FrameMaxY, LineHeight, LinesBeforeHeight: CGFloat;
  TopY: Single;
begin
  Result := -1;

  if FCTAttr = nil then
    Exit;

  LRect := Self.TextRect;
  if not IsPointInRect(APoint, LRect) then
  begin
    if IsPointInRect(APoint, TRectF.Create(LRect.Left, LRect.Top, LRect.Left + MaxSize.X, LRect.Bottom)) then
      Result := Text.Length
    else
      if ((APoint.X < LRect.Left) or SameValue(APoint.X, LRect.Left, Epsilon)) and
         ((APoint.Y > LRect.Top) or SameValue(APoint.Y, LRect.Top, Epsilon)) and
         ((APoint.Y < LRect.Bottom) or SameValue(APoint.Y, LRect.Bottom, Epsilon)) then
        Result := 0;
    Exit;
  end;
  if Text.IsEmpty then
    Exit(0);

  CreateCTFrame;
  Lines := CTFrameGetLines(FCTFrame);
  NumLines := CFArrayGetCount(Lines);
  FrameMaxY := CGRectGetMaxY(CGPathGetBoundingBox(FCTPath));
  LinesBeforeHeight := 0;
  TopY := TextRect.Top;
  for i := 0 to NumLines - 1 do
  begin
    Line := CTLineRef(CFArrayGetValueAtIndex(Lines, i));
    Width := CTLineGetTypographicBounds(Line, @Ascent,  @Descent, @Leading);
    if Width = 0 then
      Continue;
    CTFrameGetLineOrigins(FCTFrame, CFRangeMake(i, 1), @LineOrigin);
    LineHeight := FrameMaxY - LineOrigin.y + Descent - LinesBeforeHeight;
    if (APoint.Y >= (TopY + LinesBeforeHeight)) and
      (APoint.Y < (TopY + LinesBeforeHeight + LineHeight)) then
    begin
      Index := CTLineGetStringIndexForPosition(Line,
        NSPoint(TPointF.Create(APoint.X - Padding.Left - LineOrigin.x - TopLeft.X,
          $FFFF - APoint.Y - Padding.Top - TopLeft.Y)));
      if Index >= 0 then
      begin
        Result := Index;
        if Result > 0 then
        begin
          Ascent := CTLineGetOffsetForStringIndex(Line, Result, nil);
          Descent := CTLineGetOffsetForStringIndex(Line, Result - 1, nil);
          Width := Ascent - Descent;
          if (Width > 0) and (APoint.X < (TopLeft.X + Padding.Left + Descent + Width * 3 / 5)) then
            Dec(Result);
        end;
        Break;
      end;
    end;
    LinesBeforeHeight := LinesBeforeHeight + LineHeight;
  end;
  if (Result >= 0) and (Result < Text.Length) and Text.Chars[Result].IsLowSurrogate then
    Inc(Result);
end;

function TTextLayoutCT.DoRegionForRange(const ARange: TTextRange): TRegion;
var
  Line: CTLineRef;
  Lines: CFArrayRef;
  NumLines: CFIndex;
  Range: CFRange;
  Runs: CFArrayRef;
  Run: CTRunRef;
  I, J, K: Integer;
  LineOrigin: CGPoint;
  RunGlyphCount: CFIndex;
  Positions: array of CGPoint;
  Advances: array of CGSize;
  Ascent, Descent, Leading, Width: CGFloat;
  Cur: Integer;
  CurLine: TRegion;
  RangeLength, RemainsLength: Integer;
  LRange: TTextRange;
  AlignShift: TPointF;
begin
  SetLength(Result, 0);
  if (FCTAttr = nil) or (ARange.Pos < 0) or (ARange.Length < 0) then
    Exit;

  if Text.Length = 0 then
  begin
    // Getting rect at the end of the text
    SetLength(Result, 1);
    Result[0] := Self.TextRect;
    Result[0].Width := 0;
    Exit;
  end;

  RangeLength := Text.Length;
  if ARange.Pos > RangeLength then
    Exit;

  CreateCTFrame;
  SetLength(Result, 0);
  LRange := ARange;
  if (ARange.Pos = Text.Length) and (ARange.Length = 0) then
  begin
    LRange.Length := 1;
    LRange.Pos := Text.Length - 1;
  end;

  RemainsLength := Min(LRange.Length, RangeLength - LRange.Pos);

  if (LRange.Pos < Text.Length) and Text.Chars[LRange.Pos].IsLowSurrogate then
  begin
    LRange.Pos := LRange.Pos - 1;
    LRange.Length := LRange.Length + 1;
  end;
  if ((LRange.Pos + LRange.Length) < Text.Length) and Text.Chars[LRange.Pos + LRange.Length].IsLowSurrogate then
    LRange.Length := LRange.Length + 1;

  Lines := CTFrameGetLines(FCTFrame);
  NumLines := CFArrayGetCount(Lines);
  for I := 0 to NumLines - 1 do
  begin
    Line := CTLineRef(CFArrayGetValueAtIndex(Lines, I));
    Range := CTLineGetStringRange(Line);
    if (Range.location + RemainsLength) < 0 then
      Continue;
    if Range.location > (LRange.Pos + RemainsLength) then
      Break;
    Width := CTLineGetTypographicBounds(Line, @Ascent,  @Descent, @Leading);
    if Width = 0 then
      Continue;
    CTFrameGetLineOrigins(FCTFrame, CFRangeMake(I, 1), @LineOrigin);
    Runs := CTLineGetGlyphRuns(Line);
    Cur := 0;
    SetLength(CurLine, 0);
    for J := 0 to CFArrayGetCount(Runs) - 1 do
    begin
      Run := CFArrayGetValueAtIndex(Runs, J);
      RunGlyphCount := CTRunGetGlyphCount(Run);
      if RunGlyphCount > 0 then
      begin
        SetLength(Positions, RunGlyphCount);
        SetLength(Advances, RunGlyphCount);
        CTRunGetPositions(Run, CFRangeMake(0, 0), @Positions[0]);
        CTRunGetAdvances(Run, CFRangeMake(0, 0), @Advances[0]);
        for K := 0 to RunGlyphCount - 1 do
        begin
          if (((Range.location + Cur) >= LRange.Pos) and ((Range.location + Cur) < (LRange.Pos + RemainsLength))) or
             ((RemainsLength = 0) and ((Range.location + Cur) = 0))  then
          begin
            SetLength(CurLine, Length(CurLine) + 1);
            CurLine[High(CurLine)] := TRectF.Create(LineOrigin.x + Positions[K].x,
              $FFFF - (LineOrigin.y + Ascent + Descent),
              LineOrigin.x + Positions[K].x + Advances[K].width,
              $FFFF - (LineOrigin.y - Descent));
            // Resetting width if selection length is 0 (i.e. only position
            // need to be calculated, not a region)
            if RangeLength = 0 then
              CurLine[High(CurLine)].Width := 0;
          end;
          Inc(Cur);
          if (Cur < Text.Length) and Text.Chars[Cur].IsLowSurrogate then
            Inc(Cur);
        end;
      end;
    end;
    if Length(CurLine) > 0 then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := CurLine[0];
      for J := 0 to High(CurLine) do
        Result[High(Result)].Union(CurLine[J]);
      Result[High(Result)].Offset(TopLeft);

      AlignShift := TPointF.Create(Padding.Left, 0);
      case VerticalAlign of
        TTextAlign.Center:
          AlignShift.Y := (MaxSize.Y - FTextHeight + Padding.Top - Padding.Bottom) / 2;
        TTextAlign.Leading:
          AlignShift.Y := Padding.Top;
        TTextAlign.Trailing:
          AlignShift.Y := MaxSize.Y - FTextHeight - Padding.Bottom;
      end;
      Result[High(Result)].Offset(AlignShift);

      if (ARange.Pos = Text.Length) and (ARange.Length = 0) then
        Result[0].Left := Result[0].Right;
    end;
  end;
  if ARange.Length = 0 then
    for I := Low(Result) to High(Result) do
      Result[I].Right := Result[I].Left;
end;

procedure RegisterCanvasClasses;
begin
  TCanvasManager.RegisterCanvas(TCanvasQuartz, True, True);
end;

procedure UnregisterCanvasClasses;
begin
end;

initialization
  TTextLayoutManager.RegisterTextLayout(TTextLayoutCT, TCanvasQuartz);
  TBitmapCodecManager.RegisterBitmapCodecClass(SBMPImageExtension, SVBitmaps, True, TBitmapCodecQuartz);
  TBitmapCodecManager.RegisterBitmapCodecClass(SICNSImageExtension, SVIcons, True, TBitmapCodecQuartz);
  TBitmapCodecManager.RegisterBitmapCodecClass(SJPGImageExtension, SVJPGImages, True, TBitmapCodecQuartz);
  TBitmapCodecManager.RegisterBitmapCodecClass(SJP2ImageExtension, SVJP2Images, True, TBitmapCodecQuartz);
  TBitmapCodecManager.RegisterBitmapCodecClass(SJPEGImageExtension, SVJPGImages, True, TBitmapCodecQuartz);
  TBitmapCodecManager.RegisterBitmapCodecClass(SPNGImageExtension, SVPNGImages, True, TBitmapCodecQuartz);
  TBitmapCodecManager.RegisterBitmapCodecClass(SGIFImageExtension, SVGIFImages, True, TBitmapCodecQuartz);
  TBitmapCodecManager.RegisterBitmapCodecClass(STIFImageExtension, SVTIFFImages, True, TBitmapCodecQuartz);
  TBitmapCodecManager.RegisterBitmapCodecClass(STGAImageExtension, SVTGAImages, True, TBitmapCodecQuartz);
finalization
  if GlobalColorSpace <> nil then
    CGColorSpaceRelease(GlobalColorSpace);
end.

