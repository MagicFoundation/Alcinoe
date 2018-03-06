{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Graphics.iOS;

interface

{$SCOPEDENUMS ON}

uses
  iOSapi.CoreGraphics, System.Types, System.Classes, FMX.Surfaces, FMX.Graphics, FMX.Platform;

type

{ TBitmapCodecQuartz }

  TBitmapCodecQuartz = class(TCustomBitmapCodec)
  private class var
    FColorSpace: CGColorSpaceRef;
  private
    class destructor ReleaseColorSpace;
    function ImageToSurface(const Img: CGImageRef; const Bitmap: TBitmapSurface;
      const MaxSizeLimit: Cardinal): Boolean;
  strict protected
    class function ColorSpace: CGColorSpaceRef; static; inline;
  public
    class function GetImageSize(const AFileName: string): TPointF; override;
    class function IsValid(const AStream: TStream): Boolean; override;
    function LoadFromFile(const AFileName: string; const Bitmap: TBitmapSurface;
      const MaxSizeLimit: Cardinal): Boolean; override;
    function LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: Single;
      const UseEmbedded: Boolean; const Bitmap: TBitmapSurface): Boolean; override;
    function SaveToFile(const AFileName: string; const Bitmap: TBitmapSurface;
      const SaveParams: PBitmapCodecSaveParams): Boolean; override;
    function LoadFromStream(const AStream: TStream; const Bitmap: TBitmapSurface;
      const MaxSizeLimit: Cardinal): Boolean; override;
    function SaveToStream(const AStream: TStream; const Bitmap: TBitmapSurface; const Extension: string;
       const SaveParams: PBitmapCodecSaveParams): Boolean; override;
  end;

{ TCocoaGraphicServices }

  /// <summary>Implementations of <c>IFMXContextService</c> and <c>IFMXCanvasService</c> services for iOS</summary>
  TCocoaTouchGraphicServices = class(TInterfacedObject, IFMXContextService, IFMXCanvasService)
  private
    procedure RegisterServices;
    procedure UnregisterServices;
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXContextService }
    /// <summary>Registers class of context</summary>
    procedure RegisterContextClasses;
    /// <summary>Unregisters class of context</summary>
    procedure UnregisterContextClasses;
    { IFMXCanvasService }
    /// <summary>Registers class of canvas</summary>
    procedure RegisterCanvasClasses;
    /// <summary>Unregisters class of canvas</summary>
    procedure UnregisterCanvasClasses;
  end;

implementation

uses
  Macapi.ObjectiveC, iOSapi.Foundation, iOSapi.UIKit, Macapi.Helpers, System.SysUtils, FMX.Consts,
  FMX.Canvas.GPU, FMX.Context.GLES.iOS, FMX.Types;

{ TBitmapCodecQuartz }

class function TBitmapCodecQuartz.ColorSpace: CGColorSpaceRef;
begin
  if FColorSpace = nil then
    FColorSpace := CGColorSpaceCreateDeviceRGB;
  Result := FColorSpace;
end;

class destructor TBitmapCodecQuartz.ReleaseColorSpace;
begin
  if FColorSpace <> nil then
  begin
    CGColorSpaceRelease(FColorSpace);
    FColorSpace := nil;
  end;
end;

class function TBitmapCodecQuartz.GetImageSize(const AFileName: string): TPointF;
var
  Img: UIImage;
begin
  Img := TUIImage.Wrap(TUIImage.alloc.initWithContentsOfFile(StrToNSStr(AFileName)));
  if Img <> nil then
  try
    Result := PointF(Img.Size.width, Img.Size.height);
  finally
    Img.release;
  end
  else
    Result := TPointF.Zero;
end;

class function TBitmapCodecQuartz.IsValid(const AStream: TStream): Boolean;
var
  Img: UIImage;
  MemStream: TMemoryStream;
  Data: NSData;
  SavePosition: Integer;
begin
  Result := False;
  SavePosition := AStream.Position;
  try
    MemStream := TMemoryStream.Create;
    try
      MemStream.CopyFrom(AStream, AStream.Size);
      MemStream.Position := 0;

      Data := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(MemStream.Memory, MemStream.Size, False));
      try
        if Data.length > 0 then
        begin
          Img := TUIImage.Wrap(TUIImage.alloc.initWithData(Data));
          if Img <> nil then
          try
            Result := Img.cGImage <> nil;
          finally
            Img.release;
          end;
        end;
      finally
        Data.release;
      end;
    finally
      MemStream.Free;
    end;
  finally
    AStream.Position := SavePosition;
  end;
end;

function TBitmapCodecQuartz.LoadFromStream(const AStream: TStream; const Bitmap: TBitmapSurface;
  const MaxSizeLimit: Cardinal): Boolean;
var
  Img: UIImage;
  MemStream: TMemoryStream;
  Data: NSData;
begin
  Result := False;
  MemStream := TMemoryStream.Create;
  try
    MemStream.CopyFrom(AStream, AStream.Size);
    MemStream.Position := 0;

    Data := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(MemStream.Memory, MemStream.Size, False));
    try
      if Data.length > 0 then
      begin
        Img := TUIImage.Wrap(TUIImage.alloc.initWithData(Data));
        if Img <> nil then
        try
          if Img.cGImage <> nil then
            Result := ImageToSurface(Img.cGImage, Bitmap, MaxSizeLimit);
        finally
          Img.release;
        end;
      end;
    finally
      Data.release;
    end;
  finally
    MemStream.Free;
  end;
end;

function TBitmapCodecQuartz.SaveToStream(const AStream: TStream; const Bitmap: TBitmapSurface; const Extension: string;
  const SaveParams: PBitmapCodecSaveParams): Boolean;
var
  Data: NSData;
  Img: UIImage;
  ImgRef: CGImageRef;
  CtxRef: CGContextRef;
begin
  Result := False;

  CtxRef := CGBitmapContextCreate(Bitmap.Bits, Bitmap.Width, Bitmap.Height, 8, Bitmap.Pitch, ColorSpace,
    kCGImageAlphaPremultipliedLast);
  if CtxRef <> nil then
  try
    ImgRef := CGBitmapContextCreateImage(CtxRef);
    if ImgRef <> nil then
    try
      Img := TUIImage.Wrap(TUIImage.alloc.initWithCGImage(ImgRef));
      if Img <> nil then
      try
        if SameText(Extension, SJPGImageExtension) or SameText(Extension, SJPEGImageExtension) then
        begin
          if SaveParams <> nil then
            Data := TNSData.Wrap(UIImageJPEGRepresentation((Img as ILocalObject).GetObjectID, SaveParams.Quality / 100))
          else
            Data := TNSData.Wrap(UIImageJPEGRepresentation((Img as ILocalObject).GetObjectID, 1));
        end
        else
          Data := TNSData.Wrap(UIImagePNGRepresentation((Img as ILocalObject).GetObjectID));

        Result := AStream.Write(Data.bytes^, Data.length) = Abs(Data.length);
      finally
        Img.release;
      end;
    finally
      CGImageRelease(ImgRef);
    end;
  finally
    CGContextRelease(CtxRef);
  end;
end;

function TBitmapCodecQuartz.ImageToSurface(const Img: CGImageRef; const Bitmap: TBitmapSurface;
  const MaxSizeLimit: Cardinal): Boolean;
var
  R: TRectF;
  CtxRef: CGContextRef;
  Width, Height: Cardinal;
begin
  Result := False;
  Width := CGImageGetWidth(Img);
  Height := CGImageGetHeight(Img);
  if (MaxSizeLimit > 0) and ((Width > MaxSizeLimit) or (Height > MaxSizeLimit)) then
  begin
    R := TRectF.Create(0, 0, Width, Height);
    R.Fit(TRectF.Create(0, 0, MaxSizeLimit, MaxSizeLimit));
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

function TBitmapCodecQuartz.LoadFromFile(const AFileName: string; const Bitmap: TBitmapSurface;
  const MaxSizeLimit: Cardinal): Boolean;
var
  Img: UIImage;
begin
  Result := False;

  Img := TUIImage.Wrap(TUIImage.alloc.initWithContentsOfFile(StrToNSStr(AFileName)));
  if Img <> nil then
  try
    if Img.cGImage <> nil then
      Result := ImageToSurface(Img.cGImage, Bitmap, MaxSizeLimit);
  finally
    Img.release;
  end;
end;

function TBitmapCodecQuartz.LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: Single;
  const UseEmbedded: Boolean; const Bitmap: TBitmapSurface): Boolean;
var
  Img: UIImage;
  ImgRef: CGImageRef;
  CtxRef: CGContextRef;
  R: TRectF;
begin
  Result := False;

  Img := TUIImage.Wrap(TUIImage.alloc.initWithContentsOfFile(StrToNSStr(AFileName)));
  if Img <> nil then
  try
    ImgRef := Img.cGImage;
    if ImgRef <> nil then
    begin
      R := TRectF.Create(0, 0, CGImageGetWidth(ImgRef), CGImageGetHeight(ImgRef));
      R.Fit(TRectF.Create(0, 0, AFitWidth, AFitHeight));

      Bitmap.SetSize(Round(R.Width), Round(R.Height), TPixelFormat.RGBA);

      CtxRef := CGBitmapContextCreate(Bitmap.Bits, Bitmap.Width, Bitmap.Height, 8, Bitmap.Pitch, ColorSpace,
        kCGImageAlphaPremultipliedLast);
      try
        CGContextDrawImage(CtxRef, CGRectMake(0, 0, Bitmap.Width, Bitmap.Height), imgRef);
      finally
        CGContextRelease(CtxRef);
      end;

      Result := True;
    end;
  finally
    Img.release;
  end;
end;

function TBitmapCodecQuartz.SaveToFile(const AFileName: string; const Bitmap: TBitmapSurface;
  const SaveParams: PBitmapCodecSaveParams): Boolean;
var
  Data: NSData;
  Img: UIImage;
  ImgRef: CGImageRef;
  CtxRef: CGContextRef;
  AStream: TStream;
begin
  Result := False;

  CtxRef := CGBitmapContextCreate(Bitmap.Bits, Bitmap.Width, Bitmap.Height, 8, Bitmap.Pitch, ColorSpace,
    kCGImageAlphaPremultipliedLast);
  if CtxRef <> nil then
  try
    ImgRef := CGBitmapContextCreateImage(CtxRef);
    if ImgRef <> nil then
    try
      Img := TUIImage.Wrap(TUIImage.alloc.initWithCGImage(ImgRef));
      if Img <> nil then
      try
        if SameText(ExtractFileExt(AFileName), SJPGImageExtension) or SameText(ExtractFileExt(AFileName), SJPEGImageExtension) then
        begin
          if SaveParams <> nil then
            Data := TNSData.Wrap(UIImageJPEGRepresentation((Img as ILocalObject).GetObjectID, SaveParams.Quality / 100))
          else
            Data := TNSData.Wrap(UIImageJPEGRepresentation((Img as ILocalObject).GetObjectID, 1));
        end
        else
          Data := TNSData.Wrap(UIImagePNGRepresentation((Img as ILocalObject).GetObjectID));

        try
          AStream := TFileStream.Create(AFileName, fmCreate);
          try
            AStream.WriteBuffer(Data.bytes^, Data.length);
          finally
            AStream.Free;
          end;
        except
          Exit;
        end;

        Result := True;
      finally
        Img.release;
      end;
    finally
      CGImageRelease(ImgRef);
    end;
  finally
    CGContextRelease(CtxRef);
  end;
end;

{ TCocoaGraphicServices }

procedure TCocoaTouchGraphicServices.RegisterServices;
begin
  if not TPlatformServices.Current.SupportsPlatformService(IFMXContextService) then
    TPlatformServices.Current.AddPlatformService(IFMXContextService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXCanvasService) then
    TPlatformServices.Current.AddPlatformService(IFMXCanvasService, Self);
end;

procedure TCocoaTouchGraphicServices.UnregisterServices;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXContextService);
  TPlatformServices.Current.RemovePlatformService(IFMXCanvasService);
end;

constructor TCocoaTouchGraphicServices.Create;
begin
  inherited;
  RegisterServices;
end;

destructor TCocoaTouchGraphicServices.Destroy;
begin
  UnregisterServices;
  inherited;
end;

procedure TCocoaTouchGraphicServices.RegisterCanvasClasses;
begin
  FMX.Canvas.GPU.RegisterCanvasClasses;
end;

procedure TCocoaTouchGraphicServices.RegisterContextClasses;
begin
  FMX.Context.GLES.iOS.RegisterContextClasses;
end;

procedure TCocoaTouchGraphicServices.UnregisterCanvasClasses;
begin
  FMX.Canvas.GPU.UnregisterCanvasClasses;
end;

procedure TCocoaTouchGraphicServices.UnregisterContextClasses;
begin
  FMX.Context.GLES.iOS.UnregisterContextClasses;
end;

initialization
  { Codecs }
  TBitmapCodecManager.RegisterBitmapCodecClass(SJPGImageExtension, SVJPGImages, True, TBitmapCodecQuartz);
  TBitmapCodecManager.RegisterBitmapCodecClass(SJPEGImageExtension, SVJPGImages, True, TBitmapCodecQuartz);
  TBitmapCodecManager.RegisterBitmapCodecClass(SPNGImageExtension, SVPNGImages, True, TBitmapCodecQuartz);
end.
