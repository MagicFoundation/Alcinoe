{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{                                                       }
{            Helpers for iOS implementations            }
{                                                       }
{ Copyright(c) 2016 Embarcadero Technologies, Inc.      }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Helpers.iOS;

interface

{$SCOPEDENUMS ON}

uses
  System.UITypes, System.Types, iOSapi.UIKit, iOSapi.Foundation, iOSapi.CoreText, Macapi.CoreFoundation, FMX.Types,
  FMX.Graphics, FMX.Surfaces;

{ Singleton instance }

function SharedApplication: UIApplication;
function MainScreen: UIScreen;
function DefaultNotificationCenter: NSNotificationCenter;

{ Bitmap convertions }

function BitmapToUIImage(const Bitmap: TBitmap): UIImage;
function BitmapSurfaceToUIImage(const BitmapSurface: TBitmapSurface): UIImage;
function UIImageToBitmap(const AImage: UIImage; const ARotate: Single; const AMaxSize: TSize): TBitmap;
function UIImageToBitmapSurface(const AImage: UIImage): TBitmapSurface;
function NativeViewToSurface(const ANativeView: UIView; const ASurface: TBitmapSurface): Boolean;
function UIViewToUIImage(const AView: UIView): UIImage;

{ Device functions }

function IsPhone: Boolean;
function IsPad: Boolean;

{ Color convertions }

function AlphaColorToUIColor(const Color: TAlphaColor): UIColor;

{ Text Alignment convertions }

function TextAlignToUITextAlignment(const ATextAlignment: TTextAlign): UITextAlignment;

{ Virtual Keyboard Type convertions }

function VirtualKeyboardTypeToUIKeyboardType(const AVirtualKeyboardType: TVirtualKeyboardType): UIKeyboardType;
function ReturnKeyTypeToUIReturnKeyType(const AReturnKeyType: TReturnKeyType): UIReturnKeyType;

{ Font convertions }

function FontToUIFont(const AFont: TFont): UIFont;
function FontToCTFontRef(const AFont: TFont): CTFontRef;

{ Orientation conversion }

function ScreenOrientationToUIInterfaceOrientation(const ASource: TScreenOrientations): UIInterfaceOrientation;

implementation

uses
  System.SysUtils, System.Math, Macapi.Helpers, Macapi.ObjectiveC, iOSapi.CocoaTypes, iOSapi.CoreGraphics,
  iOSapi.CoreImage, iOSapi.Helpers;

function SharedApplication: UIApplication;
begin
  Result := TiOSHelper.SharedApplication;
end;

function MainScreen: UIScreen;
begin
  Result := TiOSHelper.MainScreen;
end;

function DefaultNotificationCenter: NSNotificationCenter;
begin
  Result := TiOSHelper.DefaultNotificationCenter;
end;

function IsPhone: Boolean;
begin
  Result := NSStrToStr(TiOSHelper.CurrentDevice.model) = 'iPhone';
end;

function IsPad: Boolean;
begin
  Result := TiOSHelper.CurrentDevice.userInterfaceIdiom = UIUserInterfaceIdiomPad;
end;

function UIImageToBitmap(const AImage: UIImage; const ARotate: Single; const AMaxSize: TSize): TBitmap;

  function ReduceImageSize(const AOriginalSize: TSize): TSize;
  var
    ImageRatio: Double;
    ScaleCoef: Double;
    MinWidth: Integer;
    MinHeight: Integer;
    MaxBitmapSize: Integer;
  begin
    Result := AOriginalSize;
    MinWidth := Min(AOriginalSize.Width, AMaxSize.Width);
    MinHeight := Min(AOriginalSize.Height, AMaxSize.Height);
    ImageRatio := AOriginalSize.Width / AOriginalSize.Height;
    if MinWidth / MinHeight < ImageRatio then
      Result := TSize.Create(MinWidth, Round(MinWidth / ImageRatio))
    else
      Result := TSize.Create(Round(MinHeight * ImageRatio), MinHeight);

    MaxBitmapSize := TCanvasManager.DefaultCanvas.GetAttribute(TCanvasAttribute.MaxBitmapSize);
    if (MaxBitmapSize > 0) and (Max(AOriginalSize.cx, AOriginalSize.cy) div MaxBitmapSize > 0) then
    begin
      ScaleCoef := Max(AOriginalSize.cx, AOriginalSize.cy) / MaxBitmapSize;
      Result := TSize.Create(Round(AOriginalSize.cx / ScaleCoef), Round(AOriginalSize.cy / ScaleCoef));
    end;
  end;

var
  ImageRef: CGImageRef;
  Bitmap: TBitmap;
  CtxRef: CGContextRef;
  ColorSpace: CGColorSpaceRef;
  Data: TBitmapData;
  BitmapSize: TSize;
begin
  ImageRef := AImage.CGImage;
  if ImageRef <> nil then
  begin
    BitmapSize := ReduceImageSize(TSize.Create(CGImageGetWidth(ImageRef), CGImageGetHeight(ImageRef)));
    Bitmap := TBitmap.Create(BitmapSize.cx, BitmapSize.cy);
    ColorSpace := CGColorSpaceCreateDeviceRGB;
    try
      if Bitmap.Map(TMapAccess.Write, Data) then
      try
        CtxRef := CGBitmapContextCreate(Data.Data, Bitmap.Width, Bitmap.Height, 8, Data.Pitch, ColorSpace,
          kCGImageAlphaPremultipliedLast or kCGBitmapByteOrder32Big);
        try
          CGContextDrawImage(CtxRef, CGRectMake(0, 0, Bitmap.Width, BitMap.Height), ImageRef);
        finally
          CGContextRelease(CtxRef);
        end;
      finally
        Bitmap.Unmap(Data);
      end;
    finally
      CGColorSpaceRelease(ColorSpace);
    end;
    Bitmap.Rotate(ARotate);
    Result := Bitmap;
  end
  else
    Result := nil;
end;

function UIImageToBitmapSurface(const AImage: UIImage): TBitmapSurface;
var
  ImageRef: CGImageRef;
  BitmapSurface: TBitmapSurface;
  CtxRef: CGContextRef;
  ColorSpace: CGColorSpaceRef;
  BitmapSize: TSize;
begin
  ImageRef := AImage.CGImage;
  if ImageRef <> nil then
  begin
    BitmapSize := TSize.Create(CGImageGetWidth(ImageRef), CGImageGetHeight(ImageRef));
    BitmapSurface := TBitmapSurface.Create;
    BitmapSurface.SetSize(BitmapSize.cx, BitmapSize.cy);
    ColorSpace := CGColorSpaceCreateDeviceRGB;
    try
      CtxRef := CGBitmapContextCreate(BitmapSurface.Bits, BitmapSurface.Width, BitmapSurface.Height, 8,
        BitmapSurface.Pitch, ColorSpace, kCGImageAlphaPremultipliedLast or kCGBitmapByteOrder32Big);
      try
        CGContextDrawImage(CtxRef, CGRectMake(0, 0, BitmapSurface.Width, BitmapSurface.Height), ImageRef);
      finally
        CGContextRelease(CtxRef);
      end;
    finally
      CGColorSpaceRelease(ColorSpace);
    end;
    Result := BitmapSurface;
  end
  else
    Result := nil;
end;

function NativeViewToSurface(const ANativeView: UIView; const ASurface: TBitmapSurface): Boolean;
var
  Size: NSSize;
  ContRef: CGContextRef;
  ColorSpace: CGColorSpaceRef;
begin
  Size := ANativeView.frame.size;
  ColorSpace := CGColorSpaceCreateDeviceRGB;
  ASurface.SetSize(Ceil(Size.width), Ceil(Size.height));
  ContRef := CGBitmapContextCreate(ASurface.Bits, ASurface.Width, ASurface.Height, 8,
    4 * ASurface.Width, ColorSpace, kCGImageAlphaPremultipliedLast or kCGBitmapByteOrder32Big);
  try
    ANativeView.layer.renderInContext(ContRef);
  finally
    CGContextRelease(ContRef);
  end;
  ASurface.Flip;
  Result := True;
end;

function UIViewToUIImage(const AView: UIView): UIImage;
begin
  Result := nil;
  UIGraphicsBeginImageContextWithOptions(AView.bounds.size, AView.isOpaque, 0);
  try
    AView.drawViewHierarchyInRectAfterScreenUpdates(AView.bounds, False);
    Result := TUIImage.Wrap(UIGraphicsGetImageFromCurrentImageContext);
  finally
    UIGraphicsEndImageContext;
  end;
end;

function BitmapToUIImage(const Bitmap: TBitmap): UIImage;
var
  ImageRef: CGImageRef;
  CtxRef: CGContextRef;
  ColorSpace: CGColorSpaceRef;
  BitmapData: TBitmapData;
begin
  if Bitmap.IsEmpty then
    Result := TUIImage.Create
  else
  begin
    ColorSpace := CGColorSpaceCreateDeviceRGB;
    try
      if Bitmap.Map(TMapAccess.Read, BitmapData) then
      begin
        CtxRef := CGBitmapContextCreate(BitmapData.Data, Bitmap.Width, Bitmap.Height, 8, 4 * Bitmap.Width, ColorSpace, kCGImageAlphaPremultipliedLast or kCGBitmapByteOrder32Big );
        try
          ImageRef := CGBitmapContextCreateImage(CtxRef);
          try
            Result := TUIImage.Alloc;
            Result.initWithCGImage(ImageRef, Bitmap.BitmapScale, UIImageOrientationUp);
          finally
            CGImageRelease(ImageRef);
          end;
        finally
          CGContextRelease(CtxRef);
        end;
      end;
    finally
      CGColorSpaceRelease(ColorSpace);
    end;
  end;
end;

function BitmapSurfaceToUIImage(const BitmapSurface: TBitmapSurface): UIImage;
var
  ImageRef: CGImageRef;
  CtxRef: CGContextRef;
  ColorSpace: CGColorSpaceRef;
begin
  if BitmapSurface.Width * BitmapSurface.Height * BitmapSurface.BytesPerPixel = 0 then
    Result := TUIImage.Create
  else
  begin
    ColorSpace := CGColorSpaceCreateDeviceRGB;
    try
        CtxRef := CGBitmapContextCreate(BitmapSurface.Bits, BitmapSurface.Width, BitmapSurface.Height, 8,
          4 * BitmapSurface.Width, ColorSpace, kCGImageAlphaPremultipliedLast or kCGBitmapByteOrder32Big);
        try
          ImageRef := CGBitmapContextCreateImage(CtxRef);
          try
            Result := TUIImage.Alloc;
            Result.initWithCGImage(ImageRef, 1, UIImageOrientationUp);
          finally
            CGImageRelease(ImageRef);
          end;
        finally
          CGContextRelease(CtxRef);
        end;
    finally
      CGColorSpaceRelease(ColorSpace);
    end;
  end;
end;

function AlphaColorToUIColor(const Color: TAlphaColor): UIColor;
var
  Red: CGFloat;
  Green: CGFloat;
  Blue: CGFloat;
  Alpha: CGFloat;
begin
  Alpha := TAlphaColorRec(Color).A / 255;
  Red := TAlphaColorRec(Color).R / 255;
  Green := TAlphaColorRec(Color).G / 255;
  Blue := TAlphaColorRec(Color).B / 255;
  Result := TUIColor.Wrap(TUIColor.OCClass.colorWithRed(Red, Green, Blue, Alpha));
end;

function TextAlignToUITextAlignment(const ATextAlignment: TTextAlign): UITextAlignment;
var
  TextAligment: UITextAlignment;
begin
  case ATextAlignment of
    TTextAlign.Center:
      TextAligment := UITextAlignmentCenter;
    TTextAlign.Leading:
      TextAligment := UITextAlignmentLeft;
    TTextAlign.Trailing:
      TextAligment := UITextAlignmentRight;
  else
    TextAligment := UITextAlignmentLeft;
  end;
  Result := TextAligment;
end;

function VirtualKeyboardTypeToUIKeyboardType(const AVirtualKeyboardType: TVirtualKeyboardType): UIKeyboardType;
var
  KeyboardType: UIKeyboardType;
begin
  case AVirtualKeyboardType of
    TVirtualKeyboardType.Default:
      KeyboardType := UIKeyboardTypeDefault;
    TVirtualKeyboardType.NumbersAndPunctuation:
      KeyboardType := UIKeyboardTypeNumbersAndPunctuation;
    TVirtualKeyboardType.NumberPad:
      KeyboardType := UIKeyboardTypeNumberPad;
    TVirtualKeyboardType.PhonePad:
      KeyboardType := UIKeyboardTypePhonePad;
    TVirtualKeyboardType.Alphabet:
      KeyboardType := UIKeyboardTypeDefault;
    TVirtualKeyboardType.URL:
      KeyboardType := UIKeyboardTypeURL;
    TVirtualKeyboardType.NamePhonePad:
      KeyboardType := UIKeyboardTypeNamePhonePad;
    TVirtualKeyboardType.EmailAddress:
      KeyboardType := UIKeyboardTypeEmailAddress;
  else
    KeyboardType := UIKeyboardTypeDefault;
  end;
  Result := KeyboardType;
end;

function ReturnKeyTypeToUIReturnKeyType(const AReturnKeyType: TReturnKeyType): UIReturnKeyType;
var
  ReturnKeyType: UIReturnKeyType;
begin
  case AReturnKeyType of
    TReturnKeyType.Default:
      ReturnKeyType := UIReturnKeyDefault;
    TReturnKeyType.Done:
      ReturnKeyType := UIReturnKeyDone;
    TReturnKeyType.Go:
      ReturnKeyType := UIReturnKeyGo;
    TReturnKeyType.Next:
      ReturnKeyType := UIReturnKeyNext;
    TReturnKeyType.Search:
      ReturnKeyType := UIReturnKeySearch;
    TReturnKeyType.Send:
      ReturnKeyType := UIReturnKeySend;
  else
    ReturnKeyType := UIReturnKeyDefault;
  end;
  Result := ReturnKeyType;
end;

function GetFontName(const AFont: TFont): string;
const
  BoldStyleSuffix = '-Bold'; //Do not localize
  BoldItalicStyleSuffix = '-BoldOblique'; //Do not localize
  ItalicStyleSuffix = '-Oblique'; //Do not localize
begin
  if (TFontStyle.fsBold in AFont.Style) and (TFontStyle.fsItalic in AFont.Style) then
    Result := AFont.Family + BoldItalicStyleSuffix
  else if TFontStyle.fsBold in AFont.Style then
    Result := AFont.Family + BoldStyleSuffix
  else if TFontStyle.fsItalic in AFont.Style then
    Result := AFont.Family + ItalicStyleSuffix
  else
    Result := AFont.Family;
end;

function FontToUIFont(const AFont: TFont): UIFont;
begin
  Result := TUIFont.Wrap(TUIFont.OCClass.fontWithName(StrToNSStr(GetFontName(AFont)), AFont.Size));
end;

function FontToCTFontRef(const AFont: TFont): CTFontRef;
begin
  Result := CTFontCreateWithName(CFSTR(GetFontName(AFont)), AFont.Size, nil);
end;

function ScreenOrientationToUIInterfaceOrientation(const ASource: TScreenOrientations): UIInterfaceOrientation;
begin
  Result := 0;
  if TScreenOrientation.Landscape in ASource then
    Result := Result or UIInterfaceOrientationMaskLandscapeLeft;
  if TScreenOrientation.InvertedLandscape in ASource then
    Result := Result or UIInterfaceOrientationMaskLandscapeRight;
  if TScreenOrientation.Portrait in ASource then
    Result := Result or UIInterfaceOrientationMaskPortrait;
  if TScreenOrientation.InvertedPortrait in ASource then
    Result := Result or UIInterfaceOrientationMaskPortraitUpsideDown;
  if ASource = [] then
    Result := UIInterfaceOrientationMaskAll;
end;

end.

