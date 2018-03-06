{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2012-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.FontGlyphs.iOS;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.Classes, System.SysUtils, System.UITypes, System.UIConsts, System.Generics.Collections,
  System.Generics.Defaults, Macapi.ObjectiveC, Macapi.CoreFoundation, iOSapi.CocoaTypes, iOSapi.CoreGraphics,
  iOSapi.Foundation, iOSapi.CoreText, iOSapi.UIKit, FMX.Types, FMX.Surfaces, FMX.FontGlyphs;

type
  TIOSFontGlyphManager = class(TFontGlyphManager)
  const
    BoundsLimit = $FFFF;
  private
    FColorSpace: CGColorSpaceRef;
    FFontRef: CTFontRef;
    FDefaultBaseline: Single;
    FDefaultVerticalAdvance: Single;
    procedure GetDefaultBaseline;
    function GetPostScriptFontName: CFStringRef;
  protected
    procedure LoadResource; override;
    procedure FreeResource; override;
    function DoGetGlyph(const Char: UCS4Char; const Settings: TFontGlyphSettings;
      const UseColorfulPalette: Boolean): TFontGlyph; override;
    function DoGetBaseline: Single; override;
    function IsColorfulCharacter(const Char: UCS4Char): Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.Math, System.Character, System.Math.Vectors, FMX.Graphics, FMX.Consts, FMX.Utils,  Macapi.Helpers;

{ TIOSFontGlyphManager }

constructor TIOSFontGlyphManager.Create;
begin
  inherited Create;
  FColorSpace := CGColorSpaceCreateDeviceRGB;
end;

destructor TIOSFontGlyphManager.Destroy;
begin
  CGColorSpaceRelease(FColorSpace);
  inherited;
end;

procedure TIOSFontGlyphManager.LoadResource;
const
  //Rotating matrix to simulate Italic font attribute
  ItalicMatrix: CGAffineTransform = (
    a: 1;
    b: 0;
    c: 0.176326981; //~tan(10 degrees)
    d: 1;
    tx: 0;
    ty: 0
  );
var
  NewFontRef: CTFontRef;
  Matrix: PCGAffineTransform;
begin
  Matrix := nil;
  FFontRef := CTFontCreateWithName(GetPostScriptFontName, CurrentSettings.Size * CurrentSettings.Scale, nil);
  try
    if TFontStyle.fsItalic in CurrentSettings.Style then
    begin
      NewFontRef := CTFontCreateCopyWithSymbolicTraits(FFontRef, 0, nil,
        kCTFontItalicTrait, kCTFontItalicTrait);
      if NewFontRef <> nil then
      begin
        CFRelease(FFontRef);
        FFontRef := NewFontRef;
      end
      else
      begin
        Matrix := @ItalicMatrix;
        //Font has no Italic version, applying transform matrix
        NewFontRef := CTFontCreateWithName(GetPostScriptFontName, CurrentSettings.Size * CurrentSettings.Scale,
          @ItalicMatrix);
        if NewFontRef <> nil then
        begin
          CFRelease(FFontRef);
          FFontRef := NewFontRef;
        end;
      end;
    end;
    if TFontStyle.fsBold in CurrentSettings.Style then
    begin
      NewFontRef := CTFontCreateCopyWithSymbolicTraits(FFontRef, 0, Matrix, kCTFontBoldTrait, kCTFontBoldTrait);
      if NewFontRef <> nil then
      begin
        CFRelease(FFontRef);
        FFontRef := NewFontRef;
      end;
    end;
    //
    GetDefaultBaseline;
  except
    CFRelease(FFontRef);
  end;
end;

procedure TIOSFontGlyphManager.FreeResource;
begin
  if FFontRef <> nil then
    CFRelease(FFontRef);
end;

procedure TIOSFontGlyphManager.GetDefaultBaseline;
var
  Chars: string;
  Str: CFStringRef;
  Frame: CTFrameRef;
  Attr: CFMutableAttributedStringRef;
  Path: CGMutablePathRef;
  Bounds: CGRect;
  FrameSetter: CTFramesetterRef;
  // Metrics
  Line: CTLineRef;
  Lines: CFArrayRef;
  Runs: CFArrayRef;
  Run: CTRunRef;
  Ascent, Descent, Leading: CGFloat;
  BaseLinePos: CGPoint;
begin
  Path := CGPathCreateMutable();
  Bounds := CGRectMake(0, 0, BoundsLimit, BoundsLimit);
  CGPathAddRect(Path, nil, Bounds);
  Chars := 'a';
  Str := CFStringCreateWithCharacters(kCFAllocatorDefault, PChar(Chars), 1);

  Attr := CFAttributedStringCreateMutable(kCFAllocatorDefault, 0);
  CFAttributedStringReplaceString(Attr, CFRangeMake(0, 0), Str);

  CFAttributedStringBeginEditing(Attr);
  try
    // Font
    if FFontRef <> nil then
      CFAttributedStringSetAttribute(Attr, CFRangeMake(0, 1), kCTFontAttributeName, FFontRef);
  finally
    CFAttributedStringEndEditing(Attr);
  end;

  FrameSetter := CTFramesetterCreateWithAttributedString(CFAttributedStringRef(Attr));
  CFRelease(Attr);

  Frame := CTFramesetterCreateFrame(FrameSetter, CFRangeMake(0, 0), Path, nil);
  CFRelease(FrameSetter);
  CFRelease(Str);

  // Metrics
  Lines := CTFrameGetLines(Frame);
  Line := CTLineRef(CFArrayGetValueAtIndex(Lines, 0));
  Runs := CTLineGetGlyphRuns(Line);
  Run := CFArrayGetValueAtIndex(Runs, 0);
  CTRunGetTypographicBounds(Run, CFRangeMake(0, 1), @Ascent,  @Descent, @Leading);

  CTFrameGetLineOrigins(Frame, CFRangeMake(0, 0), @BaseLinePos);
  FDefaultBaseline := BoundsLimit - BaseLinePos.y;

  FDefaultVerticalAdvance := FDefaultBaseline + Descent;

  CFRelease(Frame);
  CFRelease(Path);
end;

function TIOSFontGlyphManager.GetPostScriptFontName: CFStringRef;
var
  LUIFont: UIFont;
  LocalObject: ILocalObject;
begin
  Result := nil;
  LUIFont := TUIFont.Wrap(TUIFont.OCClass.fontWithName(StrToNSStr(CurrentSettings.Family), CurrentSettings.Size * CurrentSettings.Scale));
  if Supports(LUIFont, ILocalObject, LocalObject) then
    Result := CTFontCopyPostScriptName(LocalObject.GetObjectID);
  if Result = nil then
    //In case there is no direct name for the requested font returns source name and let CoreText to select appropriate font
    Result := CFSTR(CurrentSettings.Family);
end;

procedure PathApplierFunction(info: Pointer; const element: PCGPathElement); cdecl;
var
  P, P1, P2: PCGPoint;
begin
  P := element^.points;
  case element.type_ of
    kCGPathElementMoveToPoint:
      TPathData(info).MoveTo(TPointF.Create(P.x, P.y));
    kCGPathElementAddLineToPoint:
      TPathData(info).LineTo(TPointF.Create(P.x, P.y));
    kCGPathElementAddQuadCurveToPoint:
      begin
        P1 := P;
        Inc(P1);
        TPathData(info).QuadCurveTo(TPointF.Create(P.x, P.y), TPointF.Create(P1.x, P1.y));
      end;
    kCGPathElementAddCurveToPoint:
      begin
        P1 := P;
        Inc(P1);
        P2 := P1;
        Inc(P2);
        TPathData(info).CurveTo(TPointF.Create(P.x, P.y), TPointF.Create(P1.x, P1.y), TPointF.Create(P2.x, P2.y));
      end;
    kCGPathElementCloseSubpath:
      TPathData(info).ClosePath;
  end;
end;

function TIOSFontGlyphManager.DoGetBaseline: Single;
begin
  Result := FDefaultBaseline;
end;

function TIOSFontGlyphManager.DoGetGlyph(const Char: UCS4Char; const Settings: TFontGlyphSettings;
  const UseColorfulPalette: Boolean): TFontGlyph;
var
  CharsString: string;
  CharsStringLength: Integer;
  Str: CFStringRef;
  Frame: CTFrameRef;
  Attr: CFMutableAttributedStringRef;
  Path: CGMutablePathRef;
  Bounds: CGRect;
  Rgba: array [0..3] of CGFloat;
  TextColor: CGColorRef;
  FrameSetter: CTFramesetterRef;
  Context: CGContextRef;
  I, J: Integer;
  Color: TAlphaColorRec;
  C: Byte;
  GlyphRect: TRect;
  // Metrics
  Line: CTLineRef;
  Lines: CFArrayRef;
  Runs: CFArrayRef;
  Run: CTRunRef;
  Ascent, Descent, Leading: CGFloat;
  Size: CGSize;
  GlyphStyle: TFontGlyphStyles;
  BaseLinePos: CGPoint;
  BaseLineOffset: Single;
  //
  RunGlyphCount: CFIndex;
  glyph: CGGlyph;
  glyphMatrix: CGAffineTransform;
  position:  CGPoint;
  glyphPath: CGPathRef;
  M: TMatrix;
  Bits: PAlphaColorRecArray;
  ContextSize: TSize;
begin
  Path := CGPathCreateMutable();
  Bounds := CGRectMake(0, 0, BoundsLimit, BoundsLimit);
  CGPathAddRect(Path, nil, Bounds);
  CharsString := System.Char.ConvertFromUtf32(Char);
  CharsStringLength := CharsString.Length * 2;
  Str := CFStringCreateWithCharacters(kCFAllocatorDefault, PChar(CharsString), CharsStringLength);

  Attr := CFAttributedStringCreateMutable(kCFAllocatorDefault, 0);
  CFAttributedStringReplaceString(Attr, CFRangeMake(0, 0), Str);

  CFAttributedStringBeginEditing(Attr);
  try
    // Font
    if FFontRef <> nil then
      CFAttributedStringSetAttribute(Attr, CFRangeMake(0, CharsStringLength), kCTFontAttributeName, FFontRef);
    // Color
    Rgba[0] := 1;
    Rgba[1] := 1;
    Rgba[2] := 1;
    Rgba[3] := 1;
    TextColor := CGColorCreate(FColorSpace, @Rgba[0]);
    try
      CFAttributedStringSetAttribute(Attr, CFRangeMake(0, CharsStringLength), kCTForegroundColorAttributeName, TextColor);
    finally
      CFRelease(TextColor);
    end;
  finally
    CFAttributedStringEndEditing(Attr);
  end;

  FrameSetter := CTFramesetterCreateWithAttributedString(CFAttributedStringRef(Attr));
  CFRelease(Attr);

  Frame := CTFramesetterCreateFrame(FrameSetter, CFRangeMake(0, 0), Path, nil);
  CFRelease(FrameSetter);
  CFRelease(Str);

  // Metrics
  Context := CGBitmapContextCreate(nil, 1, 1, 8, 4, FColorSpace, kCGImageAlphaPremultipliedLast);
  try
    Lines := CTFrameGetLines(Frame);

    Line := CTLineRef(CFArrayGetValueAtIndex(Lines, 0));
    Runs := CTLineGetGlyphRuns(Line);

    Run := CFArrayGetValueAtIndex(Runs, 0);

    Bounds := CTRunGetImageBounds(Run, Context, CFRangeMake(0, 1));
    CTRunGetAdvances(Run, CFRangeMake(0, 1), @Size);
    CTRunGetTypographicBounds(Run, CFRangeMake(0, 1), @Ascent,  @Descent, @Leading);

    GlyphRect := Rect(Trunc(Bounds.origin.x),
      Max(Trunc(Ascent - Bounds.origin.y - Bounds.size.height) - 1, 0),
      Ceil(Bounds.origin.x + Bounds.size.width),
      Round(Ascent + Descent + Descent));

    CTFrameGetLineOrigins(Frame, CFRangeMake(0, 0), @BaseLinePos);
    BaseLineOffset := BoundsLimit - BaseLinePos.y;

    GlyphStyle := [];
    if ((Bounds.size.width = 0) and (Bounds.size.height = 0)) or not HasGlyph(Char) then
      GlyphStyle := [TFontGlyphStyle.NoGlyph];
    if TFontGlyphSetting.Path in Settings then
      GlyphStyle := GlyphStyle + [TFontGlyphStyle.HasPath];
    if UseColorfulPalette then
      GlyphStyle := GlyphStyle + [TFontGlyphStyle.ColorGlyph];
  finally
    CGContextRelease(Context);
  end;

  Result := TFontGlyph.Create(Point(GlyphRect.Left, GlyphRect.Top), Size.width,
    Round(FDefaultVerticalAdvance), GlyphStyle);
  if (TFontGlyphSetting.Bitmap in Settings) and
     (HasGlyph(Char) or ((Bounds.size.width > 0) and (Bounds.size.height > 0))) then
  begin
    ContextSize := TSize.Create(Max(GlyphRect.Right, GlyphRect.Width), GlyphRect.Bottom);

    Context := CGBitmapContextCreate(nil, ContextSize.Width, ContextSize.Height, 8, ContextSize.Width * 4, FColorSpace,
      kCGImageAlphaPremultipliedLast);
    try
      Bits := PAlphaColorRecArray(CGBitmapContextGetData(Context));

      if GlyphRect.Left < 0 then
        CGContextTranslateCTM(Context, -GlyphRect.Left, 0);
      CGContextTranslateCTM(Context, 0, -(BoundsLimit - ContextSize.Height));
      if not SameValue(FDefaultBaseline - BaseLineOffset, 0, TEpsilon.Position) then
        CGContextTranslateCTM(Context, 0, -Abs(FDefaultBaseline - BaseLineOffset));
      CTFrameDraw(Frame, Context);

      Result.Bitmap.SetSize(GlyphRect.Width, GlyphRect.Height, TPixelFormat.BGRA);

      if TFontGlyphSetting.PremultipliedAlpha in Settings then
      begin
        for I := GlyphRect.Top to GlyphRect.Bottom - 1 do
          Move(Bits^[I * ContextSize.Width + Max(GlyphRect.Left, 0)],
            Result.Bitmap.GetPixelAddr(0, I - GlyphRect.Top)^, Result.Bitmap.Pitch);
      end
      else
        for I := GlyphRect.Left to GlyphRect.Right - 1 do
          for J := GlyphRect.Top to GlyphRect.Bottom - 1 do
          begin
            Color := Bits[J * ContextSize.Width + Max(I, 0)];
            if Color.R > 0 then
            begin
              C := (Color.R + Color.G + Color.B) div 3;
              Result.Bitmap.Pixels[I - GlyphRect.Left, J - GlyphRect.Top] := MakeColor($FF, $FF, $FF, C);
            end
          end;
    finally
      CGContextRelease(Context);
    end;
  end;
  //Path
  if TFontGlyphSetting.Path in Settings then
  begin
    RunGlyphCount := CTRunGetGlyphCount(Run);
    for I := 0 to RunGlyphCount - 1 do
    begin
      CTRunGetGlyphs(Run, CFRangeMake(I, 1), @glyph);
      CTRunGetPositions(run, CFRangeMake(I, 1), @position);
                                                                                    
      glyphMatrix := CGAffineTransformTranslate(CGAffineTransformIdentity,
        position.x, position.y);
      glyphPath := CTFontCreatePathForGlyph(FFontRef, glyph, @glyphMatrix);
      if glyphPath <> nil then
      begin
        CGPathApply(glyphPath, Result.Path, @PathApplierFunction);
        CFRelease(glyphPath);
      end;
    end;
    M := TMatrix.Identity;
    M.m22 := -1;
    Result.Path.ApplyMatrix(M);
  end;
  CFRelease(Frame);
  CFRelease(Path);
end;

function TIOSFontGlyphManager.IsColorfulCharacter(const Char: UCS4Char): Boolean;
begin
  Result := inherited or
    (Char = $1F004) or
    (Char = $1F0CF) or
    (Char = $1F170) or
    (Char = $1F171) or
    (Char = $1F17E) or
    (Char = $1F17F) or
    (Char = $1F18F) or
    ((Char >= $1F191) and (Char <= $1F19A)) or
    (Char = $1F201) or
    (Char = $1F202) or
    (Char = $1F21A) or
    (Char = $1F22F) or
    ((Char >= $1F232) and (Char <= $1F23A)) or
    (Char = $1F250) or
    (Char = $1F251) or
    ((Char >= $1F300) and (Char <= $1F320)) or
    ((Char >= $1F330) and (Char <= $1F393)) or
    ((Char >= $1F3A0) and (Char <= $1F3F0)) or
    ((Char >= $1F400) and (Char <= $1F43E)) or
    (Char = $1F440) or
    ((Char >= $1F442) and (Char <= $1F49F)) or
    ((Char >= $1F4A0) and (Char <= $1F4FC)) or
    ((Char >= $1F500) and (Char <= $1F53D)) or
    ((Char >= $1F550) and (Char <= $1F567)) or
    ((Char >= $1F5FB) and (Char <= $1F64F)) or
    ((Char >= $1F680) and (Char <= $1F6C5)) or
    (Char = $2139) or
    ((Char >= $2194) and (Char <= $2199)) or
    (Char = $21A9) or
    (Char = $21AA) or
    (Char = $231A) or
    (Char = $231B) or
    ((Char >= $23E9) and (Char <= $23EC)) or
    (Char = $23F0) or
    (Char = $23F3) or
    (Char = $24C2) or
    (Char = $25AA) or
    (Char = $25AB) or
    (Char = $25B6) or
    (Char = $25C0) or
    ((Char >= $25FB) and (Char <= $25FE)) or
    (Char = $2600) or
    (Char = $2601) or
    (Char = $260E) or
    (Char = $2611) or
    (Char = $2614) or
    (Char = $2615) or
    (Char = $261D) or
    (Char = $263A) or
    ((Char >= $2648) and (Char <= $2653)) or
    (Char = $2660) or
    (Char = $2663) or
    (Char = $2665) or
    (Char = $2666) or
    (Char = $2668) or
    (Char = $267B) or
    (Char = $267F) or
    (Char = $2693) or
    (Char = $26A0) or
    (Char = $26A1) or
    (Char = $26AA) or
    (Char = $26AB) or
    (Char = $26BD) or
    (Char = $26BE) or
    (Char = $26C4) or
    (Char = $26C5) or
    (Char = $26CE) or
    (Char = $26D4) or
    (Char = $26EA) or
    (Char = $26F2) or
    (Char = $26F3) or
    (Char = $26F5) or
    (Char = $26FA) or
    (Char = $26FD) or
    (Char = $2702) or
    (Char = $2705) or
    ((Char >= $2708) and (Char <= $270F)) or
    (Char = $2712) or
    (Char = $2714) or
    (Char = $2716) or
    (Char = $2728) or
    (Char = $2733) or
    (Char = $2734) or
    (Char = $2744) or
    (Char = $2747) or
    (Char = $274C) or
    (Char = $274E) or
    ((Char >= $2753) and (Char <= $2755)) or
    (Char = $2757) or
    (Char = $2764) or
    ((Char >= $2795) and (Char = $2797)) or
    (Char = $27B0) or
    (Char = $27BF) or
    (Char = $2934) or
    (Char = $2935) or
    ((Char >= $2B05) and (Char = $2B07)) or
    (Char = $2B1B) or
    (Char = $2B1C) or
    (Char = $2B50) or
    (Char = $2B55) or
    (Char = $3030) or
    (Char = $303D) or
    (Char = $3297) or
    (Char = $3299);
end;

end.
