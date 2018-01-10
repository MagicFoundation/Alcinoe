{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2012-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.TextLayout.GPU;

{$MINENUMSIZE 4}

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.UITypes, System.Generics.Collections, FMX.Types, FMX.TextLayout, FMX.FontGlyphs, FMX.Graphics;

type
  PCharRec = ^TCharRec;
  TCharRec = record
    Glyph: TFontGlyph;
    SrcRect: TRectF;
    Bitmap: TBitmap;
    BitmapRef: Boolean;
  end;

  TCharDic = class(TDictionary<UCS4Char, PCharRec>)
  private
    FBaseline: Single;
    procedure CharRecNotifyHandler(Sender: TObject; const Value: PCharRec; Action:
      System.Generics.Collections.TCollectionNotification);
  public
    constructor Create(ACapacity: Integer = 0);
    property Baseline: Single read FBaseline write FBaseline;
  end;

  TFamilyDic = class(TDictionary<Int64, TCharDic>)
  private
    procedure CharDicNotifyHandler(Sender: TObject; const Value: TCharDic;
      Action: System.Generics.Collections.TCollectionNotification);
  public
    constructor Create;
  end;

(*
  --------GPUFrame-------------
  |(GPURun)(GPURun)...(GPURun)| <- GPULine (several GPURun's with different font and/or color)
  |(GPURun)                   | <- GPULine (no additional styling, so only a single GPURun)
  |(GPURun)                   | <- GPULine
  |                           | ...
  |                           |
  |                           |
  -----------------------------
*)

  TGPURun = class
  private
    FChars: TList<UCS4Char>;
    FStartIndex: Integer;
    FLength: Integer;
    FImageRect: TRectF;
    FColor: TAlphaColor;
    FFont: TFont;
    FIsDefaultColor: Boolean;
    FTrimmed: Boolean;
    FClipBounds: TList<TRectF>;
    FClipped: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetColor(AColor: TAlphaColor; IsDefault: Boolean);
    procedure Clip;
    procedure SetText(const AText: string; const AStartIndex, ALength: Integer);
    procedure DeleteTextFromStart(ACount: Integer);
    procedure DeleteTextFromEnd(ACount: Integer);
    //
    property Chars: TList<UCS4Char> read FChars;
    property StartIndex: Integer read FStartIndex;
    property Length: Integer read FLength;
    property ImageRect: TRectF read FImageRect write FImageRect;
    property Color: TAlphaColor read FColor;
    property IsDefaultColor: Boolean read FIsDefaultColor;
    property Font: TFont read FFont write FFont;
    property IsTrimmed: Boolean read FTrimmed write FTrimmed;
    property IsClipped: Boolean read FClipped;
    property ClipBounds: TList<TRectF> read FClipBounds;
  end;

  TGPULine = class(TList<TGPURun>)
  private
    FHeight: Single;
    FWidth: Single;
    FTopLeft: TPointF;
  public
    constructor Create;
    destructor Destroy; override;
    //
    property Height: Single read FHeight write FHeight;
    property Width: Single read FWidth write FWidth;
    property TopLeft: TPointF read FTopLeft write FTopLeft;
  end;

  TGPUFrame = class(TList<TGPULine>)
  private
    FHeight: Single;
    FWidth: Single;
    FTopLeft: TPointF;
  public
    constructor Create;
    destructor Destroy; override;
    //
    property TopLeft: TPointF read FTopLeft write FTopLeft;
    property Height: Single read FHeight write FHeight;
    property Width: Single read FWidth write FWidth;
  end;

  TTextLayoutNG = class(TTextLayout)
  public const
    AntialiasMargin = 1;
    MaxUsefulCharMapOccupancy = 0.95; // At 5% of free space or lower the charmap is considered full.
  public type
    TCharMap = record
      Texture: TBitmap;
      BinPack: TGuillotineBinPack;
    end;
    TCharMaps = TList<TCharMap>;
  private class var
    FFamilyDic: TFamilyDic;
    FCharMaps: TCharMaps;
    FRendering: Integer;
    FNewGlyphList: TList<PCharRec>;
    FDisableGlyphPopulation: Boolean;
  private
    FOldColor: TAlphaColor;
    FScale: Single;
    FScaleFactor: Single;
    FFrame: TGPUFrame;
    FFrame3D: TGPUFrame;
    FEllipsisChar: UCS4Char;
    FFontKey: Int64;
    FStrokeBrush: TStrokeBrush;
    procedure CharMapNotify(Sender: TObject; const Item: TCharMap; Action: System.Generics.Collections.TCollectionNotification);
    function CreateFrame: TGPUFrame;
    procedure ApplyAttributes(AFrame: TGPUFrame);
    function MeasureRange(APos, ALength: Integer): TRegion;
    function GetCharDictionary(const AFont: TFont = nil): TCharDic;
    procedure UpdateCharRec(const ACanvas: TCanvas; NeedBitmap: Boolean; var NewRec: PCharRec; HasItem: Boolean;
      const CharDic: TCharDic; const AFont: TFont; const Ch: UCS4Char; const NeedPath: Boolean = False);
    function AddOrGetChar(const ACanvas: TCanvas; const Ch: UCS4Char; const CharDic: TCharDic;
      const AFont: TFont; const NeedPath: Boolean = False): PCharRec;
    class procedure MapGlyphToCache(CharRec: PCharRec);
  protected
    procedure DoRenderLayout; override;
    procedure DoDrawLayout(const ACanvas: TCanvas); override;
    function GetTextHeight: Single; override;
    function GetTextWidth: Single; override;
    function GetTextRect: TRectF; override;
    function DoPositionAtPoint(const APoint: TPointF): Integer; override;
    function DoRegionForRange(const ARange: TTextRange): TRegion; override;
  public
    constructor Create(const ACanvas: TCanvas = nil); override;
    destructor Destroy; override;
    class procedure Uninitialize;
    class procedure BeginRender;
    class procedure EndRender;
    class property CharMaps: TCharMaps read FCharMaps;
    class property DisableGlyphPopulation: Boolean read FDisableGlyphPopulation write FDisableGlyphPopulation;
    //
    procedure ConvertToPath(const APath: TPathData); override;
  end;

implementation

uses
  System.Classes, System.Math, System.SysUtils, System.Character, System.Math.Vectors, FMX.Consts, FMX.Platform,
  FMX.Canvas.GPU, FMX.Text;

const
  BitmapSize = 1024;

function FontStyleToInt(const AStyle: TFontStyles): Cardinal;
begin
  Result := $F00000;
  if TFontStyle.fsBold in AStyle then
    Result := Result + $10000;
  if TFontStyle.fsItalic in AStyle then
    Result := Result + $20000;
end;

function FontFamilyToInt(const AFamily: string): Int64;
var
  I: Integer;
begin
  Result := AFamily.Length;
  for I := 0 to AFamily.Length - 1 do
    Result := Result + Ord(AFamily.Chars[I]);
end;

function FontFamilyKey(const AFont: TFont; const AScale: Single): Int64;
begin
  if SameValue(AScale, 1.0, Epsilon) then
    Result := $FF000000 + FontFamilyToInt(AFont.Family) + FontStyleToInt(AFont.Style) + (1000 * Trunc(AFont.Size))
  else
    Result := $0F000000 + FontFamilyToInt(AFont.Family) + FontStyleToInt(AFont.Style) + (1000 * Trunc(AFont.Size));
end;

function IsCombiningCharacter(const Ch: Char): Boolean;
begin
  Result := Ch.GetUnicodeCategory in [TUnicodeCategory.ucCombiningMark, TUnicodeCategory.ucEnclosingMark, TUnicodeCategory.ucNonSpacingMark]
end;

{ TCharDic }

procedure TCharDic.CharRecNotifyHandler(Sender: TObject; const Value: PCharRec; Action: System.Generics.Collections.TCollectionNotification);
begin
  if Action = cnRemoved then
  begin
    FreeAndNil(Value.Glyph);
    if not Value.BitmapRef then
      FreeAndNil(Value.Bitmap);
    Dispose(Value);
  end;
end;

constructor TCharDic.Create(ACapacity: Integer);
begin
  inherited Create(ACapacity);
  OnValueNotify := CharRecNotifyHandler;
end;

{ TFamilyDic }

procedure TFamilyDic.CharDicNotifyHandler(Sender: TObject; const Value: TCharDic;
  Action: System.Generics.Collections.TCollectionNotification);
begin
  if Action = cnRemoved then
    Value.DisposeOf;
end;

constructor TFamilyDic.Create;
begin
  inherited Create;
  OnValueNotify := CharDicNotifyHandler;
end;

{ TNGRun }

procedure TGPURun.Clip;
var
  I: Integer;
begin
  if not FClipped then
  begin
    FClipped := True;
    FClipBounds := TList<TRectF>.Create;
    for I := 0 to FChars.Count - 1 do
      FClipBounds.Add(TRectF.Empty);
  end;
end;

constructor TGPURun.Create;
begin
  FChars := TList<UCS4Char>.Create;
  FIsDefaultColor := True;
  FClipBounds := nil;
  FClipped := False;
end;

procedure TGPURun.DeleteTextFromEnd(ACount: Integer);
var
  CharsLength: Integer;
begin
  while ACount > 0 do
  begin
    CharsLength := System.Char.ConvertFromUtf32(FChars.Last).Length;
    Dec(FLength, CharsLength);
    FChars.Delete(FChars.Count - 1);
    FClipBounds.Delete(FClipBounds.Count - 1);
    Dec(ACount);
  end;
end;

procedure TGPURun.DeleteTextFromStart(ACount: Integer);
var
  CharsLength: Integer;
begin
  while ACount > 0 do
  begin
    CharsLength := System.Char.ConvertFromUtf32(FChars[0]).Length;
    Inc(FStartIndex, CharsLength);
    Dec(FLength, CharsLength);
    FChars.Delete(0);
    FClipBounds.Delete(0);
    Dec(ACount);
  end;
end;

destructor TGPURun.Destroy;
begin
  FreeAndNil(FChars);
  FreeAndNil(FClipBounds);
  inherited;
end;

procedure TGPURun.SetColor(AColor: TAlphaColor; IsDefault: Boolean);
begin
  if IsDefault then
    if IsDefaultColor then
    //Just changing value of default color
      FColor := AColor
    else
  else
  begin
    //Overriding default color with attribute color
    FColor := AColor;
    FIsDefaultColor := False;
  end;
end;


procedure TGPURun.SetText(const AText: string; const AStartIndex, ALength: Integer);
var
  I, CharLength: Integer;
begin
  FStartIndex := AStartIndex;
  FLength := ALength;
  FChars.Clear;
  I := 0;
  while I < FLength do
  begin
    FChars.Add(System.Char.ConvertToUtf32(AText, I + FStartIndex, CharLength));
    Inc(I, CharLength);
  end;
end;

{ TNGLine }

constructor TGPULine.Create;
begin
  FHeight := 0;
  FWidth := 0;
  inherited Create;
end;

destructor TGPULine.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].DisposeOf;
  inherited;
end;

{ TNGFrame }

constructor TGPUFrame.Create;
begin
  FHeight := 0;
  FWidth := 0;
  FTopLeft := TPointF.Zero;
  inherited Create;
end;

destructor TGPUFrame.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].DisposeOf;
  inherited;
end;

{ TTextLayoutNG }

constructor TTextLayoutNG.Create(const ACanvas: TCanvas);
var
  ScreenSrv: IFMXScreenService;
begin
  inherited Create(ACanvas);
  if FFamilyDic = nil then
  begin
    FFamilyDic := TFamilyDic.Create;
    FCharMaps := TCharMaps.Create;
    FCharMaps.OnNotify := CharMapNotify;
  end;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenSrv) then
    FScale := ScreenSrv.GetScreenScale
  else
    FScale := 1;
  FScaleFactor := 1 / FScale;
  FEllipsisChar := Word(SChrHorizontalEllipsis);
  FFrame := TGPUFrame.Create;
  FStrokeBrush := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColorRec.Black);
end;

destructor TTextLayoutNG.Destroy;
begin
  FreeAndNil(FFrame);
  FreeAndNil(FFrame3D);
  FStrokeBrush.Free;
  inherited;
end;

class procedure TTextLayoutNG.Uninitialize;
begin
  FreeAndNil(FNewGlyphList);
  FreeAndNil(FCharMaps);
  FreeAndNil(FFamilyDic);
  TFontGlyphManager.UnInitialize;
end;

procedure TTextLayoutNG.CharMapNotify(Sender: TObject; const Item: TCharMap; Action: System.Generics.Collections.TCollectionNotification);
begin
{$IFNDEF AUTOREFCOUNT}
  if Action in [cnRemoved, cnExtracted] then
  begin
    Item.Texture.Free;
    Item.BinPack.Free;
  end;
{$ENDIF}
end;

procedure TTextLayoutNG.ConvertToPath(const APath: TPathData);
var
  CharDic: TCharDic;
  Rec: PCharRec;
  LLine: TGPULine;
  LRun: TGPURun;
  I, J, K: Integer;
  LinePath: TPathData;
  TextR: TRectF;
  LineAdvance, VerticalAdvance, LineVerticalAdvance: Single;
  BaselineMaxValue, BaselineOffset: Single;
  CharPath: TPathData;
begin
  if Text.IsEmpty then
    Exit;

  if FFrame3D = nil then
  begin
    FFrame3D := CreateFrame;
    ApplyAttributes(FFrame3D);
  end;

  BaselineOffset := 0;
  BaselineMaxValue := GetCharDictionary(Font).Baseline;
  VerticalAdvance := 0;
  for I := 0 to FFrame3D.Count - 1 do
  try
    LinePath := TPathData.Create;
    LLine := FFrame3D[I];
    LineAdvance := 0;
    LineVerticalAdvance := 0;
    if AttributesCount > 0 then
    begin
      for J := 0 to LLine.Count - 1 do
      begin
        CharDic := GetCharDictionary(LLine[J].Font);
        BaselineMaxValue := Max(BaselineMaxValue, CharDic.Baseline);
      end;
    end;
    CharPath := TPathData.Create;
    try
      for J := 0 to LLine.Count - 1 do
      begin
        LRun := LLine[J];
        CharDic := GetCharDictionary(LRun.Font);
        if AttributesCount > 0 then
          BaselineOffset := (BaselineMaxValue - CharDic.Baseline) * FScaleFactor;
        for K := 0 to LRun.Chars.Count - 1 do
        begin
          Rec := AddOrGetChar(nil, LRun.Chars[K], CharDic, LRun.Font, True);
          if (Rec.Glyph <> nil) and (Rec.Glyph.Path <> nil) then
          begin
            if not SameValue(BaselineOffset, 0, TEpsilon.FontSize) then
            begin
              CharPath.Assign(Rec.Glyph.Path);
              CharPath.Translate(0, BaselineOffset);
              LinePath.AddPath(CharPath);
            end
            else
              LinePath.AddPath(Rec.Glyph.Path);
          end;
          LinePath.Translate(-Rec.Glyph.Advance, 0);
          LineAdvance := LineAdvance + Rec.Glyph.Advance;
          LineVerticalAdvance := Max(LineVerticalAdvance, Rec.Glyph.VerticalAdvance);
        end;
      end;
    finally
      CharPath.Free;
    end;
    LinePath.Translate(LineAdvance, 0);
    //Aligning line
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
    LinePath.Translate(TextR.Left, VerticalAdvance);
    VerticalAdvance := VerticalAdvance + LineVerticalAdvance;
    //
    APath.AddPath(LinePath);
  finally
    FreeAndNil(LinePath);
  end;
  //
  TextR := APath.GetBounds;
  APath.Translate(0, -TextR.Top);
  case VerticalAlign of
    TTextAlign.Center:
      APath.Translate(0, (MaxSize.Y - Padding.Top - Padding.Bottom - TextR.Height) / 2);
    TTextAlign.Leading:;
    TTextAlign.Trailing:
      APath.Translate(0, (MaxSize.Y - Padding.Top - Padding.Bottom - TextR.Height));
  end;
  APath.Translate(0, TopLeft.Y);
end;

procedure TTextLayoutNG.UpdateCharRec(const ACanvas: TCanvas; NeedBitmap: Boolean; var NewRec: PCharRec;
  HasItem: Boolean; const CharDic: TCharDic; const AFont: TFont; const Ch: UCS4Char; const NeedPath: Boolean = False);
var
  Map: TBitmapData;
  J: Integer;
  Bitmap: TBitmap;
  LFont: TFont;
  GlyphSettings: TFontGlyphSettings;
begin
  if not HasItem then
    New(NewRec)
  else
  begin
    FreeAndNil(NewRec.Glyph);
    if not NewRec.BitmapRef then
      FreeAndNil(NewRec.Bitmap);
  end;

  if AFont = nil then
    LFont := Self.Font
  else
    LFont := AFont;
  GlyphSettings := [];
  if NeedBitmap then
    GlyphSettings := [TFontGlyphSetting.Bitmap, TFontGlyphSetting.PremultipliedAlpha];
  if NeedPath then
    GlyphSettings := GlyphSettings + [TFontGlyphSetting.Path];

  NewRec.Glyph := TFontGlyphManager.Current.GetGlyph(Ch, LFont, FScale, GlyphSettings);
  CharDic.Baseline := TFontGlyphManager.Current.GetBaseline(LFont, FScale);

  if not (TFontGlyphStyle.NoGlyph in NewRec.Glyph.Style) and (NewRec.Glyph.Bitmap <> nil) and
    (NewRec.Glyph.Bitmap.Width > 0) and (NewRec.Glyph.Bitmap.Height > 0) then
  begin
    if FRendering > 0 then
    begin
      Bitmap := TBitmap.Create(NewRec.Glyph.Bitmap.Width + AntialiasMargin * 2, NewRec.Glyph.Bitmap.Height +
        AntialiasMargin * 2);
      Bitmap.BitmapScale := FScale;

      if Bitmap.Map(TMapAccess.Write, Map) then
      try
        FillChar(Map.Data^, Map.Pitch * Map.Height, 0);

        NewRec.Bitmap := Bitmap;
        NewRec.BitmapRef := False;
        NewRec.SrcRect := TRectF.Create(0, 0, NewRec.Glyph.Bitmap.Width, NewRec.Glyph.Bitmap.Height);
        NewRec.SrcRect.Offset(AntialiasMargin, AntialiasMargin);

        for J := 0 to NewRec.Glyph.Bitmap.Height - 1 do
          Move(NewRec.Glyph.Bitmap.Scanline[J]^, Map.GetPixelAddr(AntialiasMargin, J + AntialiasMargin)^,
            NewRec.Glyph.Bitmap.Pitch);
      finally
        Bitmap.Unmap(Map);
      end;
      if FNewGlyphList = nil then
        FNewGlyphList := TList<PCharRec>.Create;
      FNewGlyphList.Add(NewRec);
    end
    else
      MapGlyphToCache(NewRec);
  end
  else
  begin
    NewRec.Bitmap := nil;
    NewRec.SrcRect := TRectF.Empty;
  end;

  if not HasItem then
    CharDic.Add(Ch, NewRec);
end;

class procedure TTextLayoutNG.BeginRender;
begin
  Inc(FRendering);
end;

class procedure TTextLayoutNG.EndRender;
var
  I: Integer;
  Rec: PCharRec;
begin
  Dec(FRendering);
  if (FNewGlyphList <> nil) and (FNewGlyphList.Count > 0) and (FRendering = 0) and not FDisableGlyphPopulation then
  begin
    for I := 0 to FNewGlyphList.Count - 1 do
    begin
      Rec := FNewGlyphList[I];
      if (Rec.Glyph.Bitmap.Width > 0) and (Rec.Glyph.Bitmap.Height > 0) then
        MapGlyphToCache(Rec);
    end;
    FNewGlyphList.Clear;
  end;
end;

function TTextLayoutNG.AddOrGetChar(const ACanvas: TCanvas; const Ch: UCS4Char; const CharDic: TCharDic;
  const AFont: TFont; const NeedPath: Boolean = False): PCharRec;
var
  NeedBitmap, HasItem: Boolean;
begin
  NeedBitmap := ACanvas <> nil;
  // if not exists - place in bitmap and add to dictionaty
  HasItem := CharDic.TryGetValue(Ch, Result);
  if not HasItem or (NeedBitmap and (Result.Bitmap = nil) and not (TFontGlyphStyle.NoGlyph in Result.Glyph.Style)) or
     (NeedPath and not (TFontGlyphStyle.HasPath in Result.Glyph.Style)) then
    UpdateCharRec(ACanvas, NeedBitmap, Result, HasItem, CharDic, AFont, Ch, NeedPath);
end;

procedure TTextLayoutNG.DoDrawLayout(const ACanvas: TCanvas);
var
  CharDic: TCharDic;
  Rec: PCharRec;
  Pos: TPointF;
  R, SrcR, ClipBounds: TRectF;
  LLine: TGPULine;
  LRun: TGPURun;
  I, J, K: Integer;
  VerticalAligned, HorizontalAligned, ColoredGlyph: Boolean;
  Styles: TFontStyles;
  Thickness: Single;
  BaselineMaxValue, BaselineOffset: Single;
begin
  if Text.IsEmpty then
    Exit;

  if FOldColor <> Color then
  begin
    FOldColor := Color;
    for I := 0 to FFrame.Count - 1 do
    begin
      LLine := FFrame[I];
      for J := 0 to LLine.Count - 1 do
      begin
        LRun := LLine[J];
        LRun.SetColor(Color, True);
      end;
    end;
  end;

  if not SameValue(FScale, ACanvas.Scale, Epsilon) then
  begin
    FScale := ACanvas.Scale;
    FScaleFactor := 1 / FScale;
    DoRenderLayout;
  end;

  HorizontalAligned := SameValue(Frac(TopLeft.X), 0.0, TEpsilon.Position) and SameValue(Frac(ACanvas.Matrix.m31), 0.0, TEpsilon.Position);
  VerticalAligned := SameValue(Frac(TopLeft.Y), 0.0, TEpsilon.Position) and SameValue(Frac(ACanvas.Matrix.m32), 0.0, TEpsilon.Position);

  BaselineOffset := 0;
  BaselineMaxValue := GetCharDictionary(Font).Baseline;
  for I := 0 to FFrame.Count - 1 do
  begin
    LLine := FFrame[I];
    Pos := LLine.TopLeft + TopLeft;
    if AttributesCount > 0 then
    begin
      for J := 0 to LLine.Count - 1 do
      begin
        CharDic := GetCharDictionary(LLine[J].Font);
        BaselineMaxValue := Max(BaselineMaxValue, CharDic.Baseline);
      end;
    end;
    for J := 0 to LLine.Count - 1 do
    begin
      LRun := LLine[J];
      if LRun.Font <> nil then
        Styles := LRun.Font.Style
      else
        Styles := Self.Font.Style;
      CharDic := GetCharDictionary(LRun.Font);
      if AttributesCount > 0 then
        BaselineOffset := (BaselineMaxValue - CharDic.Baseline) * FScaleFactor;
      TCustomCanvasGpu(ACanvas).ModulateColor := LRun.Color;
      for K := 0 to LRun.Chars.Count - 1 do
      begin
        Rec := AddOrGetChar(ACanvas, LRun.Chars[K], CharDic, LRun.Font);
        if Rec.Bitmap <> nil then
        begin
          if HorizontalAligned then
            R.Left := ACanvas.AlignToPixelHorizontally(Pos.X) + Rec.Glyph.Origin.X * FScaleFactor
          else
            R.Left := Pos.X + Rec.Glyph.Origin.X * FScaleFactor;
          if VerticalAligned then
            R.Top := ACanvas.AlignToPixelVertically(Pos.Y + BaselineOffset) + Rec.Glyph.Origin.Y * FScaleFactor
          else
            R.Top := Pos.Y + BaselineOffset + Rec.Glyph.Origin.Y * FScaleFactor;

          R.Right := R.Left + (Rec.SrcRect.Width * FScaleFactor);
          R.Bottom := R.Top + (Rec.SrcRect.Height * FScaleFactor);
          SrcR := Rec.SrcRect;
          if LRun.IsClipped then
          begin
            ClipBounds := LRun.ClipBounds[K];
            SrcR.Top := SrcR.Top + ClipBounds.Top * FScale;
            R.Top := R.Top + ClipBounds.Top;
            SrcR.Bottom := SrcR.Bottom - ClipBounds.Bottom * FScale;
            R.Bottom := R.Bottom - ClipBounds.Bottom;
            SrcR.Left := SrcR.Left + ClipBounds.Left * FScale;
            R.Left := R.Left + ClipBounds.Left;
            SrcR.Right := SrcR.Right - ClipBounds.Right * FScale;
            R.Right := R.Right - ClipBounds.Right;
          end;
          // Draw
          ColoredGlyph := TFontGlyphStyle.ColorGlyph in Rec.Glyph.Style;
          if ColoredGlyph then
            TCustomCanvasGpu(ACanvas).ModulateColor := $FFFFFFFF;
          ACanvas.DrawBitmap(Rec.Bitmap, SrcR, R, Opacity);
          if ColoredGlyph then
            TCustomCanvasGpu(ACanvas).ModulateColor := LRun.Color;
        end;
        // Offset current position
        Pos.X := Pos.X + (Rec.Glyph.Advance * FScaleFactor);
      end;
      if LRun.IsTrimmed then
      begin
        Rec := AddOrGetChar(ACanvas, FEllipsisChar, GetCharDictionary(Self.Font), Self.Font);
        TCustomCanvasGpu(ACanvas).ModulateColor := Self.Color;
        if Rec.Bitmap <> nil then
        begin
          if HorizontalAligned then
            R.Left := ACanvas.AlignToPixelHorizontally(Pos.X) + Rec.Glyph.Origin.X * FScaleFactor
          else
            R.Left := Pos.X + Rec.Glyph.Origin.X * FScaleFactor;
          if VerticalAligned then
            R.Top := ACanvas.AlignToPixelVertically(Pos.Y + BaselineOffset) + Rec.Glyph.Origin.Y * FScaleFactor
          else
            R.Top := Pos.Y + BaselineOffset + Rec.Glyph.Origin.Y * FScaleFactor;
          R.Right := R.Left + (Rec.SrcRect.Width * FScaleFactor);
          R.Bottom := R.Top + (Rec.SrcRect.Height * FScaleFactor);
          // Draw
          ACanvas.DrawBitmap(Rec.Bitmap, Rec.SrcRect, R, Opacity);
        end;
      end;
      if ([TFontStyle.fsStrikeOut, TFontStyle.fsUnderline] * Styles) <> [] then
      begin
        FStrokeBrush.Color := LRun.Color;
        if LRun.Font <> nil then
          Thickness := LRun.Font.Size / 15
        else
          Thickness := Self.Font.Size / 15;
        FStrokeBrush.Thickness := Thickness;
        if TFontStyle.fsStrikeOut in Styles then
          ACanvas.DrawLine(TPointF.Create(Pos.X - LRun.ImageRect.Width, Pos.Y + BaselineOffset + LRun.ImageRect.Height / 2), TPointF.Create(Pos.X, Pos.Y + BaselineOffset + LRun.ImageRect.Height / 2), Opacity, FStrokeBrush);
        if TFontStyle.fsUnderline in Styles then
          ACanvas.DrawLine(TPointF.Create(Pos.X - LRun.ImageRect.Width, Pos.Y + BaselineOffset + CharDic.Baseline * FScaleFactor + 1.5 * Thickness), TPointF.Create(Pos.X, Pos.Y + BaselineOffset + CharDic.Baseline * FScaleFactor + 1.5 * Thickness), Opacity, FStrokeBrush);
      end;
    end;
  end;
  TCustomCanvasGpu(ACanvas).ModulateColor := $FFFFFFFF;
end;

procedure TTextLayoutNG.DoRenderLayout;

  procedure AlignFrame;
  var
    LTop, LLeft: Single;
    I: Integer;
  begin
    LLeft := Padding.Left;
    case HorizontalAlign of
      TTextAlign.Center:
        begin
          LLeft := (MaxSize.X - Padding.Right - Padding.Left - FFrame.Width) / 2;
          for I := 0 to FFrame.Count - 1 do
            FFrame[I].TopLeft := TPointF.Create((MaxSize.X - Padding.Right - Padding.Left - FFrame[I].Width) / 2, 0);
        end;
      TTextAlign.Trailing:
        begin
          LLeft := MaxSize.X - Padding.Right - FFrame.Width;
          for I := 0 to FFrame.Count - 1 do
            FFrame[I].TopLeft := TPointF.Create(MaxSize.X - Padding.Right - FFrame[I].Width, 0);
        end;
    end;
    LTop := Padding.Top;
    case VerticalAlign of
      TTextAlign.Center:
        LTop := (MaxSize.Y - Padding.Top - Padding.Bottom - FFrame.Height) / 2;
      TTextAlign.Trailing:
        LTop := MaxSize.Y - Padding.Bottom - FFrame.Height;
    end;
    FFrame.TopLeft := TPointF.Create(LLeft, LTop);
    for I := 0 to FFrame.Count - 1 do
    begin
      FFrame[I].TopLeft := TPointF.Create(FFrame[I].TopLeft.X, LTop);
        LTop := LTop + FFrame[I].Height;
    end;
  end;

  procedure CheckClipping;
  var
    I, J, K: Integer;
    X: Single;
    Run: TGPURun;
    Rec: PCharRec;
    ChDic: TCharDic;
    R: TRectF;
  begin
    if (FFrame.Width < MaxSize.X) and (FFrame.Height < MaxSize.Y) then
      Exit;
    //Checking for lines upper than top border
    if VerticalAlign <> TTextAlign.Leading then
      while FFrame.Count > 0 do
        if FFrame[0].TopLeft.Y < 0 then
          if (FFrame[0].TopLeft.Y + FFrame[0].Height) < 0 then //Remove Invisible line
          begin
            FFrame.TopLeft.Offset(0, FFrame[0].Height);
            FFrame.Height := FFrame.Height - FFrame[0].Height;
            FFrame.Delete(0);
          end
          else
          begin //Adding clip rects
            for J := 0 to FFrame[0].Count - 1 do
            begin
              Run := FFrame[0][J];
              if (Run.ImageRect.Height + FFrame[0].TopLeft.Y) > 0 then
              begin
                Run.Clip;
                ChDic := GetCharDictionary(Run.Font);
                for K := 0 to Run.Chars.Count - 1 do
                begin
                  Rec := AddOrGetChar(nil, Run.Chars[K], ChDic, Run.Font);
                  X := Rec.Glyph.Origin.Y * FScaleFactor + FFrame[0].TopLeft.Y;
                  if X < 0 then
                  begin
                    R := Run.ClipBounds[K];
                    R.Top := Abs(X);
                    Run.ClipBounds[K] := R;
                  end;
                end;
              end;
            end;
            Break;
          end
        else
          Break;
    //Checking for lines lower than bottom border
    if VerticalAlign <> TTextAlign.Trailing then
      while FFrame.Count > 0 do
        if (FFrame[FFrame.Count - 1].TopLeft.Y + FFrame[FFrame.Count - 1].Height) > MaxSize.Y then
          if FFrame[FFrame.Count - 1].TopLeft.Y > MaxSize.Y then
          begin
            FFrame.Height := FFrame.Height - FFrame[FFrame.Count - 1].Height;
            FFrame.Delete(FFrame.Count - 1);
          end
          else
          begin
            for J := 0 to FFrame.Last.Count - 1 do
            begin
              Run := FFrame.Last[J];
              if (Run.ImageRect.Height + FFrame.Last.TopLeft.Y) > MaxSize.Y then
              begin
                Run.Clip;
                ChDic := GetCharDictionary(Run.Font);
                for K := 0 to Run.Chars.Count - 1 do
                begin
                  Rec := AddOrGetChar(nil, Run.Chars[K], ChDic, Run.Font);
                  X := MaxSize.Y - FFrame.Last.TopLeft.Y - Rec.Glyph.VerticalAdvance * FScaleFactor;
                  if X < 0 then
                  begin
                    R := Run.ClipBounds[K];
                    R.Bottom := Abs(X);
                    Run.ClipBounds[K] := R;
                  end;
                end;
              end;
            end;
            Break;
          end
        else
          Break;
    //
    for I := 0 to FFrame.Count - 1 do
      if FFrame[I].Width > MaxSize.X then
      begin
        //Checking for characters that are lefter than left border
        if HorizontalAlign <> TTextAlign.Leading then
        begin
          X := FFrame[I].TopLeft.X;
          if X < 0 then
            while FFrame[I].Count > 0 do
              if X < 0 then
              begin
                Run := FFrame[I][0];
                if Run.Length > 0 then
                begin
                  ChDic := GetCharDictionary(Run.Font);
                  while X < 0 do
                  begin
                    Run.Clip;
                    Rec := AddOrGetChar(nil, Run.Chars[0], ChDic, Run.Font);
                    if (X + Rec.Glyph.Advance * FScaleFactor) < 0 then
                    begin
                      Run.DeleteTextFromStart(1);
                      FFrame[I].TopLeft.Offset(Rec.Glyph.Advance * FScaleFactor, 0);
                      FFrame[I].Width := FFrame[I].Width - Rec.Glyph.Advance * FScaleFactor;
                    end
                    else
                    begin
                      R := Run.ClipBounds[0];
                      R.Left := Abs(X);
                      Run.ClipBounds[0] := R;
                    end;
                    X := X + Rec.Glyph.Advance * FScaleFactor;
                  end;
                end;
                if Run.Length = 0 then
                  FFrame[I].Delete(0);
              end
              else
                Break;
        end;
        //Checking for characters that are righter than right border
        if HorizontalAlign <> TTextAlign.Trailing then
        begin
          X := FFrame[I].TopLeft.X;
          J := 0;
          while (X < MaxSize.X) and (J < FFrame[I].Count) do
          begin
            Run := FFrame[I][J];
            ChDic := GetCharDictionary(Run.Font);
            for K := 0 to Run.Chars.Count - 1 do
            begin
              Rec := AddOrGetChar(nil, Run.Chars[K], ChDic, Run.Font);
              X := X + Rec.Glyph.Advance * FScaleFactor;
              if X > MaxSize.X then
              begin
                Run.Clip;
                FFrame[I].Width := X - FFrame[I].TopLeft.X;
                if K < (Run.Chars.Count - 1) then
                  Run.DeleteTextFromEnd(Run.Chars.Count - K - 1);
                R := Run.ClipBounds[K];
                R.Right := X - MaxSize.X;
                Run.ClipBounds[K] := R;
                FFrame[I].DeleteRange(J + 1, FFrame[I].Count - J - 1);
                if Run.Length = 0 then
                  FFrame[I].Delete(J);
                Break;
              end;
            end;
            Inc(J);
          end;
        end;
      end;
  end;

  procedure ReduceFrameSize;
  var
    MaxWidth: Single;
    I: Integer;
  begin
    MaxWidth := 0;
    for I := 0 to FFrame.Count - 1 do
      MaxWidth := Max(MaxWidth, FFrame[I].Width);
    FFrame.Width := MaxWidth;
  end;

var
  CharDic: TCharDic;
  Rec, Rec1: PCharRec;
  R: TRectF;
  LLine, NewLine: TGPULine;
  LRun, NewRun: TGPURun;
  I, LineIndex, RunIndex, CharIndex, RunLength: Integer;
  WidthLimit, LineWidth, LineWidthLimit: Single;
  CurrentPos, RemainLength, RunEndIndex, WordBeginIndex, CharLength: Integer;
begin
  FOldColor := Self.Color;
  FreeAndNil(FFrame);
  FreeAndNil(FFrame3D);

  if LayoutCanvas <> nil then
    if not SameValue(FScale, LayoutCanvas.Scale, Epsilon) then
    begin
      FScale := LayoutCanvas.Scale;
      FScaleFactor := 1 / FScale;
    end;

  FFontKey := FontFamilyKey(Font, FScale);

  //Splitting text
  FFrame := CreateFrame;
  //Applying attributes
  ApplyAttributes(FFrame);

  //Calculation metrics
  WidthLimit := MaxSize.X - Padding.Left - Padding.Right;
  LineIndex := 0;
  while LineIndex < FFrame.Count do
  begin
    LLine := FFrame[LineIndex];
    LLine.Width := 0;
    LLine.Height := 0;
    RunIndex := 0;
    while RunIndex  < LLine.Count do
    begin
      LRun := LLine[RunIndex];
      CharDic := GetCharDictionary(LRun.Font);
      if LRun.Length = 0 then
      begin
        Rec := AddOrGetChar(nil, System.Char.ConvertToUtf32('|', 0), CharDic, LRun.Font);
        //
        LLine.Width := 1;
        LLine.Height := Rec.Glyph.VerticalAdvance * FScaleFactor;
        //
        LRun.ImageRect := TRectF.Create(0, 0, LLine.Width, LLine.Height);
      end
      else
        LRun.ImageRect := TRectF.Create(0, 0, 0, 0);
        RemainLength := LRun.StartIndex + LRun.Length;
        CharIndex := LRun.StartIndex;
        while CharIndex < RemainLength do
        begin
          Rec := AddOrGetChar(nil, System.Char.ConvertToUtf32(Text, CharIndex, CharLength), CharDic, LRun.Font);
          //Checking for MaxSize exceeding
          if (WidthLimit > 0) and (LRun.ImageRect.Width > 0) and (WordWrap or (Trimming <> TTextTrimming.None)) and
		     ((LLine.Width + Rec.Glyph.Advance * FScaleFactor) > WidthLimit) then
          begin
            if WordWrap then
            begin
              //Wrapping text to several lines
              if Text.Chars[CharIndex].GetUnicodeCategory <> TUnicodeCategory.ucSpaceSeparator then
              begin
                WordBeginIndex := CharIndex;
                while (WordBeginIndex > LRun.StartIndex) and (Text.Chars[WordBeginIndex - 1].GetUnicodeCategory <> TUnicodeCategory.ucSpaceSeparator) do
                  Dec(WordBeginIndex);
                if Text.Chars[WordBeginIndex].IsLowSurrogate then
                  Dec(WordBeginIndex);
                RunEndIndex := WordBeginIndex;
                while (RunEndIndex > LRun.StartIndex) and (Text.Chars[RunEndIndex - 1].GetUnicodeCategory = TUnicodeCategory.ucSpaceSeparator) do
                  Dec(RunEndIndex);
                if Text.Chars[RunEndIndex].IsLowSurrogate then
                  Dec(RunEndIndex);

                if WordBeginIndex = LLine[0].StartIndex then
                begin
                  CurrentPos := CharIndex;
                  if Text.Chars[CharIndex - 1].IsLowSurrogate then
                    LRun.SetText(Self.Text, LRun.StartIndex, Max((CharIndex - 2) - LRun.StartIndex + 1, 0))
                  else
                    LRun.SetText(Self.Text, LRun.StartIndex, Max((CharIndex - 1) - LRun.StartIndex + 1, 0));
                end
                else
                begin
                  LRun.SetText(Self.Text, LRun.StartIndex, Max(RunEndIndex - LRun.StartIndex, 0));
                  CurrentPos := WordBeginIndex;
                end;
              end
              else
              begin
                RunEndIndex := CharIndex;
                WordBeginIndex := CharIndex;
                while (RunEndIndex >= LRun.StartIndex) and (Text.Chars[RunEndIndex].GetUnicodeCategory = TUnicodeCategory.ucSpaceSeparator) do
                  Dec(RunEndIndex);
                if Text.Chars[RunEndIndex].IsLowSurrogate then
                  Dec(RunEndIndex);
                while (WordBeginIndex <= (LRun.StartIndex + LRun.Length)) and (Text.Chars[WordBeginIndex].GetUnicodeCategory = TUnicodeCategory.ucSpaceSeparator) do
                  Inc(WordBeginIndex);

                LRun.SetText(Self.Text, LRun.StartIndex, RunEndIndex - LRun.StartIndex + 1);

                CurrentPos := WordBeginIndex;
              end;
            end
            else
            begin
              CurrentPos := CharIndex;
              //Getting back to last visible
              if Trimming <> TTextTrimming.None then
              begin
                Rec := AddOrGetChar(nil, FEllipsisChar, GetCharDictionary(Self.Font), Self.Font);
                LineWidth := LLine.Width;
                LineWidthLimit := WidthLimit - Rec.Glyph.Advance * FScaleFactor;
                while (CurrentPos >= LRun.StartIndex) and (LineWidth > LineWidthLimit) do
                begin
                  Rec := AddOrGetChar(nil, System.Char.ConvertToUtf32(Text, LRun.StartIndex), CharDic, LRun.Font);
                  LineWidth := LineWidth - Rec.Glyph.Advance * FScaleFactor;
                  Dec(CurrentPos);
                end;
              end;
              //Checking for trimming
              case Trimming of
                TTextTrimming.None:
                  RunLength := CurrentPos - LRun.StartIndex;
                TTextTrimming.Character:
                  if CurrentPos > 0 then
                    if Text.Chars[CurrentPos - 1].IsLetterOrDigit then
                    begin
                      RunLength := CurrentPos - LRun.StartIndex - 1;
                      while (RunLength > 0) and not Text.Chars[LRun.StartIndex + RunLength - 1].IsLetterOrDigit do
                        Dec(RunLength);
                    end
                    else
                      RunLength := GetLexemeEnd(Text, GetPrevLexemeBegin(Text, CurrentPos)) + 1 - LRun.StartIndex
                  else
                    RunLength := 0;
                TTextTrimming.Word:
                  begin
                    RunLength := GetLexemeBegin(Text, CurrentPos) + 1 - LRun.StartIndex;
                    if (LRun.StartIndex + RunLength) = (CurrentPos + 1) then
                      RunLength := GetLexemeEnd(Text, GetPrevLexemeBegin(Text, CurrentPos)) - LRun.StartIndex + 1;
                  end;
              else
                RunLength := LRun.Length;
              end;
              LRun.SetText(Self.Text, LRun.StartIndex, RunLength);
              LRun.IsTrimmed := (Trimming <> TTextTrimming.None);
              CurrentPos := RemainLength;
              if CurrentPos < (LRun.StartIndex + LRun.Length) then
                CurrentPos := LRun.StartIndex + LRun.Length;
            end;
            //Decreasing Run's length
            RunEndIndex := LRun.StartIndex + LRun.Length;
            if CharIndex >= RunEndIndex then
            begin
              I := CharIndex - 1;
              R := LRun.ImageRect;
              while (I >= 0) and (I >= RunEndIndex) do
              begin
                if Text.Chars[I].IsLowSurrogate then
                  Dec(I);
                Rec1 := AddOrGetChar(nil, System.Char.ConvertToUtf32(Text, I), CharDic, LRun.Font);
                LLine.Width := LLine.Width - (Rec1.Glyph.Advance * FScaleFactor);
                R.Width := R.Width - Rec1.Glyph.Advance * FScaleFactor;
                Dec(I);
              end;
              LRun.ImageRect := R;
            end;
            if LRun.IsTrimmed then
            begin
              Rec1 := AddOrGetChar(nil, FEllipsisChar, GetCharDictionary(Self.Font), Self.Font);
              R := LRun.ImageRect;
              R.Width := R.Width + Rec1.Glyph.Advance * FScaleFactor;
              LRun.ImageRect := R;
              LLine.Width := LLine.Width + Rec1.Glyph.Advance * FScaleFactor;
              LLine.Height := Max(LLine.Height, Rec1.Glyph.VerticalAdvance * FScaleFactor);
            end;
            //Applying wrapping
            if WordWrap and (CurrentPos < RemainLength) then
            begin
              //Forming new line
              NewLine := TGPULine.Create;
              NewRun := TGPURun.Create;
              NewRun.Font := LRun.Font;
              NewRun.SetColor(LRun.Color, LRun.IsDefaultColor);
              NewRun.SetText(Self.Text, CurrentPos, RemainLength - CurrentPos);

              NewLine.Add(NewRun);
              for I := RunIndex + 1 to LLine.Count - 1 do
                NewLine.Add(LLine[I]);
              LLine.DeleteRange(RunIndex + 1, LLine.Count - (RunIndex + 1));
              FFrame.Insert(LineIndex + 1, NewLine);
              RunIndex := LLine.Count;
            end;
            Break;
          end
          else
          begin
            R := LRun.ImageRect;
            R.Width := R.Width + Rec.Glyph.Advance * FScaleFactor;
            R.Height := Max(R.Height, Rec.Glyph.VerticalAdvance * FScaleFactor);
            LRun.ImageRect := R;
            LLine.Width := LLine.Width + (Rec.Glyph.Advance * FScaleFactor);
            LLine.Height := Max(LLine.Height, R.Height);
          end;
          Inc(CharIndex, CharLength);
        end;
      Inc(RunIndex);
    end;
    //
    FFrame.Width := Max(FFrame.Width, LLine.Width);
    FFrame.Height := FFrame.Height + LLine.Height;
    Inc(LineIndex);
  end;
  AlignFrame;
  CheckClipping;
  ReduceFrameSize;
end;

function TTextLayoutNG.GetCharDictionary(const AFont: TFont = nil): TCharDic;
var
  FamilyKey: Int64;
begin
  Result := nil;
  if AFont = nil then
    FamilyKey := FFontKey
  else
    FamilyKey := FontFamilyKey(AFont, FScale);

  if not FFamilyDic.TryGetValue(FamilyKey, Result) then
  begin
    Result := TCharDic.Create(1024);
    FFamilyDic.Add(FamilyKey, Result);
  end;
end;

function TTextLayoutNG.GetTextHeight: Single;
begin
  if not SameValue(MaxSize.Y, TTextLayout.MaxLayoutSize.Y, Epsilon) then
    Result := Min(FFrame.Height, MaxSize.Y)
  else
    Result := FFrame.Height;
end;

function TTextLayoutNG.GetTextRect: TRectF;
begin
  Result := TRectF.Create(FFrame.TopLeft, TextWidth, TextHeight);
  Result.Offset(TopLeft);
  if FFrame.TopLeft.Y < 0 then
    Result.Offset(0, Abs(FFrame.TopLeft.Y));
end;

function TTextLayoutNG.GetTextWidth: Single;
begin
  Result := FFrame.Width;
end;

function TTextLayoutNG.CreateFrame: TGPUFrame;
var
  ST: TStringList;
  LLine: TGPULine;
  LRun: TGPURun;
  I, StartIndex: Integer;
begin
  Result :=  TGPUFrame.Create;
  if Text.IsEmpty then
  begin
    LLine := TGPULine.Create;
    LRun := TGPURun.Create;
    LRun.Font := nil;
    LRun.SetColor(Self.Color, True);
    LLine.Add(LRun);
    Result.Add(LLine);
  end
  else begin
    ST := TStringList.Create;
    try
      ST.Text := Self.Text;
      StartIndex := 0;
      for I := 0 to ST.Count - 1 do
      begin
        LLine := TGPULine.Create;
        LRun := TGPURun.Create;
        LRun.Font := nil;
        LRun.SetColor(Self.Color, True);
        StartIndex := Self.Text.IndexOf(ST[I], StartIndex);
        LRun.SetText(Self.Text, StartIndex, ST[I].Length);
        LLine.Add(LRun);
        Result.Add(LLine);
        Inc(StartIndex, LRun.Length);
        if I < (ST.Count - 1) then
          Inc(StartIndex, ST.LineBreak.Length);
      end;
    finally
      FreeAndNil(ST);
    end;
  end;
end;

procedure TTextLayoutNG.ApplyAttributes(AFrame: TGPUFrame);

  procedure ApplyAttribute(Run: TGPURun; Attribute: TTextAttribute);
  begin
    Run.SetColor(Attribute.Color, False);
    if Run.Font = nil then
      Run.Font := Attribute.Font
    else
      if Attribute.Font <> nil then
      begin
        Run.Font.Family := Attribute.Font.Family;
        Run.Font.Size := Attribute.Font.Size;
        Run.Font.Style := Run.Font.Style + Attribute.Font.Style;
      end;
  end;

var
  I, CurrentPos, RemainLength, LineIndex, RunIndex: Integer;
  LAttribute: TTextAttributedRange;
  LLine: TGPULine;
  LRun, NewRun: TGPURun;
begin
  for I := 0 to AttributesCount - 1 do
  begin
    LAttribute := Attributes[I];
    CurrentPos := LAttribute.Range.Pos;
    RemainLength := LAttribute.Range.Length;
    while RemainLength > 0 do
    begin
      //Looking for Run
      for LineIndex := 0 to AFrame.Count - 1 do
      begin
        RunIndex := 0;
        LLine := AFrame[LineIndex];
        while RunIndex < LLine.Count do
        begin
          LRun := LLine[RunIndex];
          if CurrentPos < (LRun.StartIndex + LRun.Length) then
          begin
            if CurrentPos > LRun.StartIndex then
            begin
              //Attibute start's not from the begin of the run, need to
              //split run
              NewRun := TGPURun.Create;
              NewRun.SetText(Text, LRun.StartIndex, CurrentPos - LRun.StartIndex);
              NewRun.Font := LRun.Font;
              NewRun.SetColor(LRun.Color, LRun.IsDefaultColor);
              LLine.Insert(RunIndex, NewRun);
              Inc(RunIndex);
              LRun.SetText(Text, CurrentPos, LRun.Length - NewRun.Length);
            end;
            //Current position and start of the Run are equal
            NewRun := nil;
            if RemainLength < LRun.Length then
            begin
              //Attribute length is less than current Run, need to create
              //a new Run after current
              NewRun := TGPURun.Create;
              NewRun.SetText(Self.Text, LRun.StartIndex + RemainLength, LRun.Length - RemainLength);
              NewRun.Font := LRun.Font;
              NewRun.SetColor(LRun.Color, LRun.IsDefaultColor);
              LRun.SetText(Text, LRun.StartIndex, LRun.Length - NewRun.Length);
            end;
            //Applying attribute
            ApplyAttribute(LRun, LAttribute.Attribute);
            Dec(RemainLength, LRun.Length);
            Inc(CurrentPos, LRun.Length);
            if NewRun <> nil then
            begin
              //Inserting new run after applying attribute
              LLine.Insert(RunIndex + 1, NewRun);
              Inc(RunIndex);
              Inc(CurrentPos, NewRun.Length);
            end;
          end;
          if RemainLength <= 0 then
            Break;
          Inc(RunIndex);
        end;
        if RemainLength <= 0 then
          Break;
      end;
    end;
  end;
end;

class procedure TTextLayoutNG.MapGlyphToCache(CharRec: PCharRec);
var
  GlyphSize: TPoint;
  Map: TBitmapData;
  I, LIndex: Integer;
  LRect: TRect;
  CharMap: TCharMap;
begin
  // CharRec.Bitmap already has AntialiasMargin applied to it.
  if CharRec.Bitmap <> nil then
    GlyphSize := TPoint.Create(CharRec.Bitmap.Width, CharRec.Bitmap.Height)
  else
    GlyphSize := TPoint.Create(CharRec.Glyph.Bitmap.Width + AntialiasMargin * 2, CharRec.Glyph.Bitmap.Height + AntialiasMargin * 2);
  LRect := TRect.Empty;
  LIndex := -1;
  for I := 0 to FCharMaps.Count - 1 do
    if FCharMaps[I].BinPack.Occupancy < MaxUsefulCharMapOccupancy then
    begin
      LRect := FCharMaps[I].BinPack.Insert(GlyphSize, False);
      if not LRect.IsEmpty then
      begin
        LIndex := I;
        Break;
      end;
    end;

  if LIndex = -1 then
  begin
    CharMap.Texture := TBitmap.Create(BitmapSize, BitmapSize);
    if CharRec.Bitmap <> nil then
      CharMap.Texture.BitmapScale := CharRec.Bitmap.BitmapScale;
    CharMap.BinPack := TGuillotineBinPack.Create(TPoint.Create(BitmapSize, BitmapSize));
    FCharMaps.Add(CharMap);
    LRect := CharMap.BinPack.Insert(GlyphSize, False);
    if LRect.IsEmpty then
      Exit;
  end
  else
    CharMap := FCharMaps[LIndex];

  if not CharRec.BitmapRef then
    FreeAndNil(CharRec.Bitmap);

  if CharMap.Texture.Map(TMapAccess.Write, Map) then
  try
    CharRec.Bitmap := CharMap.Texture;
    CharRec.BitmapRef := True;
    CharRec.SrcRect := TRectF.Create(LRect.Left + AntialiasMargin, LRect.Top + AntialiasMargin,
      LRect.Right - AntialiasMargin, LRect.Bottom - AntialiasMargin);
    for I := 0 to CharRec.Glyph.Bitmap.Height - 1 do
      Move(CharRec.Glyph.Bitmap.Scanline[I]^, Map.GetPixelAddr(LRect.Left + AntialiasMargin,
        LRect.Top + AntialiasMargin + I)^, CharRec.Glyph.Bitmap.Pitch);
  finally
    CharMap.Texture.Unmap(Map);
  end;
end;

function TTextLayoutNG.MeasureRange(APos, ALength: Integer): TRegion;
var
  I, LengthOffset, LLength, CharsLength: Integer;
  CharDic: TCharDic;
  Rec: PCharRec;
  R, R1: TRectF;
  Offset: Single;
  LRun: TGPURun;
  LineIndex, RunIndex: Integer;
begin
  SetLength(Result, 0);

  LLength := Text.Length;
  while ((APos + ALength) < LLength) and IsCombiningCharacter(Text.Chars[APos + ALength]) do //Skipping combining characters
    Inc(ALength);
  if (APos < LLength) and Text.Chars[APos].IsLowSurrogate then
  begin
    Dec(APos);
    Inc(ALength);
  end;

  for LineIndex := 0 to FFrame.Count - 1 do
  begin
    Offset := 0;
    if (LineIndex > 0) and (Length(Result) > 0) and (FFrame[LineIndex - 1].Count > 0) and (FFrame[LineIndex].Count > 0) then
      Dec(ALength, FFrame[LineIndex].First.StartIndex - FFrame[LineIndex - 1].Last.StartIndex - FFrame[LineIndex - 1].Last.Length);
    for RunIndex := 0 to FFrame[LineIndex].Count - 1 do
    begin
      LRun := FFrame[LineIndex][RunIndex];
      LengthOffset := LRun.StartIndex;
      R := TRectF.Create(0, 0, 0, 0);
      if APos < (LRun.StartIndex + LRun.Length) then
      begin
        CharDic := GetCharDictionary(LRun.Font);
        for I := 0 to LRun.Chars.Count - 1 do
        begin
          CharsLength := System.Char.ConvertFromUtf32(LRun.Chars[I]).Length;
          Rec := AddOrGetChar(nil, LRun.Chars[I], CharDic, LRun.Font);
          if LengthOffset < APos then
          begin
            Offset := Offset + Rec.Glyph.Advance * FScaleFactor;
            Inc(LengthOffset, CharsLength);
          end
          else
            if ALength > 0 then
            begin
              R1 := TRectF.Create(Offset, 0, Offset + Rec.Glyph.Advance * FScaleFactor, FFrame[LineIndex].Height);
              if R.IsEmpty then
                R := R1
              else
                R.Union(R1);
              Offset := Offset + Rec.Glyph.Advance * FScaleFactor;
              Dec(ALength, CharsLength);
              Inc(LengthOffset, CharsLength);
            end
            else
              Break;
        end;
      end
      else
        if APos = LLength then
        begin
          R := LRun.ImageRect;
          R.Left := R.Right;
          Dec(ALength);
        end
        else
          if ALength > 0 then
            Offset := Offset + LRun.ImageRect.Width
          else
            Break;
      if R.Right > 0 then
      begin
        SetLength(Result, Length(Result) + 1);
        R.Offset(FFrame[LineIndex].TopLeft);
        Result[High(Result)] := R;
        R := R.Empty;
      end;
    end;
    if R.Right > 0 then
    begin
      SetLength(Result, Length(Result) + 1);
      R.Offset(FFrame[LineIndex].TopLeft);
      Result[High(Result)] := R;
      R := R.Empty;
    end;
    if ALength = 0 then
      Exit;
  end;
end;

function TTextLayoutNG.DoPositionAtPoint(const APoint: TPointF): Integer;

  function RegionContains(const ARect: TRectF; const LPoint: TPointF): Boolean;
  begin
    Result := ((LPoint.X > ARect.Left) or SameValue(LPoint.X, ARect.Left, Epsilon)) and
              ((LPoint.X < ARect.Right)or SameValue(LPoint.X, ARect.Right, Epsilon)) and
              ((LPoint.Y > ARect.Top) or SameValue(LPoint.Y, ARect.Top, Epsilon)) and
              ((LPoint.Y < ARect.Bottom) or SameValue(LPoint.Y, ARect.Bottom, Epsilon));
  end;

var
  I, Index, Length, CharsLength: Integer;
  CharDic: TCharDic;
  Rec: PCharRec;
  LPoint: TPointF;
  R: TRectF;
  Offset: Single;
  LRun: TGPURun;
  LineIndex, RunIndex: Integer;
begin
  Result := -1;

  LPoint := TPointF.Create(APoint.X - TopLeft.X - Padding.Left, APoint.Y - TopLeft.Y - Padding.Top);
  LPoint.Offset(0, -FFrame.TopLeft.Y);

  for LineIndex := 0 to FFrame.Count - 1 do
  begin
    Offset := FFrame[LineIndex].TopLeft.X;
    if LPoint.Y > FFrame[LineIndex].Height then
    begin
      LPoint.Offset(0, -FFrame[LineIndex].Height);
      Continue;
    end;
    if LPoint.X < FFrame[LineIndex].TopLeft.X then
      Exit(FFrame[LineIndex].First.StartIndex);
    if LPoint.X > (FFrame[LineIndex].TopLeft.X + FFrame[LineIndex].Width) then
      Exit(FFrame[LineIndex].Last.StartIndex + FFrame[LineIndex].Last.Length);
    for RunIndex := 0 to FFrame[LineIndex].Count - 1 do
    begin
      LRun := FFrame[LineIndex][RunIndex];
      if LRun.Chars.Count = 0 then
        Exit(LRun.StartIndex);
      CharDic := GetCharDictionary(LRun.Font);
      CharsLength := 0;
      for I := 0 to LRun.Chars.Count - 1 do
      begin
        Rec := AddOrGetChar(nil, LRun.Chars[I], CharDic, LRun.Font);
        R := TRectF.Create(Offset, 0, Offset + Rec.Glyph.Advance * FScaleFactor, FFrame[LineIndex].Height);
        if RegionContains(R, LPoint) then
        begin
          Index := CharsLength + LRun.StartIndex;
          if LPoint.X > (R.Left + R.Width * 3 / 5) then
            Inc(Index);
          Length := Text.Length;
          if Text.Chars[Index].IsLowSurrogate then //Moving to the end of the surrogate pair
            Inc(Index);
          while ((Index + 1) < Length) and IsCombiningCharacter(Text.Chars[Index + 1]) do //Skipping combining characters
            Inc(Index);
          Exit(Min(Index, Text.Length));
        end;
        Offset := Offset + Rec.Glyph.Advance * FScaleFactor;
        Inc(CharsLength, System.Char.ConvertFromUtf32(LRun.Chars[I]).Length);
      end;
    end;
  end;
end;

function TTextLayoutNG.DoRegionForRange(const ARange: TTextRange): TRegion;
var
  i: Integer;
begin
  SetLength(Result, 0);
  if (ARange.Pos < 0) or (ARange.Length < 0) then
    Exit;

  if (ARange.Pos = Text.Length) and (ARange.Length = 0) then
    if Text.IsEmpty then
    begin
      SetLength(Result, 1);
      Result[0] := Self.TextRect;
      Exit;
    end
    else
    begin
      Result := MeasureRange(Text.Length - 1, 1);
      for i := Low(Result) to High(Result) do
        Result[i].Left := Result[i].Right;
    end
  else
  begin
    Result := MeasureRange(ARange.Pos, ARange.Length);
    if Length(Result) = 0 then
    begin
      SetLength(Result, 1);
      Result[0] := Self.TextRect;
      Result[0].Right := Result[0].Left;
      Exit;
    end;
  end;
  for i := Low(Result) to High(Result) do
    Result[i].Offset(TopLeft);
end;

initialization
finalization
  TTextLayoutNG.Uninitialize;
end.
