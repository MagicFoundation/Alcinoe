{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
Author(s):    JWB Software
              Anders Melander & Mike Lischke

product:      Alcinoe Graphic Functions
Version:      4.00

Description:  Procedure ALStrecth to stretch a bitmap using
              lanczos3 (by exemple)

Legal issues: Copyright (C) 1999-2013 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :

History :     26/06/2012: Add xe2 support

Link :

**************************************************************}
unit ALGraphic;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     Winapi.Windows,
     Vcl.Graphics;
     {$ELSE}
     Windows,
     Graphics;
     {$IFEND}

{$if CompilerVersion<=18.5}
//http://stackoverflow.com/questions/7630781/delphi-2007-and-xe2-using-nativeint
type
  NativeInt = Integer;
  NativeUInt = Cardinal;
{$ifend}

type
  // resampling support types
  TALResamplingFilter = (sfBox, sfTriangle, sfHermite, sfBell, sfSpline, sfLanczos3, sfMitchell);

// Resampling support routines
procedure ALStretch(NewWidth, NewHeight: Cardinal; Filter: TALResamplingFilter; Radius: Single; Source, Target: TBitmap); overload;
procedure ALStretch(NewWidth, NewHeight: Cardinal; Filter: TALResamplingFilter; Radius: Single; Source: TBitmap); overload;
procedure ALStrecth(Source, Target: TBitmap; filter: TALResamplingFilter; Radius: single); overload;

//tolerance is same value as tolerance in photoshop
procedure ALReplaceColor(SrcBitmap: TBitmap; OldColor, NewColor: Tcolor; Tolerance: Integer);

Type
  TALAverageColorMosaicKey = Array of array of Tcolor;

Function AlGetAverageColor(aSrcBmp: TBitmap; aRect: Trect): Tcolor;
Function AlGetAverageColorMosaicKey(aSrcBmp: TBitmap;
                                    aDestCanvas: Tcanvas;  //can be nil if we don't want to draw the result
                                    const aMosaicInGrayScale: Boolean = False;
                                    const aMosaicSquareWidth: integer = -1;
                                    const aMosaicSquareHeight: integer = -1): TALAverageColorMosaicKey; overload;
Function AlGetAverageColorMosaicKey(aSrcBmp: TBitmap): TALAverageColorMosaicKey; overload;

//tolerance is same value as tolerance in photoshop
procedure AlTrimImage(aSrcBmp: TBitmap; const aTolerance: Integer = 0);
procedure AlCropImage(aSrcBmp: TBitmap; aCropRect: Trect);
procedure ALRotateBMP90(aSrcBmp: TBitmap; aDestBmp: Tbitmap);
procedure ALRotateBMP180(aSrcBmp: TBitmap; aDestBmp: Tbitmap);
procedure ALRotateBMP270(aSrcBmp: TBitmap; aDestBmp: Tbitmap);

implementation

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     System.SysUtils,
     System.Classes,
     System.math;
     {$ELSE}
     SysUtils,
     Classes,
     math;
     {$IFEND}

const

  cAlPixelCountMax = 32768; //Using a large value for PixelCountMax serves two purposes.
                            //No bitmap can be that large (at present), and this effectively turns off
                            //range checking on Scanline variables.

type

  pAlRGBTripleArray = ^TAlRGBTripleArray;
  TAlRGBTripleArray = array[0..cAlPixelCountMax-1] OF TRGBTriple;

  // resampling support types
  TALRGBInt = record
   R, G, B: Integer;
  end;

  PALRGBWord = ^TALRGBWord;
  TALRGBWord = record
   R, G, B: Word;
  end;

  PALRGBAWord = ^TALRGBAWord;
  TALRGBAWord = record
   R, G, B, A: Word;
  end;

  PALBGR = ^TALBGR;
  TALBGR = packed record
   B, G, R: Byte;
  end;

  PALBGRA = ^TALBGRA;
  TALBGRA = packed record
   B, G, R, A: Byte;
  end;

  PALRGB = ^TALRGB;
  TALRGB = packed record
   R, G, B: Byte;
  end;

  PALRGBA = ^TALRGBA;
  TALRGBA = packed record
   R, G, B, A: Byte;
  end;

  PALPixelArray = ^TALPixelArray;
  TALPixelArray = array[0..0] of TALBGR;

  TALFilterFunction = function(Value: Single): Single;

  // contributor for a Pixel
  PALContributor = ^TALContributor;
  TALContributor = record
   Weight: Integer; // Pixel Weight
   Pixel: Integer; // Source Pixel
  end;

  TALContributors = array of TALContributor;

  // list of source pixels contributing to a destination pixel
  TALContributorEntry = record
   N: Integer;
   Contributors: TALContributors;
  end;

  TALContributorList = array of TALContributorEntry;

const
  cALDefaultFilterRadius: array[TALResamplingFilter] of Single = (0.5, 1, 1, 1.5, 2, 3, 2);

{*****************************************}
// f(t) = 2|t|^3 - 3|t|^2 + 1, -1 <= t <= 1
function ALHermiteFilter(Value: Single): Single;
begin
  if Value < 0 then Value := -Value;
  if Value < 1 then Result := (2 * Value - 3) * Sqr(Value) + 1
               else Result := 0;
end;

{*********************************************************}
// This filter is also known as 'nearest neighbour' Filter.
function ALBoxFilter(Value: Single): Single;
begin
  if (Value > -0.5) and (Value <= 0.5) then Result := 1
                                       else Result := 0;
end;

{**********************************}
// aka 'linear' or 'bilinear' filter
function ALTriangleFilter(Value: Single): Single;
begin
  if Value < 0 then Value := -Value;
  if Value < 1 then Result := 1 - Value
               else Result := 0;
end;

{*******************************************}
function ALBellFilter(Value: Single): Single;
begin
  if Value < 0 then Value := -Value;
  if Value < 0.5 then Result := 0.75 - Sqr(Value)
                 else
    if Value < 1.5 then
    begin
      Value := Value - 1.5;
      Result := 0.5 * Sqr(Value);
    end
    else Result := 0;
end;

{****************}
// B-spline filter
function ALSplineFilter(Value: Single): Single;
var
  Temp: Single;
begin
  if Value < 0 then Value := -Value;
  if Value < 1 then
  begin
    Temp := Sqr(Value);
    Result := 0.5 * Temp * Value - Temp + 2 / 3;
  end
  else
    if Value < 2 then
    begin
      Value := 2 - Value;
      Result := Sqr(Value) * Value / 6;
    end
    else Result := 0;
end;

{***********************************************}
function ALLanczos3Filter(Value: Single): Single;

  function SinC(Value: Single): Single;
  begin
    if Value <> 0 then
    begin
      Value := Value * Pi;
      Result := Sin(Value) / Value;
    end
    else Result := 1;
  end;

begin
  if Value < 0 then Value := -Value;
  if Value < 3 then Result := SinC(Value) * SinC(Value / 3)
               else Result := 0;
end;

{***********************************************}
function ALMitchellFilter(Value: Single): Single;
const
  B = 1 / 3;
  C = 1 / 3;
var Temp: Single;
begin
  if Value < 0 then Value := -Value;
  Temp := Sqr(Value);
  if Value < 1 then
  begin
    Value := (((12 - 9 * B - 6 * C) * (Value * Temp))
             + ((-18 + 12 * B + 6 * C) * Temp)
             + (6 - 2 * B));
    Result := Value / 6;
  end
  else
    if Value < 2 then
    begin
      Value := (((-B - 6 * C) * (Value * Temp))
               + ((6 * B + 30 * C) * Temp)
               + ((-12 * B - 48 * C) * Value)
               + (8 * B + 24 * C));
      Result := Value / 6;
    end
    else Result := 0;
end;

{***}
const
  cALFilterList: array[TALResamplingFilter] of TALFilterFunction = (
    ALBoxFilter,
    ALTriangleFilter,
    ALHermiteFilter,
    ALBellFilter,
    ALSplineFilter,
    ALLanczos3Filter,
    ALMitchellFilter
  );

{*******************************************}
procedure ALFillLineChache(N, Delta: Integer;
                           Line: Pointer;
                           var CurrentLineR: array of Integer;
                           var CurrentLineG: array of Integer;
                           var CurrentLineB: array of Integer);
var
  I: Integer;
  Run: PALBGR;
begin
  Run := Line;
  for I := 0 to N - 1 do
  begin
    CurrentLineR[I] := Run.R;
    CurrentLineG[I] := Run.G;
    CurrentLineB[I] := Run.B;
    Inc(PByte(Run), Delta);
  end;
end;

{******************************************************************************************************}
// ensures Value is in the range 0..255, values < 0 are clamped to 0 and values > 255 are clamped to 255
function ALClampByte(Value: Integer): Byte;
asm
         OR EAX, EAX
         JNS @@positive
         XOR EAX, EAX
         RET
@@positive:
         CMP EAX, 255
         JBE @@OK
         MOV EAX, 255
@@OK:
end;

{**************************************}
function ALApplyContributors(N: Integer;
                             Contributors: TALContributors;
                             var CurrentLineR: array of Integer;
                             var CurrentLineG: array of Integer;
                             var CurrentLineB: array of Integer): TALBGR;
var
  J: Integer;
  RGB: TALRGBInt;
  Total,
  Weight: Integer;
  Pixel: Cardinal;
  Contr: ^TALContributor;
begin
  RGB.R := 0;
  RGB.G := 0;
  RGB.B := 0;
  Total := 0;
  Contr := @Contributors[0];
  for J := 0 to N - 1 do
  begin
    Weight := Contr.Weight;
    Inc(Total, Weight);
    Pixel := Contr.Pixel;
    Inc(RGB.r, CurrentLineR[Pixel] * Weight);
    Inc(RGB.g, CurrentLineG[Pixel] * Weight);
    Inc(RGB.b, CurrentLineB[Pixel] * Weight);

    Inc(Contr);
  end;

  if Total = 0 then
  begin
    Result.R := ALClampByte(RGB.R shr 8);
    Result.G := ALClampByte(RGB.G shr 8);
    Result.B := ALClampByte(RGB.B shr 8);
  end
  else
  begin
    Result.R := ALClampByte(RGB.R div Total);
    Result.G := ALClampByte(RGB.G div Total);
    Result.B := ALClampByte(RGB.B div Total);
  end;
end;

{*******************************************************************************************************}
// This is the actual scaling routine. Target must be allocated already with sufficient size. Source must
// contain valid data, Radius must not be 0 and Filter must not be nil.
procedure ALDoStretch(Filter: TALFilterFunction; Radius: Single; Source, Target: TBitmap);
var
  ScaleX,
  ScaleY: Single;  // Zoom scale factors
  I, J,
  K, N: Integer; // Loop variables
  Center: Single; // Filter calculation variables
  Width: Single;
  Weight: Integer;  // Filter calculation variables
  Left,
  Right: Integer; // Filter calculation variables
  Work: TBitmap;
  ContributorList: TALContributorList;
  SourceLine,
  DestLine: PALPixelArray;
  DestPixel: PALBGR;
  Delta,
  DestDelta: Integer;
  SourceHeight,
  SourceWidth,
  TargetHeight,
  TargetWidth: Integer;
  CurrentLineR: array of Integer;
  CurrentLineG: array of Integer;
  CurrentLineB: array of Integer;

begin
  // shortcut variables
  SourceHeight := Source.Height;
  SourceWidth := Source.Width;
  TargetHeight := Target.Height;
  TargetWidth := Target.Width;

  if (SourceHeight = 0) or (SourceWidth = 0) or
     (TargetHeight = 0) or (TargetWidth = 0) then Exit;

  // create intermediate image to hold horizontal zoom
  Work := TBitmap.Create;
  try
    Work.PixelFormat := pf24Bit;
    Work.Height := SourceHeight;
    Work.Width := TargetWidth;
    if SourceWidth = 1 then ScaleX :=  TargetWidth / SourceWidth
                       else ScaleX :=  (TargetWidth - 1) / (SourceWidth - 1);
    if (SourceHeight = 1) or (TargetHeight = 1) then ScaleY :=  TargetHeight / SourceHeight
                                                else ScaleY :=  (TargetHeight - 1) / (SourceHeight - 1);

    // pre-calculate filter contributions for a row
    SetLength(ContributorList, TargetWidth);
    // horizontal sub-sampling
    if ScaleX < 1 then
    begin
      // scales from bigger to smaller Width
      Width := Radius / ScaleX;
      for I := 0 to TargetWidth - 1 do
      begin
        ContributorList[I].N := 0;
        SetLength(ContributorList[I].Contributors, Trunc(2 * Width + 1));
        Center := I / ScaleX;
        Left := Floor(Center - Width);
        Right := Ceil(Center + Width);
        for J := Left to Right do
        begin
          Weight := Round(Filter((Center - J) * ScaleX) * ScaleX * 256);
          if Weight <> 0 then
          begin
            if J < 0 then N := -J
                     else
              if J >= SourceWidth then N := SourceWidth - J + SourceWidth - 1
                                  else N := J;
            K := ContributorList[I].N;
            Inc(ContributorList[I].N);
            ContributorList[I].Contributors[K].Pixel := N;
            ContributorList[I].Contributors[K].Weight := Weight;
          end;
        end;
      end;
    end
    else
    begin
      // horizontal super-sampling
      // scales from smaller to bigger Width
      for I := 0 to TargetWidth - 1 do
      begin
        ContributorList[I].N := 0;
        SetLength(ContributorList[I].Contributors, Trunc(2 * Radius + 1));
        Center := I / ScaleX;
        Left := Floor(Center - Radius);
        Right := Ceil(Center + Radius);
        for J := Left to Right do
        begin
          Weight := Round(Filter(Center - J) * 256);
          if Weight <> 0 then
          begin
            if J < 0 then N := -J
                     else
             if J >= SourceWidth then N := SourceWidth - J + SourceWidth - 1
                                 else N := J;
            K := ContributorList[I].N;
            Inc(ContributorList[I].N);
            ContributorList[I].Contributors[K].Pixel := N;
            ContributorList[I].Contributors[K].Weight := Weight;
          end;
        end;
      end;
    end;

    // now apply filter to sample horizontally from Src to Work
    SetLength(CurrentLineR, SourceWidth);
    SetLength(CurrentLineG, SourceWidth);
    SetLength(CurrentLineB, SourceWidth);
    for K := 0 to SourceHeight - 1 do
    begin
      SourceLine := Source.ScanLine[K];
      ALFillLineChache(SourceWidth, 3, SourceLine, CurrentLineR, CurrentLineG, CurrentLineB);
      DestPixel := Work.ScanLine[K];
      for I := 0 to TargetWidth - 1 do
        with ContributorList[I] do
        begin
          DestPixel^ := ALApplyContributors(N, ContributorList[I].Contributors, CurrentLineR, CurrentLineG, CurrentLineB);
          // move on to next column
          Inc(DestPixel);
        end;
    end;

    // free the memory allocated for horizontal filter weights, since we need the stucture again
    for I := 0 to TargetWidth - 1 do ContributorList[I].Contributors := nil;
    ContributorList := nil;

    // pre-calculate filter contributions for a column
    SetLength(ContributorList, TargetHeight);
    // vertical sub-sampling
    if ScaleY < 1 then
    begin
      // scales from bigger to smaller height
      Width := Radius / ScaleY;
      for I := 0 to TargetHeight - 1 do
      begin
        ContributorList[I].N := 0;
        SetLength(ContributorList[I].Contributors, Trunc(2 * Width + 1));
        Center := I / ScaleY;
        Left := Floor(Center - Width);
        Right := Ceil(Center + Width);
        for J := Left to Right do
        begin
          Weight := Round(Filter((Center - J) * ScaleY) * ScaleY * 256);
          if Weight <> 0 then
          begin
            if J < 0 then N := -J
                     else
              if J >= SourceHeight then N := SourceHeight - J + SourceHeight - 1
                                   else N := J;
            K := ContributorList[I].N;
            Inc(ContributorList[I].N);
            ContributorList[I].Contributors[K].Pixel := N;
            ContributorList[I].Contributors[K].Weight := Weight;
          end;
        end;
      end
    end
    else
    begin
      // vertical super-sampling
      // scales from smaller to bigger height
      for I := 0 to TargetHeight - 1 do
      begin
        ContributorList[I].N := 0;
        SetLength(ContributorList[I].Contributors, Trunc(2 * Radius + 1));
        Center := I / ScaleY;
        Left := Floor(Center - Radius);
        Right := Ceil(Center + Radius);
        for J := Left to Right do
        begin
          Weight := Round(Filter(Center - J) * 256);
          if Weight <> 0 then
          begin
            if J < 0 then N := -J
                     else
              if J >= SourceHeight then N := SourceHeight - J + SourceHeight - 1
                                   else N := J;
            K := ContributorList[I].N;
            Inc(ContributorList[I].N);
            ContributorList[I].Contributors[K].Pixel := N;
            ContributorList[I].Contributors[K].Weight := Weight;
          end;
        end;
      end;
    end;

    // apply filter to sample vertically from Work to Target
    SetLength(CurrentLineR, SourceHeight);
    SetLength(CurrentLineG, SourceHeight);
    SetLength(CurrentLineB, SourceHeight);


    SourceLine := Work.ScanLine[0];
    Delta := Integer(Work.ScanLine[1]) - Integer(SourceLine);
    DestLine := Target.ScanLine[0];
    DestDelta := Integer(Target.ScanLine[1]) - Integer(DestLine);
    for K := 0 to TargetWidth - 1 do
    begin
      DestPixel := Pointer(DestLine);
      ALFillLineChache(SourceHeight, Delta, SourceLine, CurrentLineR, CurrentLineG, CurrentLineB);
      for I := 0 to TargetHeight - 1 do
        with ContributorList[I] do
        begin
          DestPixel^ := ALApplyContributors(N, ContributorList[I].Contributors, CurrentLineR, CurrentLineG, CurrentLineB);
          Inc(NativeInt(DestPixel), DestDelta);
        end;
      Inc(SourceLine);
      Inc(DestLine);
    end;

    // free the memory allocated for vertical filter weights
    for I := 0 to TargetHeight - 1 do ContributorList[I].Contributors := nil;
    // this one is done automatically on exit, but is here for completeness
    ContributorList := nil;

  finally
    Work.Free;
    CurrentLineR := nil;
    CurrentLineG := nil;
    CurrentLineB := nil;
  end;
end;

{**************************************************************************************************}
// Scales the source bitmap to the given size (NewWidth, NewHeight) and stores the Result in Target.
// Filter describes the filter function to be applied and Radius the size of the filter area.
// Is Radius = 0 then the recommended filter area will be used (see DefaultFilterRadius).
procedure ALStretch(NewWidth, NewHeight: Cardinal; Filter: TALResamplingFilter; Radius: Single; Source, Target: TBitmap);
begin
  if Radius = 0 then Radius := cALDefaultFilterRadius[Filter];
  Target.Handle := 0;
  Target.PixelFormat := pf24Bit;
  Target.Width := NewWidth;
  Target.Height := NewHeight;
  Source.PixelFormat := pf24Bit;
  ALDoStretch(cALFilterList[Filter], Radius, Source, Target);
end;

{***************************************************************************************************************}
procedure ALStretch(NewWidth, NewHeight: Cardinal; Filter: TALResamplingFilter; Radius: Single; Source: TBitmap);
var
  Target: TBitmap;
begin
  if Radius = 0 then Radius := cALDefaultFilterRadius[Filter];
  Target := TBitmap.Create;
  try
    Target.PixelFormat := pf24Bit;
    Target.Width := NewWidth;
    Target.Height := NewHeight;
    Source.PixelFormat := pf24Bit;
    ALDoStretch(cALFilterList[Filter], Radius, Source, Target);
    Source.Assign(Target);
  finally
    Target.Free;
  end;
end;

{****************************************************************************************}
procedure ALStrecth(Source, Target: TBitmap; filter: TALResamplingFilter; Radius: single);
begin
  ALStretch(Target.Width, Target.Height, Filter, Radius, Source, Target);
end;

{*******************************************************************************************}
procedure ALReplaceColor(SrcBitmap: TBitmap; OldColor, NewColor: Tcolor; Tolerance: Integer);
Var R1, G1, B1: Integer;
    x, y: integer;
    aColor: Tcolor;
begin
  R1 := GetRValue(OldColor);
  G1 := GetGValue(OldColor);
  B1 := GetBValue(OldColor);

  SrcBitmap.Canvas.Lock;
  Try
    For x := 0 to SrcBitmap.Width do
      for y := 0 to SrcBitmap.Height do begin
        aColor := SrcBitmap.Canvas.Pixels[x,y];
        If (Tolerance<=0) or
           (sqrt(sqr(GetRValue(aColor) - R1) +
                 sqr(GetGValue(aColor) - G1) +
                 sqr(GetBValue(aColor) - B1)) <= Tolerance)
        then SrcBitmap.Canvas.Pixels[x,y] := NewColor;
      end;
  finally
    SrcBitmap.Canvas.Unlock;
  end;
end;

{*****************************************************************}
Function AlGetAverageColor(aSrcBmp: TBitmap; aRect: Trect): Tcolor;
Var aCount: Integer;
    aLine : pAlRGBTripleArray;
    R, G, B: Cardinal;
    X, Y: integer;
Begin
  aCount := 0;
  R := 0;
  G := 0;
  B := 0;
  aSrcBmp.PixelFormat := pf24bit;
  for Y := aRect.top to aRect.bottom - 1 do begin
    aLine := aSrcBmp.Scanline[Y];
    for X := aRect.left to aRect.right - 1 do begin
      With aLine[X] do begin
        R := R + rgbtRed;
        G := G + rgbtGreen;
        B := B + rgbtBlue;
      end;
      inc(aCount);
    end;
  end;
  R := round(R / aCount);
  G := round(G / aCount);
  B := round(B / aCount);
  Result := RGB(r, g, b);
End;

{***************************************************}
Function AlGetAverageColorMosaicKey(aSrcBmp: TBitmap;
                                    aDestCanvas: Tcanvas;
                                    const aMosaicInGrayScale: Boolean = False;
                                    const aMosaicSquareWidth: integer = -1;
                                    const aMosaicSquareHeight: integer = -1): TALAverageColorMosaicKey;

Var aMosaicSquareWidthTmp: Integer;
    aMosaicSquareHeightTmp: Integer;
    aColor: Tcolor;
    aRect: Trect;
    X1, Y1: integer;
    X2, Y2: integer;
    r, g, b: Byte;

Begin

  //init aMosaicSquareWidthTmp and aMosaicSquareHeightTmp
  aMosaicSquareWidthTmp := aMosaicSquareWidth;
  if aMosaicSquareWidthTmp <= 0 then aMosaicSquareWidthTmp := Round(aSrcBmp.Width / 5);
  aMosaicSquareHeightTmp := aMosaicSquareHeight;
  if aMosaicSquareHeightTmp <= 0 then aMosaicSquareHeightTmp := Round(aSrcBmp.Height / 5);

  //init Result
  X2 := trunc(aSrcBmp.Width / aMosaicSquareWidthTmp);
  if aSrcBmp.Width mod aMosaicSquareWidthTmp <> 0 then inc(X2);
  Y2 := trunc(aSrcBmp.Height / aMosaicSquareHeightTmp);
  if aSrcBmp.Height mod aMosaicSquareHeightTmp <> 0 then inc(Y2);
  SetLength(Result, X2, Y2);

  //start the loop
  X1 := 0;
  X2 := 0;
  while X1 < aSrcBmp.Width do begin

    Y1 := 0;
    Y2 := 0;
    while Y1 < aSrcBmp.Height do begin

      //calc the rect
      aRect := Rect(X1,
                    Y1,
                    Min(X1 + aMosaicSquareWidthTmp, aSrcBmp.Width),
                    Min(Y1 + aMosaicSquareHeightTmp, aSrcBmp.Height));

      //find the average color
      aColor := AlGetAverageColor(aSrcBmp,aRect);

      //move the color in grayscale
      if aMosaicInGrayScale then begin
        // their is 2 Main Solution :
        //   1/ The luminosity method is a more sophisticated version of the average
        //      method. It also averages the values, but it forms a weighted average
        //      to account for human perception. We’re more sensitive to green than
        //      other colors, so green is weighted most heavily. The formula for luminosity
        //      is 0.212671 R + 0.715160 G + 0.072169 B.
        //   2/ The average method simply averages the values: (R + G + B) / 3.
        // I choose the 1 method because in the second methode only Red color, or only Blue color are converted
        // to the same grayscale color. see on http://www.johndcook.com/blog/2009/08/24/more-on-colors-and-grayscale/
        R := round(0.212671 * GetRValue(aColor) + 0.715160 * GetGValue(aColor) + 0.072169 * GetBValue(aColor));
        G := R;
        B := R;
        aColor := RGB(r, g, b);
      end;

      //fill the result
      result[X2,Y2] := acolor;

      //draw the result
      if assigned(aDestCanvas) then begin
        aDestCanvas.Brush.Color := aColor;
        aDestCanvas.FillRect(aRect);
      end;

      //inc Y
      Y1 := Y1 + aMosaicSquareHeightTmp;

      //inc Y2
      inc(Y2);

    end;

    //inc X
    X1 := X1 + aMosaicSquareWidthTmp;

    //inc X2
    inc(X2);

  end;

End;

{******************************************************************************}
Function AlGetAverageColorMosaicKey(aSrcBmp: TBitmap): TALAverageColorMosaicKey;
var aResizedBitmap: Tbitmap;
begin

  aResizedBitmap := Tbitmap.Create;
  try

    //init the Resizedbitmap
    aResizedBitmap.PixelFormat := pf24bit;
    aResizedBitmap.Width:=200;
    aResizedBitmap.Height:=200;

    //the First Picture
    ALStrecth(aSrcBmp, aResizedBitmap, sfLanczos3, 3.0);
    Result := AlGetAverageColorMosaicKey(aResizedBitmap,
                                         nil,
                                         true,
                                         40,
                                         40);

  finally
    aResizedBitmap.free;
  end;

End;

{*********************************************************************}
procedure AlTrimImage(aSrcBmp: TBitmap; const aTolerance: Integer = 0);
var aRow: pAlRGBTripleArray;
    aIsBlack : Integer;
    aCropRect: Trect;
    aBackRColor,
    aBackGColor,
    aBackBColor: Byte;
    aTmpBmp: Tbitmap;
    X, Y: integer;
begin

  //set the aSrcBmp.pixelformat
  aSrcBmp.PixelFormat := pf24bit;

  //security check
  if (aSrcBmp.Width = 0) or
     (aSrcBmp.Height = 0) then exit;

  //Retrieve Background Color
  aBackRColor := GetRvalue(aSrcBmp.Canvas.Pixels[0,0]);
  aBackGColor := GetGvalue(aSrcBmp.Canvas.Pixels[0,0]);
  aBackBColor := GetBvalue(aSrcBmp.Canvas.Pixels[0,0]);

  //init a cropRect
  aCropRect := Rect(0,0,0,0);

  //Find Top
  aCropRect.top := 0;
  for Y := 0 to aSrcBmp.Height-1 do begin
    aRow := aSrcBmp.Scanline[Y];
    aisBlack := 0;
    for X := 0 to aSrcBmp.Width-1 do begin
      With aRow[X] do begin
        if (sqrt(sqr(rgbtRed - aBackRColor) +
                 sqr(rgbtGreen - aBackGColor) +
                 sqr(rgbtBlue - aBackBColor)) > aTolerance) then begin
           aIsBlack := 1;
           break;
        end;
      end;
    end;
    if aisBlack = 0 then Inc(aCropRect.Top)
    else break;
  end;

  //Find bottom
  aCropRect.Bottom := aSrcBmp.Height;
  for Y := aSrcBmp.Height-1 Downto 0 do begin
    aRow := aSrcBmp.Scanline[Y];
    aisBlack := 0;
    for X := 0 to aSrcBmp.Width-1 do begin
      With aRow[X] do begin
        if (sqrt(sqr(rgbtRed - aBackRColor) +
                 sqr(rgbtGreen - aBackGColor) +
                 sqr(rgbtBlue - aBackBColor)) > aTolerance) then begin
           aIsBlack := 1;
           break;
        end;
      end;
    end;
    if aisBlack = 0 then Dec(aCropRect.Bottom)
    else break;
  end;

  //Find Left
  aCropRect.Left := aSrcBmp.Width;
  for Y := 0 to aSrcBmp.Height-1 do begin
    aRow := aSrcBmp.Scanline[Y];
    aisBlack := 0;
    for X := 0 to aSrcBmp.Width-1 do begin
      With aRow[X] do begin
        if (sqrt(sqr(rgbtRed - aBackRColor) +
                 sqr(rgbtGreen - aBackGColor) +
                 sqr(rgbtBlue - aBackBColor)) <= aTolerance) then inc(aisBlack)
        else break;
      end;
    end;
    aCropRect.Left := Min(aCropRect.Left,aIsBlack);
  end;

  //Find right
  aCropRect.Right := 0;
  for Y := 0 to aSrcBmp.Height-1 do begin
    aRow := aSrcBmp.Scanline[Y];
    aisBlack := aSrcBmp.Width;
    for X := aSrcBmp.Width-1 downto 0 do begin
      With aRow[X] do begin
        if (sqrt(sqr(rgbtRed - aBackRColor) +
                 sqr(rgbtGreen - aBackGColor) +
                 sqr(rgbtBlue - aBackBColor)) <= aTolerance) then dec(aisBlack)
        else break;
      end;
    end;
    aCropRect.right := Max(aCropRect.right,aIsBlack);
  end;

  //crop the img if necessary
  if (aCropRect.Top > 0) or
     (aCropRect.left > 0) or
     (aCropRect.bottom < aSrcBmp.Height) or
     (aCropRect.right < aSrcBmp.width) then begin

    //if the picture was blanc
    if (aCropRect.Bottom < aCropRect.top) or
       (aCropRect.right < aCropRect.left) then begin

      aSrcBmp.Width := 0;
      aSrcBmp.height := 0;

    end

    //else do a copy
    else begin

      //use BitBlt to do the copy
      aTmpBmp := Tbitmap.Create;
      Try
        aTmpbmp.PixelFormat := Pf24Bit;
        aTmpbmp.Width := aCropRect.right - aCropRect.left;
        aTmpbmp.Height := aCropRect.bottom - aCropRect.top;
        If Not BitBlt(aTmpbmp.canvas.Handle,           //[in] Handle to the destination device context.
                      0,                               //[in] Specifies the x-coordinate, in logical units, of the upper-left corner of the destination rectangle
                      0,                               //[in] Specifies the y-coordinate, in logical units, of the upper-left corner of the destination rectangle
                      aTmpbmp.Width,                   //[in] Specifies the width, in logical units, of the source and destination rectangles.
                      aTmpBmp.Height,                  //[in] Specifies the height, in logical units, of the source and the destination rectangles.
                      asrcBmp.canvas.Handle,           //[in] Handle to the source device context
                      aCropRect.left,                  //[in] Specifies the x-coordinate, in logical units, of the upper-left corner of the source rectangle.
                      aCropRect.top,                   //[in] Specifies the y-coordinate, in logical units, of the upper-left corner of the source rectangle.
                      SRCCOPY) then raiseLastOsError;  //[in] Specifies a raster-operation code

        aSrcBmp.Width := aTmpbmp.Width;
        aSrcBmp.Height := aTmpbmp.Height;
        aSrcbmp.Assign(aTmpbmp);
      Finally
        aTmpBmp.free;
      End;

    end;

  end;

end;

{********************************************************}
procedure AlCropImage(aSrcBmp: TBitmap; aCropRect: Trect);
Var aTmpBmp: Tbitmap;
begin

  //security check
  if (aSrcBmp.Width = 0) or
     (aSrcBmp.Height = 0) then exit;

  //init the aCropRect
  if (aCropRect.Top < 0) then aCropRect.Top := 0;
  if (aCropRect.left < 0) then aCropRect.left := 0;
  if (aCropRect.bottom > aSrcBmp.Height) then aCropRect.bottom := aSrcBmp.Height;
  if (aCropRect.right > aSrcBmp.width) then aCropRect.right := aSrcBmp.width;

  //crop the img if necessary
  if (aCropRect.Top > 0) or
     (aCropRect.left > 0) or
     (aCropRect.bottom < aSrcBmp.Height) or
     (aCropRect.right < aSrcBmp.width) then begin

    aTmpBmp := Tbitmap.Create;
    Try
      aSrcBmp.PixelFormat := pf24bit;
      aTmpbmp.PixelFormat := Pf24Bit;
      aTmpbmp.Width := aCropRect.right - aCropRect.left;
      aTmpbmp.Height := aCropRect.bottom - aCropRect.top;
      If not BitBlt(aTmpbmp.canvas.Handle,            //[in] Handle to the destination device context.
                    0,                                //[in] Specifies the x-coordinate, in logical units, of the upper-left corner of the destination rectangle
                    0,                                //[in] Specifies the y-coordinate, in logical units, of the upper-left corner of the destination rectangle
                    aTmpbmp.Width,                    //[in] Specifies the width, in logical units, of the source and destination rectangles.
                    aTmpBmp.Height,                   //[in] Specifies the height, in logical units, of the source and the destination rectangles.
                    asrcBmp.canvas.Handle,            //[in] Handle to the source device context
                    aCropRect.left,                   //[in] Specifies the x-coordinate, in logical units, of the upper-left corner of the source rectangle.
                    aCropRect.top,                    //[in] Specifies the y-coordinate, in logical units, of the upper-left corner of the source rectangle.
                    SRCCOPY) then RaiseLastOSError;   //[in] Specifies a raster-operation code
      aSrcBmp.Width := aTmpbmp.Width;
      aSrcBmp.Height := aTmpbmp.Height;
      aSrcbmp.Assign(aTmpbmp);
    Finally
      aTmpBmp.free;
    End;

  end;

end;

{***********************************************************}
procedure ALRotateBMP90(aSrcBmp: TBitmap; aDestBmp: Tbitmap);
var X, Y: Integer;
    aSrcScanLine: pAlRGBTripleArray;
    aDestScanLines: array of pAlRGBTripleArray;
begin

  //init the aDestBmp
  aDestBmp.PixelFormat := pf24bit;
  aDestBmp.Width := aSrcBmp.Height;
  aDestBmp.Height := aSrcBmp.Width;

  //init aDestScanLines
  SetLength(aDestScanLines, aDestBmp.Height);
  for Y := 0 to aDestBmp.Height - 1 do aDestScanLines[Y] := aDestBmp.ScanLine[Y];

  //invert the pixel
  for Y := 0 to aSrcBmp.Height - 1 do begin
    aSrcScanLine := aSrcBmp.Scanline[Y];
    for X := 0 to aSrcBmp.Width - 1 do begin
      aDestScanLines[X][(aSrcBmp.Height - 1) - Y] := aSrcScanLine[X]
    end;
  end;

end;

{************************************************************}
procedure ALRotateBMP180(aSrcBmp: TBitmap; aDestBmp: Tbitmap);
var X, Y: Integer;
    aSrcScanLine: pAlRGBTripleArray;
    aDestScanLines: array of pAlRGBTripleArray;
begin

  //init the aDestBmp
  aDestBmp.PixelFormat := pf24bit;
  aDestBmp.Width := aSrcBmp.Width;
  aDestBmp.Height := aSrcBmp.Height;

  //init aDestScanLines
  SetLength(aDestScanLines, aDestBmp.Height);
  for Y := 0 to aDestBmp.Height - 1 do aDestScanLines[Y] := aDestBmp.ScanLine[Y];

  //invert the pixel
  for Y := 0 to aSrcBmp.Height - 1 do begin
    aSrcScanLine := aSrcBmp.Scanline[Y];
    for X := 0 to aSrcBmp.Width - 1 do begin
      aDestScanLines[(aSrcBmp.Height - 1) - Y][(aSrcBmp.width - 1) - X] := aSrcScanLine[X]
    end;
  end;

end;

{************************************************************}
procedure ALRotateBMP270(aSrcBmp: TBitmap; aDestBmp: Tbitmap);
var X, Y: Integer;
    aSrcScanLine: pAlRGBTripleArray;
    aDestScanLines: array of pAlRGBTripleArray;
begin

  //init the aDestBmp
  aDestBmp.PixelFormat := pf24bit;
  aDestBmp.Width := aSrcBmp.Height;
  aDestBmp.Height := aSrcBmp.Width;

  //init aDestScanLines
  SetLength(aDestScanLines, aDestBmp.Height);
  for Y := 0 to aDestBmp.Height - 1 do aDestScanLines[Y] := aDestBmp.ScanLine[Y];

  //invert the pixel
  for Y := 0 to aSrcBmp.Height - 1 do begin
    aSrcScanLine := aSrcBmp.Scanline[Y];
    for X := 0 to aSrcBmp.Width - 1 do begin
      aDestScanLines[(aSrcBmp.width - 1) - X][Y] := aSrcScanLine[X]
    end;
  end;

end;

end.
