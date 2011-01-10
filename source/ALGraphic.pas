{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/
Author(s):    JWB Software
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      Alcinoe Graphic Functions
Version:      3.51

Description:  Procedure ALStrecth to stretch a bitmap using
              lanczos3 (by exemple)

Legal issues: Copyright (C) 1999-2010 by Arkadia Software Engineering

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

History:

Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALGraphic;

interface

uses Windows,
     SysUtils,
     Classes,
     Graphics;

type
  // Type of a filter for use with Stretch()
  TALFilterProc = function(Value: Single): Single;

  // Sample filters for use with Stretch()
  function ALSplineFilter(Value: Single): Single;
  function ALBellFilter(Value: Single): Single;
  function ALTriangleFilter(Value: Single): Single;
  function ALBoxFilter(Value: Single): Single;
  function ALHermiteFilter(Value: Single): Single;
  function ALLanczos3Filter(Value: Single): Single;
  function ALMitchellFilter(Value: Single): Single;

  // Interpolator
  // Src:	Source bitmap
  // Dst:	Destination bitmap
  // filter:	Weight calculation filter
  // fwidth:	Relative sample radius
  procedure ALStrecth(Src, Dst: TBitmap; filter: TALFilterProc; fwidth: single);
  //tolerance is same value as tolerance in photoshop
  procedure ALReplaceColor(SrcBitmap: TBitmap; OldColor, NewColor: Tcolor; Tolerance: Integer);

// -----------------------------------------------------------------------------
//
//			List of Filters
//
// -----------------------------------------------------------------------------

const
  ALResampleFilters: array[0..6] of record
    Name: string;	// Filter name
    Filter: TALFilterProc;// Filter implementation
    Width: Single;	// Suggested sampling width/radius
  end = (
    (Name: 'Box';	Filter: ALBoxFilter;	Width: 0.5),
    (Name: 'Triangle';	Filter: ALTriangleFilter;	Width: 1.0),
    (Name: 'Hermite';	Filter: ALHermiteFilter;	Width: 1.0),
    (Name: 'Bell';	Filter: ALBellFilter;	Width: 1.5),
    (Name: 'B-Spline';	Filter: ALSplineFilter;	Width: 2.0),
    (Name: 'Lanczos3';	Filter: ALLanczos3Filter;	Width: 3.0),
    (Name: 'Mitchell';	Filter: ALMitchellFilter;	Width: 2.0)
    );

Const cAlPixelCountMax = 32768; //Using a large value for PixelCountMax serves two purposes.
                                //No bitmap can be that large (at present), and this effectively turns off
                                //range checking on Scanline variables.

type pAlRGBTripleArray = ^TAlRGBTripleArray;
     TAlRGBTripleArray = ARRAY[0..cAlPixelCountMax-1] OF TRGBTriple;

     TALAverageColorMosaicKey = Array of array of Tcolor;

Function AlGetAverageColor(aSrcBmp: TBitmap; aRect: Trect): Tcolor;
Function AlGetAverageColorMosaicKey(aSrcBmp: TBitmap;
                                    aDestCanvas: Tcanvas;  //can be nil if we don't want to draw the result
                                    const aMosaicInGrayScale: Boolean = False;
                                    const aMosaicSquareWidth: integer = -1;
                                    const aMosaicSquareHeight: integer = -1): TALAverageColorMosaicKey; overload;
Function AlGetAverageColorMosaicKey(aSrcBmp: TBitmap): TALAverageColorMosaicKey; overload;

//tolerance is same value as tolerance in photoshop
procedure AlTrimImage(Var aSrcBmp: TBitmap; const aTolerance: Integer = 0);
procedure AlCropImage(Var aSrcBmp: TBitmap; aCropRect: Trect);

implementation

uses math;

// -----------------------------------------------------------------------------
//
//			Filter functions
//
// -----------------------------------------------------------------------------

// Hermite filter
function ALHermiteFilter(Value: Single): Single;
begin
  // f(t) = 2|t|^3 - 3|t|^2 + 1, -1 <= t <= 1
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 1.0) then
    Result := (2.0 * Value - 3.0) * Sqr(Value) + 1.0
  else
    Result := 0.0;
end;

// Box filter
// a.k.a. "Nearest Neighbour" filter
// anme: I have not been able to get acceptable
//       results with this filter for subsampling.
function ALBoxFilter(Value: Single): Single;
begin
  if (Value > -0.5) and (Value <= 0.5) then
    Result := 1.0
  else
    Result := 0.0;
end;

// Triangle filter
// a.k.a. "Linear" or "Bilinear" filter
function ALTriangleFilter(Value: Single): Single;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 1.0) then
    Result := 1.0 - Value
  else
    Result := 0.0;
end;

// Bell filter
function ALBellFilter(Value: Single): Single;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 0.5) then
    Result := 0.75 - Sqr(Value)
  else if (Value < 1.5) then
  begin
    Value := Value - 1.5;
    Result := 0.5 * Sqr(Value);
  end else
    Result := 0.0;
end;

// B-spline filter
function ALSplineFilter(Value: Single): Single;
var
  tt			: single;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 1.0) then
  begin
    tt := Sqr(Value);
    Result := 0.5*tt*Value - tt + 2.0 / 3.0;
  end else if (Value < 2.0) then
  begin
    Value := 2.0 - Value;
    Result := 1.0/6.0 * Sqr(Value) * Value;
  end else
    Result := 0.0;
end;

// Lanczos3 filter
function ALLanczos3Filter(Value: Single): Single;
  function SinC(Value: Single): Single;
  begin
    if (Value <> 0.0) then
    begin
      Value := Value * Pi;
      Result := sin(Value) / Value
    end else
      Result := 1.0;
  end;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 3.0) then
    Result := SinC(Value) * SinC(Value / 3.0)
  else
    Result := 0.0;
end;

function ALMitchellFilter(Value: Single): Single;
const
  B		= (1.0 / 3.0);
  C		= (1.0 / 3.0);
var
  tt			: single;
begin
  if (Value < 0.0) then
    Value := -Value;
  tt := Sqr(Value);
  if (Value < 1.0) then
  begin
    Value := (((12.0 - 9.0 * B - 6.0 * C) * (Value * tt))
      + ((-18.0 + 12.0 * B + 6.0 * C) * tt)
      + (6.0 - 2 * B));
    Result := Value / 6.0;
  end else
  if (Value < 2.0) then
  begin
    Value := (((-1.0 * B - 6.0 * C) * (Value * tt))
      + ((6.0 * B + 30.0 * C) * tt)
      + ((-12.0 * B - 48.0 * C) * Value)
      + (8.0 * B + 24 * C));
    Result := Value / 6.0;
  end else
    Result := 0.0;
end;

// -----------------------------------------------------------------------------
//
//			Interpolator
//
// -----------------------------------------------------------------------------
type
  // Contributor for a pixel
  TContributor = record
    pixel: integer;		// Source pixel
    weight: single;		// Pixel weight
  end;

  TContributorList = array[0..0] of TContributor;
  PContributorList = ^TContributorList;

  // List of source pixels contributing to a destination pixel
  TCList = record
    n		: integer;
    p		: PContributorList;
  end;

  TCListList = array[0..0] of TCList;
  PCListList = ^TCListList;

  TRGB = packed record
    r, g, b	: single;
  end;

  // Physical bitmap pixel
  TColorRGB = packed record
    r, g, b	: BYTE;
  end;
  PColorRGB = ^TColorRGB;

  // Physical bitmap scanline (row)
  TRGBList = packed array[0..0] of TColorRGB;
  PRGBList = ^TRGBList;

procedure ALStrecth(Src, Dst: TBitmap; filter: TALFilterProc; fwidth: single);
var
  xscale, yscale	: single;		// Zoom scale factors
  i, j, k		: integer;		// Loop variables
  center		: single;		// Filter calculation variables
  width, fscale, weight	: single;		// Filter calculation variables
  left, right		: integer;		// Filter calculation variables
  n			: integer;		// Pixel number
  Work			: TBitmap;
  contrib		: PCListList;
  rgb			: TRGB;
  color			: TColorRGB;
  SourceLine		,
  DestLine		: PRGBList;
  SourcePixel		,
  DestPixel		: PColorRGB;
  Delta			,
  DestDelta		: integer;
  SrcWidth		,
  SrcHeight		,
  DstWidth		,
  DstHeight		: integer;

  function Color2RGB(Color: TColor): TColorRGB;
  begin
    Result.r := Color AND $000000FF;
    Result.g := (Color AND $0000FF00) SHR 8;
    Result.b := (Color AND $00FF0000) SHR 16;
  end;

  function RGB2Color(Color: TColorRGB): TColor;
  begin
    Result := Color.r OR (Color.g SHL 8) OR (Color.b SHL 16);
  end;

begin
  DstWidth := Dst.Width;
  DstHeight := Dst.Height;
  SrcWidth := Src.Width;
  SrcHeight := Src.Height;
  if (SrcWidth < 1) or (SrcHeight < 1) then
    raise Exception.Create('Source bitmap too small');

  // Create intermediate image to hold horizontal zoom
  Work := TBitmap.Create;
  try
    Work.Height := SrcHeight;
    Work.Width := DstWidth;
    // xscale := DstWidth / SrcWidth;
    // yscale := DstHeight / SrcHeight;
    // Improvement suggested by David Ullrich:
    if (SrcWidth = 1) then
      xscale:= DstWidth / SrcWidth
    else
      xscale:= (DstWidth - 1) / (SrcWidth - 1);
    if (SrcHeight = 1) then
      yscale:= DstHeight / SrcHeight
    else
      yscale:= (DstHeight - 1) / (SrcHeight - 1);
    // This implementation only works on 24-bit images because it uses
    // TBitmap.Scanline
    Src.PixelFormat := pf24bit;
    Dst.PixelFormat := Src.PixelFormat;
    Work.PixelFormat := Src.PixelFormat;

    // --------------------------------------------
    // Pre-calculate filter contributions for a row
    // -----------------------------------------------
    GetMem(contrib, DstWidth* sizeof(TCList));
    // Horizontal sub-sampling
    // Scales from bigger to smaller width
    if (xscale < 1.0) then
    begin
      width := fwidth / xscale;
      fscale := 1.0 / xscale;
      for i := 0 to DstWidth-1 do
      begin
        contrib^[i].n := 0;
        GetMem(contrib^[i].p, trunc(width * 2.0 + 1) * sizeof(TContributor));
        center := i / xscale;
        // Original code:
        // left := ceil(center - width);
        // right := floor(center + width);
        left := floor(center - width);
        right := ceil(center + width);
        for j := left to right do
        begin
          weight := filter((center - j) / fscale) / fscale;
          if (weight = 0.0) then
            continue;
          if (j < 0) then
            n := -j
          else if (j >= SrcWidth) then
            n := SrcWidth - j + SrcWidth - 1
          else
            n := j;
          k := contrib^[i].n;
          contrib^[i].n := contrib^[i].n + 1;
          contrib^[i].p^[k].pixel := n;
          contrib^[i].p^[k].weight := weight;
        end;
      end;
    end else
    // Horizontal super-sampling
    // Scales from smaller to bigger width
    begin
      for i := 0 to DstWidth-1 do
      begin
        contrib^[i].n := 0;
        GetMem(contrib^[i].p, trunc(fwidth * 2.0 + 1) * sizeof(TContributor));
        center := i / xscale;
        // Original code:
        // left := ceil(center - fwidth);
        // right := floor(center + fwidth);
        left := floor(center - fwidth);
        right := ceil(center + fwidth);
        for j := left to right do
        begin
          weight := filter(center - j);
          if (weight = 0.0) then
            continue;
          if (j < 0) then
            n := -j
          else if (j >= SrcWidth) then
            n := SrcWidth - j + SrcWidth - 1
          else
            n := j;
          k := contrib^[i].n;
          contrib^[i].n := contrib^[i].n + 1;
          contrib^[i].p^[k].pixel := n;
          contrib^[i].p^[k].weight := weight;
        end;
      end;
    end;

    // ----------------------------------------------------
    // Apply filter to sample horizontally from Src to Work
    // ----------------------------------------------------
    for k := 0 to SrcHeight-1 do
    begin
      SourceLine := Src.ScanLine[k];
      DestPixel := Work.ScanLine[k];
      for i := 0 to DstWidth-1 do
      begin
        rgb.r := 0.0;
        rgb.g := 0.0;
        rgb.b := 0.0;
        for j := 0 to contrib^[i].n-1 do
        begin
          color := SourceLine^[contrib^[i].p^[j].pixel];
          weight := contrib^[i].p^[j].weight;
          if (weight = 0.0) then
            continue;
          rgb.r := rgb.r + color.r * weight;
          rgb.g := rgb.g + color.g * weight;
          rgb.b := rgb.b + color.b * weight;
        end;
        if (rgb.r > 255.0) then
          color.r := 255
        else if (rgb.r < 0.0) then
          color.r := 0
        else
          color.r := round(rgb.r);
        if (rgb.g > 255.0) then
          color.g := 255
        else if (rgb.g < 0.0) then
          color.g := 0
        else
          color.g := round(rgb.g);
        if (rgb.b > 255.0) then
          color.b := 255
        else if (rgb.b < 0.0) then
          color.b := 0
        else
          color.b := round(rgb.b);
        // Set new pixel value
        DestPixel^ := color;
        // Move on to next column
        inc(DestPixel);
      end;
    end;

    // Free the memory allocated for horizontal filter weights
    for i := 0 to DstWidth-1 do
      FreeMem(contrib^[i].p);

    FreeMem(contrib);

    // -----------------------------------------------
    // Pre-calculate filter contributions for a column
    // -----------------------------------------------
    GetMem(contrib, DstHeight* sizeof(TCList));
    // Vertical sub-sampling
    // Scales from bigger to smaller height
    if (yscale < 1.0) then
    begin
      width := fwidth / yscale;
      fscale := 1.0 / yscale;
      for i := 0 to DstHeight-1 do
      begin
        contrib^[i].n := 0;
        GetMem(contrib^[i].p, trunc(width * 2.0 + 1) * sizeof(TContributor));
        center := i / yscale;
        // Original code:
        // left := ceil(center - width);
        // right := floor(center + width);
        left := floor(center - width);
        right := ceil(center + width);
        for j := left to right do
        begin
          weight := filter((center - j) / fscale) / fscale;
          if (weight = 0.0) then
            continue;
          if (j < 0) then
            n := -j
          else if (j >= SrcHeight) then
            n := SrcHeight - j + SrcHeight - 1
          else
            n := j;
          k := contrib^[i].n;
          contrib^[i].n := contrib^[i].n + 1;
          contrib^[i].p^[k].pixel := n;
          contrib^[i].p^[k].weight := weight;
        end;
      end
    end else
    // Vertical super-sampling
    // Scales from smaller to bigger height
    begin
      for i := 0 to DstHeight-1 do
      begin
        contrib^[i].n := 0;
        GetMem(contrib^[i].p, trunc(fwidth * 2.0 + 1) * sizeof(TContributor));
        center := i / yscale;
        // Original code:
        // left := ceil(center - fwidth);
        // right := floor(center + fwidth);
        left := floor(center - fwidth);
        right := ceil(center + fwidth);
        for j := left to right do
        begin
          weight := filter(center - j);
          if (weight = 0.0) then
            continue;
          if (j < 0) then
            n := -j
          else if (j >= SrcHeight) then
            n := SrcHeight - j + SrcHeight - 1
          else
            n := j;
          k := contrib^[i].n;
          contrib^[i].n := contrib^[i].n + 1;
          contrib^[i].p^[k].pixel := n;
          contrib^[i].p^[k].weight := weight;
        end;
      end;
    end;

    // --------------------------------------------------
    // Apply filter to sample vertically from Work to Dst
    // --------------------------------------------------
    SourceLine := Work.ScanLine[0];
    Delta := integer(Work.ScanLine[1]) - integer(SourceLine);
    DestLine := Dst.ScanLine[0];
    DestDelta := integer(Dst.ScanLine[1]) - integer(DestLine);
    for k := 0 to DstWidth-1 do
    begin
      DestPixel := pointer(DestLine);
      for i := 0 to DstHeight-1 do
      begin
        rgb.r := 0;
        rgb.g := 0;
        rgb.b := 0;
        // weight := 0.0;
        for j := 0 to contrib^[i].n-1 do
        begin
          color := PColorRGB(integer(SourceLine)+contrib^[i].p^[j].pixel*Delta)^;
          weight := contrib^[i].p^[j].weight;
          if (weight = 0.0) then
            continue;
          rgb.r := rgb.r + color.r * weight;
          rgb.g := rgb.g + color.g * weight;
          rgb.b := rgb.b + color.b * weight;
        end;
        if (rgb.r > 255.0) then
          color.r := 255
        else if (rgb.r < 0.0) then
          color.r := 0
        else
          color.r := round(rgb.r);
        if (rgb.g > 255.0) then
          color.g := 255
        else if (rgb.g < 0.0) then
          color.g := 0
        else
          color.g := round(rgb.g);
        if (rgb.b > 255.0) then
          color.b := 255
        else if (rgb.b < 0.0) then
          color.b := 0
        else
          color.b := round(rgb.b);
        DestPixel^ := color;
        inc(integer(DestPixel), DestDelta);
      end;
      Inc(SourceLine, 1);
      Inc(DestLine, 1);
    end;

    // Free the memory allocated for vertical filter weights
    for i := 0 to DstHeight-1 do
      FreeMem(contrib^[i].p);

    FreeMem(contrib);

  finally
    Work.Free;
  end;
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
           (
            sqrt(
                 sqr(GetRValue(aColor) - R1) +
                 sqr(GetGValue(aColor) - G1) +
                 sqr(GetBValue(aColor) - B1)
                ) <= Tolerance
           )
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
      aRect := Rect(
                    X1,
                    Y1,
                    Min(X1 + aMosaicSquareWidthTmp, aSrcBmp.Width),
                    Min(Y1 + aMosaicSquareHeightTmp, aSrcBmp.Height)
                   );

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
    ALStrecth(aSrcBmp, aResizedBitmap, ALLanczos3Filter, 3.0);
    Result := AlGetAverageColorMosaicKey(
                                         aResizedBitmap,
                                         nil,
                                         true,
                                         40,
                                         40
                                        );

  finally
    aResizedBitmap.free;
  end;

End;

{*************************************************************************}
procedure AlTrimImage(Var aSrcBmp: TBitmap; const aTolerance: Integer = 0);
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
        if (sqrt(
                 sqr(rgbtRed - aBackRColor) +
                 sqr(rgbtGreen - aBackGColor) +
                 sqr(rgbtBlue - aBackBColor)
                ) > aTolerance) then begin
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
        if (sqrt(
                 sqr(rgbtRed - aBackRColor) +
                 sqr(rgbtGreen - aBackGColor) +
                 sqr(rgbtBlue - aBackBColor)
                ) > aTolerance) then begin
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
        if (sqrt(
                 sqr(rgbtRed - aBackRColor) +
                 sqr(rgbtGreen - aBackGColor) +
                 sqr(rgbtBlue - aBackBColor)
                ) <= aTolerance) then inc(aisBlack)
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
        if (sqrt(
                 sqr(rgbtRed - aBackRColor) +
                 sqr(rgbtGreen - aBackGColor) +
                 sqr(rgbtBlue - aBackBColor)
                ) <= aTolerance) then dec(aisBlack)
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
        If Not BitBlt(
                      aTmpbmp.canvas.Handle,   //[in] Handle to the destination device context.
                      0,                       //[in] Specifies the x-coordinate, in logical units, of the upper-left corner of the destination rectangle
                      0,                       //[in] Specifies the y-coordinate, in logical units, of the upper-left corner of the destination rectangle
                      aTmpbmp.Width,           //[in] Specifies the width, in logical units, of the source and destination rectangles.
                      aTmpBmp.Height,          //[in] Specifies the height, in logical units, of the source and the destination rectangles.
                      asrcBmp.canvas.Handle,   //[in] Handle to the source device context
                      aCropRect.left,          //[in] Specifies the x-coordinate, in logical units, of the upper-left corner of the source rectangle.
                      aCropRect.top,           //[in] Specifies the y-coordinate, in logical units, of the upper-left corner of the source rectangle.
                      SRCCOPY                  //[in] Specifies a raster-operation code
                     ) then raiseLastOsError;
        aSrcbmp.Free;
      Except
        aTmpBmp.free;
        raise;
      End;
      aSrcBmp := aTmpBmp;

    end;

  end;

end;

{************************************************************}
procedure AlCropImage(Var aSrcBmp: TBitmap; aCropRect: Trect);
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
      If not BitBlt(
                    aTmpbmp.canvas.Handle,   //[in] Handle to the destination device context.
                    0,                       //[in] Specifies the x-coordinate, in logical units, of the upper-left corner of the destination rectangle
                    0,                       //[in] Specifies the y-coordinate, in logical units, of the upper-left corner of the destination rectangle
                    aTmpbmp.Width,           //[in] Specifies the width, in logical units, of the source and destination rectangles.
                    aTmpBmp.Height,          //[in] Specifies the height, in logical units, of the source and the destination rectangles.
                    asrcBmp.canvas.Handle,   //[in] Handle to the source device context
                    aCropRect.left,          //[in] Specifies the x-coordinate, in logical units, of the upper-left corner of the source rectangle.
                    aCropRect.top,           //[in] Specifies the y-coordinate, in logical units, of the upper-left corner of the source rectangle.
                    SRCCOPY                  //[in] Specifies a raster-operation code
                   ) then RaiseLastOSError;
      aSrcbmp.Free;
    Except
      aTmpBmp.free;
      raise;
    End;
    aSrcBmp := aTmpBmp;

  end;

end;

end.
