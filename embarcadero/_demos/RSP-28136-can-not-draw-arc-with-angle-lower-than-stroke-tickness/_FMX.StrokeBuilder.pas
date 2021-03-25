{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013-2021 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.StrokeBuilder;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.UITypes, System.Math, System.Math.Vectors, FMX.Graphics, FMX.Canvas.GPU.Helpers;

{.$DEFINE SafeInitArrays}
{.$DEFINE BuildSanityChecks}

type
  TStrokeBuilder = class
  private
    FMatrix: TMatrix;
    FBrush: TStrokeBrush;

    FVertices: TCanvasHelper.TVertexArray;
    FColors: TCanvasHelper.TAlphaColorArray;
    FIndices: TCanvasHelper.TIndexArray;

    FCurrentVertex: Integer;
    FCurrentIndex: Integer;

    FSegmentCount: Integer;
    FLastSegmentFraction: Single;
    FExtraPieces: Integer;
    FLastDashExtend: Boolean;
    FThickness: Single;
    FHalfThickness: Single;
    FStrokeColor: TAlphaColor;

    FEllipseCenter: TPointF;
    FEllipseRadius: TPointF;
    FEllipseCircumf: Single;
    FEllipseTransfCenter: TPointF;
    FUndeterminedMode: Boolean;

    function GetMatrixScale: TPointF;

    procedure ArrayFillCheck;
    procedure InitArrays(const VertexCount, IndexCount: Integer); inline;
    procedure FinalizeArrays;
    procedure InitArrayPointers;

    procedure InsertVertex(const VertexPos: TPointF; const Color: TAlphaColor);
    procedure InsertIndex(const Value: Integer);
    function GetCapDivisions: Integer; inline;
    procedure GetDashEstimate(out VertexCount, IndexCount: Integer);
    procedure InsertDash(SrcPos, DestPos: TPointF; const DashDirVec, ThickPerp: TPointF);
    procedure GetDotEstimate(out VertexCount, IndexCount: Integer);
    procedure InsertDot(const MidPos, DotDirVec, ThickPerp: TPointF);
    function GetPatternStepCount: Integer;
    procedure ComputeBuildEstimates(const TentSegmentCount: Single; out VertexCount, IndexCount: Integer);
    procedure InsertSegment(const SegmentPos, SegDirVec, ThickPerp, DestPos: TPointF; IsLast: Boolean);

    function GetEllipseTransfAt(const Delta: Single): TPointF;
    procedure InsertEllipseSegment(const SegInitDist: Single; IsLast: Boolean);
  public
    procedure BuildLine(const SrcPos, DestPos: TPointF; const Opacity: Single);
    procedure BuildIntermEllipse(const Center, Radius: TPointF; const Opacity: Single);
    procedure BuildSolidEllipse(const Center, Radius: TPointF; const Opacity: Single);

    procedure BuildIntermPolygon(const Points: TPolygon; const Opacity: Single; BreakAtEnd: Boolean = False);
    procedure BuildSolidPolygon(const Points: TPolygon; const Opacity: Single; BreakAtEnd: Boolean = False);

    procedure BuildIntermPath(const Path: TPathData; const Opacity: Single);
    procedure BuildSolidPath(const Path: TPathData; const Opacity: Single);

    procedure ResetArrays;

    property Matrix: TMatrix read FMatrix write FMatrix;
    property Brush: TStrokeBrush read FBrush write FBrush;

    property Vertices: TCanvasHelper.TVertexArray read FVertices;
    property Colors: TCanvasHelper.TAlphaColorArray read FColors;
    property Indices: TCanvasHelper.TIndexArray read FIndices;
  end;

implementation

uses
  System.UIConsts, System.Generics.Collections;

function PointFDot(const P1, P2: TPointF): Single;
begin
  Result := (P1.X * P2.X) + (P1.Y * P2.Y);
end;

{$REGION 'Stroke Builder implementation'}

function TStrokeBuilder.GetMatrixScale: TPointF;
const
  BaseVector: TPointF = (X: 0; Y: 0);
begin
  Result.X := (PointF(1, 0) * FMatrix).Distance(BaseVector * FMatrix);
  Result.Y := (PointF(0, 1) * FMatrix).Distance(BaseVector * FMatrix);
end;

procedure TStrokeBuilder.InitArrayPointers();
begin
  FCurrentVertex := 0;
  FCurrentIndex := 0;
end;

procedure TStrokeBuilder.InitArrays(const VertexCount, IndexCount: Integer);
{$IFDEF SafeInitArrays}
var
  Index: Integer;
{$ENDIF}
begin
  SetLength(FVertices, VertexCount);
  SetLength(FColors, VertexCount);
  SetLength(FIndices, IndexCount);

{$IFDEF SafeInitArrays}
  for Index := 0 to IndexCount - 1 do
    FIndices[Index] := -1;

  FillChar(FVertices[0], SizeOf(TPointF) * VertexCount, 0);
  FillChar(FColors[0], SizeOf(TAlphaColor) * VertexCount, 0);
{$ENDIF}
  InitArrayPointers();
end;

procedure TStrokeBuilder.ResetArrays;
begin
  SetLength(FVertices, 0);
  SetLength(FColors, 0);
  SetLength(FIndices, 0);
end;

procedure TStrokeBuilder.ArrayFillCheck;
begin
{$IFDEF BuildSanityChecks}
  Assert(FCurrentVertex = Length(FVertices), 'Vertices have not been filled correctly.');
  Assert(FCurrentIndex = Length(FIndices), 'Indices have not been filled correctly.');
{$ENDIF}
end;

procedure TStrokeBuilder.FinalizeArrays;
begin
  if FUndeterminedMode then
  begin
    SetLength(FVertices, FCurrentVertex);
    SetLength(FColors, FCurrentVertex);
    SetLength(FIndices, FCurrentIndex);
  end;
end;

procedure TStrokeBuilder.InsertVertex(const VertexPos: TPointF; const Color: TAlphaColor);
var
  NewValue: Integer;
begin
  if FUndeterminedMode and (Length(FVertices) <= FCurrentVertex) then
  begin
    NewValue := 8 + Ceil(Length(FVertices) * 1.5);

    SetLength(FVertices, NewValue);
    SetLength(FColors, NewValue);
  end;

{$IFDEF BuildSanityChecks}
  Assert(FCurrentVertex < Length(FVertices), 'Too many vertices.');
{$ENDIF}
  FVertices[FCurrentVertex] := VertexPos;
  FColors[FCurrentVertex] := Color;
  Inc(FCurrentVertex);
end;

procedure TStrokeBuilder.InsertIndex(const Value: Integer);
var
  NewValue: Integer;
begin
  if FUndeterminedMode and (Length(FIndices) <= FCurrentIndex) then
  begin
    NewValue := 12 + Ceil(Length(FIndices) * 1.5);
    SetLength(FIndices, NewValue);
  end;

{$IFDEF BuildSanityChecks}
  Assert(FCurrentIndex < Length(FIndices), 'Too many indices.');
{$ENDIF}
  FIndices[FCurrentIndex] := Value;
  Inc(FCurrentIndex);
end;

function TStrokeBuilder.GetCapDivisions: Integer;
begin
  if FBrush.Cap = TStrokeCap.Round then
    Result := Max(Ceil(FThickness * Pi / 4.0), 2)
  else
    Result := 0;
end;

procedure TStrokeBuilder.GetDashEstimate(out VertexCount, IndexCount: Integer);
var
  Divisions: Integer;
begin
  case FBrush.Cap of
    TStrokeCap.Round:
      begin
        Divisions := GetCapDivisions;

        VertexCount := 6 + Divisions * 2;
        IndexCount := 6 + (Divisions + 1) * 6;
      end;

  else
    begin
      VertexCount := 4;
      IndexCount := 6;
    end;
  end;
end;

procedure TStrokeBuilder.InsertDash(SrcPos, DestPos: TPointF; const DashDirVec, ThickPerp: TPointF);
var
  InitIndex, DivIndex, Divisions: Integer;
  SinValue, CosValue: Single;
  RoundShift: TPointF;
begin
  if FBrush.Cap = TStrokeCap.Round then
  begin
    RoundShift := DashDirVec * FHalfThickness;

    SrcPos := SrcPos + RoundShift;
    DestPos := DestPos - RoundShift;
  end;

  InitIndex := FCurrentVertex;

  InsertVertex(SrcPos + ThickPerp, FStrokeColor);
  InsertVertex(DestPos + ThickPerp, FStrokeColor);
  InsertVertex(DestPos - ThickPerp, FStrokeColor);
  InsertVertex(SrcPos - ThickPerp, FStrokeColor);

  InsertIndex(InitIndex + 0);
  InsertIndex(InitIndex + 1);
  InsertIndex(InitIndex + 2);
  InsertIndex(InitIndex + 2);
  InsertIndex(InitIndex + 3);
  InsertIndex(InitIndex + 0);

  if FBrush.Cap = TStrokeCap.Round then
  begin
    InsertVertex(SrcPos, FStrokeColor);
    InsertVertex(DestPos, FStrokeColor);

    Divisions := GetCapDivisions;

    for DivIndex := 0 to Divisions - 1 do
    begin
      SinCos((DivIndex + 1) * Pi / (Divisions + 1), SinValue, CosValue);

      InsertVertex(PointF(SrcPos.X + ThickPerp.X * CosValue - ThickPerp.Y * SinValue, SrcPos.Y + ThickPerp.X * SinValue + ThickPerp.Y *
        CosValue), FStrokeColor);
    end;

    for DivIndex := 0 to Divisions - 1 do
    begin
      SinCos((DivIndex + 1) * Pi / (Divisions + 1), SinValue, CosValue);

      InsertVertex(PointF(DestPos.X + ThickPerp.Y * SinValue - ThickPerp.X * CosValue,
        DestPos.Y - (ThickPerp.X * SinValue + ThickPerp.Y * CosValue)), FStrokeColor);
    end;

    InsertIndex(InitIndex + 4);
    InsertIndex(InitIndex + 0);
    InsertIndex(InitIndex + 6);

    InsertIndex(InitIndex + 4);
    InsertIndex(InitIndex + 5 + Divisions);
    InsertIndex(InitIndex + 3);

    for DivIndex := 0 to Divisions - 2 do
    begin
      InsertIndex(InitIndex + 4);
      InsertIndex(InitIndex + 6 + DivIndex);
      InsertIndex(InitIndex + 7 + DivIndex);
    end;

    InsertIndex(InitIndex + 2);
    InsertIndex(InitIndex + 6 + Divisions);
    InsertIndex(InitIndex + 5);

    InsertIndex(InitIndex + 5);
    InsertIndex(InitIndex + 5 + Divisions * 2);
    InsertIndex(InitIndex + 1);

    for DivIndex := 0 to Divisions - 2 do
    begin
      InsertIndex(InitIndex + 5);
      InsertIndex(InitIndex + 6 + Divisions + DivIndex);
      InsertIndex(InitIndex + 7 + Divisions + DivIndex);
    end;
  end;
end;

procedure TStrokeBuilder.GetDotEstimate(out VertexCount, IndexCount: Integer);
var
  Divisions: Integer;
begin
  case FBrush.Cap of
    TStrokeCap.Round:
      begin
        Divisions := GetCapDivisions;

        VertexCount := 3 + Divisions * 2;
        IndexCount := (Divisions + 1) * 6;
      end;

  else
    begin
      VertexCount := 4;
      IndexCount := 6;
    end;
  end;
end;

procedure TStrokeBuilder.InsertDot(const MidPos, DotDirVec, ThickPerp: TPointF);
var
  InitIndex, DivIndex, Divisions: Integer;
  SinValue, CosValue: Single;
  DotParShift: TPointF;
begin
  InitIndex := FCurrentVertex;

  if FBrush.Cap = TStrokeCap.Flat then
  begin
    DotParShift := DotDirVec * FHalfThickness;

    InsertVertex(MidPos + ThickPerp - DotParShift, FStrokeColor);
    InsertVertex(MidPos + DotParShift + ThickPerp, FStrokeColor);
    InsertVertex(MidPos + DotParShift - ThickPerp, FStrokeColor);
    InsertVertex(MidPos - (ThickPerp + DotParShift), FStrokeColor);

    InsertIndex(InitIndex + 0);
    InsertIndex(InitIndex + 1);
    InsertIndex(InitIndex + 2);
    InsertIndex(InitIndex + 2);
    InsertIndex(InitIndex + 3);
    InsertIndex(InitIndex + 0);
  end
  else
  begin
    InsertVertex(MidPos, FStrokeColor);

    Divisions := 2 + GetCapDivisions * 2;

    for DivIndex := 0 to Divisions - 1 do
    begin
      SinCos(DivIndex * (Pi * 2.0) / Divisions, SinValue, CosValue);

      InsertVertex(PointF(MidPos.X + ThickPerp.X * CosValue - ThickPerp.Y * SinValue, MidPos.Y + ThickPerp.X * SinValue + ThickPerp.Y *
        CosValue), FStrokeColor);
    end;

    for DivIndex := 0 to Divisions - 1 do
    begin
      InsertIndex(InitIndex);
      InsertIndex(InitIndex + 1 + DivIndex);
      InsertIndex(InitIndex + 1 + ((DivIndex + 1) mod Divisions));
    end;
  end;
end;

function TStrokeBuilder.GetPatternStepCount: Integer;
begin
  case FBrush.Dash of
    TStrokeDash.Solid, TStrokeDash.Custom:
      Result := 1;

    TStrokeDash.Dash:
      Result := 4;

    TStrokeDash.Dot:
      Result := 2;

    TStrokeDash.DashDot:
      Result := 6;

    TStrokeDash.DashDotDot:
      Result := 8;

  else
    Result := 0;
  end;
end;

procedure TStrokeBuilder.ComputeBuildEstimates(const TentSegmentCount: Single; out VertexCount, IndexCount: Integer);
var
  PieceVertices, PieceIndices, FloorSegmentCount, CeilSegmentCount: Integer;
begin
  FExtraPieces := 0;
  FLastDashExtend := False;

  FLastSegmentFraction := Frac(TentSegmentCount);

  FloorSegmentCount := Floor(TentSegmentCount);
  CeilSegmentCount := Ceil(TentSegmentCount);

  case FBrush.Dash of
    TStrokeDash.Solid, TStrokeDash.Custom:
      begin
        FSegmentCount := 1;
        GetDashEstimate(VertexCount, IndexCount);
      end;

    TStrokeDash.Dash:
      begin
        FSegmentCount := FloorSegmentCount;

        if FLastSegmentFraction >= 0.25 then
        begin
          FSegmentCount := CeilSegmentCount;
          FLastDashExtend := (CeilSegmentCount <> FloorSegmentCount) and (FLastSegmentFraction < 0.75);
        end;

        GetDashEstimate(PieceVertices, PieceIndices);

        VertexCount := PieceVertices * FSegmentCount;
        IndexCount := PieceIndices * FSegmentCount;
      end;

    TStrokeDash.Dot:
      begin
        FSegmentCount := Round(TentSegmentCount);

        GetDotEstimate(PieceVertices, PieceIndices);

        VertexCount := PieceVertices * FSegmentCount;
        IndexCount := PieceIndices * FSegmentCount;
      end;

    TStrokeDash.DashDot:
      begin
        FSegmentCount := FloorSegmentCount;

        if FLastSegmentFraction >= 1 / 6 then
        begin
          FSegmentCount := CeilSegmentCount;
          FLastDashExtend := (CeilSegmentCount <> FloorSegmentCount) and (FLastSegmentFraction < 0.5);
        end
        else
          FLastSegmentFraction := 1;

        GetDashEstimate(PieceVertices, PieceIndices);
        VertexCount := PieceVertices * FSegmentCount;
        IndexCount := PieceIndices * FSegmentCount;

        GetDotEstimate(PieceVertices, PieceIndices);

        if FSegmentCount > 1 then
        begin
          Inc(VertexCount, PieceVertices * (FSegmentCount - 1));
          Inc(IndexCount, PieceIndices * (FSegmentCount - 1));
        end;

        if FLastSegmentFraction >= 5 / 6 then
        begin
          Inc(VertexCount, PieceVertices);
          Inc(IndexCount, PieceIndices);
          Inc(FExtraPieces);
        end;
      end;

    TStrokeDash.DashDotDot:
      begin
        FSegmentCount := FloorSegmentCount;

        if FLastSegmentFraction >= 1 / 8 then
        begin
          FSegmentCount := CeilSegmentCount;
          FLastDashExtend := (CeilSegmentCount <> FloorSegmentCount) and (FLastSegmentFraction < 3.0 / 8.0);
        end
        else
          FLastSegmentFraction := 1;

        GetDashEstimate(PieceVertices, PieceIndices);
        VertexCount := PieceVertices * FSegmentCount;
        IndexCount := PieceIndices * FSegmentCount;

        GetDotEstimate(PieceVertices, PieceIndices);

        if FSegmentCount > 1 then
        begin
          Inc(VertexCount, PieceVertices * (FSegmentCount - 1) * 2);
          Inc(IndexCount, PieceIndices * (FSegmentCount - 1) * 2);
        end;

        if FLastSegmentFraction >= 7 / 8 then
        begin
          Inc(VertexCount, PieceVertices * 2);
          Inc(IndexCount, PieceIndices * 2);
          Inc(FExtraPieces, 2);
        end
        else if FLastSegmentFraction >= 5 / 8 then
        begin
          Inc(VertexCount, PieceVertices);
          Inc(IndexCount, PieceIndices);
          Inc(FExtraPieces);
        end;
      end;
  end;
end;

procedure TStrokeBuilder.InsertSegment(const SegmentPos, SegDirVec, ThickPerp, DestPos: TPointF; IsLast: Boolean);
var
  PieceTail: TPointF;
begin
  case FBrush.Dash of
    TStrokeDash.Solid, TStrokeDash.Custom:
      InsertDash(SegmentPos, DestPos, SegDirVec, ThickPerp);

    TStrokeDash.Dash:
      begin
        if IsLast and FLastDashExtend then
          PieceTail := DestPos
        else
          PieceTail := SegmentPos + (SegDirVec * FThickness * 3);

        InsertDash(SegmentPos, PieceTail, SegDirVec, ThickPerp);
      end;

    TStrokeDash.Dot:
      InsertDot(SegmentPos + (SegDirVec * FHalfThickness), SegDirVec, ThickPerp);

    TStrokeDash.DashDot:
      begin
        if IsLast and FLastDashExtend then
          PieceTail := DestPos
        else
          PieceTail := SegmentPos + (SegDirVec * FThickness * 3);

        InsertDash(SegmentPos, PieceTail, SegDirVec, ThickPerp);

        if (not IsLast) or (FExtraPieces > 0) then
          InsertDot(SegmentPos + (SegDirVec * FThickness * 4.5), SegDirVec, ThickPerp);
      end;

    TStrokeDash.DashDotDot:
      begin
        if IsLast and FLastDashExtend then
          PieceTail := DestPos
        else
          PieceTail := SegmentPos + (SegDirVec * FThickness * 3);

        InsertDash(SegmentPos, PieceTail, SegDirVec, ThickPerp);

        if (not IsLast) or (FExtraPieces > 0) then
          InsertDot(SegmentPos + (SegDirVec * FThickness * 4.5), SegDirVec, ThickPerp);

        if (not IsLast) or (FExtraPieces > 1) then
          InsertDot(SegmentPos + (SegDirVec * FThickness * 6.5), SegDirVec, ThickPerp);
      end;
  end;
end;

procedure TStrokeBuilder.BuildLine(const SrcPos, DestPos: TPointF; const Opacity: Single);
var
  FinalSrcPos, FinalDestPos, PiecePos, StepDelta, ThickPerp: TPointF;
  PieceDirVec, CurScale: TPointF;
  StepSize, PiecesCount: Single;
  StepIndex, LastStepIndex, PatternStepCount: Integer;
  LineLength: Single;
  TotalVertices, TotalIndices: Integer;
begin
  CurScale := GetMatrixScale;

  FThickness := FBrush.Thickness * (CurScale.X + CurScale.Y) * 0.5;
  FHalfThickness := FThickness * 0.5;
  FStrokeColor := PremultiplyAlpha(MakeColor(FBrush.Color, Opacity));
  FUndeterminedMode := False;

  FinalSrcPos := SrcPos * Matrix;
  FinalDestPos := DestPos * Matrix;
  LineLength := FinalSrcPos.Distance(FinalDestPos);

  PieceDirVec := (FinalDestPos - FinalSrcPos).Normalize;

  PatternStepCount := GetPatternStepCount;

  if PatternStepCount > 0 then
    StepSize := FThickness * PatternStepCount
  else
    StepSize := LineLength;

  StepDelta := PieceDirVec * StepSize;

  PiecePos := FinalSrcPos;
  ThickPerp := PointF(-PieceDirVec.Y, PieceDirVec.X) * (FThickness * 0.5);

  if StepSize <= 0 then
  begin
    InitArrays(0, 0);
    Exit;
  end;

  PiecesCount := LineLength / StepSize;

  ComputeBuildEstimates(PiecesCount, TotalVertices, TotalIndices);
  if FSegmentCount < 1 then
  begin
    InitArrays(0, 0);
    Exit;
  end;

  LastStepIndex := FSegmentCount - 1;

  InitArrays(TotalVertices, TotalIndices);

  for StepIndex := 0 to LastStepIndex do
  begin
    InsertSegment(PiecePos, PieceDirVec, ThickPerp, FinalDestPos, StepIndex >= LastStepIndex);
    PiecePos := PiecePos + StepDelta;
  end;

  ArrayFillCheck();
end;

function TStrokeBuilder.GetEllipseTransfAt(const Delta: Single): TPointF;
var
  Angle, CosAngle, SinAngle: Single;
  SampleAt: TPointF;
begin
  Angle := Delta * 2.0 * Pi / FEllipseCircumf;

  SinCos(Angle, SinAngle, CosAngle);

  SampleAt.X := FEllipseCenter.X + CosAngle * FEllipseRadius.X;
  SampleAt.Y := FEllipseCenter.Y - SinAngle * FEllipseRadius.Y;

  Result := SampleAt * FMatrix;
end;

procedure TStrokeBuilder.InsertEllipseSegment(const SegInitDist: Single; IsLast: Boolean);
var
  TempDelta: Single;
  SegSrcPos, SegDestPos, SegDirVec, ThickPerp: TPointF;
begin
  case FBrush.Dash of
    TStrokeDash.Dash:
      begin
        SegSrcPos := GetEllipseTransfAt(SegInitDist);

        if IsLast and FLastDashExtend then
          TempDelta := FEllipseCircumf
        else
          TempDelta := SegInitDist + FThickness * 3.0;

        SegDestPos := GetEllipseTransfAt(TempDelta);

        SegDirVec := (SegDestPos - SegSrcPos).Normalize;
        ThickPerp := PointF(-SegDirVec.Y, SegDirVec.X) * FHalfThickness;

        InsertDash(SegSrcPos, SegDestPos, SegDirVec, ThickPerp);
      end;

    TStrokeDash.Dot:
      begin
        TempDelta := SegInitDist + FHalfThickness;

        SegSrcPos := GetEllipseTransfAt(TempDelta);

        SegDirVec := (GetEllipseTransfAt(TempDelta + FHalfThickness) - GetEllipseTransfAt(TempDelta -
          FHalfThickness)).Normalize;
        ThickPerp := PointF(-SegDirVec.Y, SegDirVec.X) * FHalfThickness;

        InsertDot(SegSrcPos, SegDirVec, ThickPerp);
      end;

    TStrokeDash.DashDot:
      begin
        SegSrcPos := GetEllipseTransfAt(SegInitDist);

        if IsLast and FLastDashExtend then
          TempDelta := FEllipseCircumf
        else
          TempDelta := SegInitDist + FThickness * 3.0;

        SegDestPos := GetEllipseTransfAt(TempDelta);

        SegDirVec := (SegDestPos - SegSrcPos).Normalize;
        ThickPerp := PointF(-SegDirVec.Y, SegDirVec.X) * FHalfThickness;

        InsertDash(SegSrcPos, SegDestPos, SegDirVec, ThickPerp);

        if (not IsLast) or (FExtraPieces > 0) then
        begin
          TempDelta := SegInitDist + FThickness * 4.5;

          SegSrcPos := GetEllipseTransfAt(TempDelta);

          SegDirVec := (GetEllipseTransfAt(TempDelta + FHalfThickness) - GetEllipseTransfAt(TempDelta -
            FHalfThickness)).Normalize;
          ThickPerp := PointF(-SegDirVec.Y, SegDirVec.X) * FHalfThickness;

          InsertDot(SegSrcPos, SegDirVec, ThickPerp);
        end;
      end;

    TStrokeDash.DashDotDot:
      begin
        SegSrcPos := GetEllipseTransfAt(SegInitDist);

        if IsLast and FLastDashExtend then
          TempDelta := FEllipseCircumf
        else
          TempDelta := SegInitDist + FThickness * 3;

        SegDestPos := GetEllipseTransfAt(TempDelta);

        SegDirVec := (SegDestPos - SegSrcPos).Normalize;
        ThickPerp := PointF(-SegDirVec.Y, SegDirVec.X) * FHalfThickness;

        InsertDash(SegSrcPos, SegDestPos, SegDirVec, ThickPerp);

        if (not IsLast) or (FExtraPieces > 0) then
        begin
          TempDelta := SegInitDist + FThickness * 4.5;

          SegSrcPos := GetEllipseTransfAt(TempDelta);

          SegDirVec := (GetEllipseTransfAt(TempDelta + FHalfThickness) - GetEllipseTransfAt(TempDelta -
            FHalfThickness)).Normalize;
          ThickPerp := PointF(-SegDirVec.Y, SegDirVec.X) * FHalfThickness;

          InsertDot(SegSrcPos, SegDirVec, ThickPerp);
        end;

        if (not IsLast) or (FExtraPieces > 1) then
        begin
          TempDelta := SegInitDist + FThickness * 6.5;

          SegSrcPos := GetEllipseTransfAt(TempDelta);

          SegDirVec := (GetEllipseTransfAt(TempDelta + FHalfThickness) - GetEllipseTransfAt(TempDelta -
            FHalfThickness)).Normalize;
          ThickPerp := PointF(-SegDirVec.Y, SegDirVec.X) * FHalfThickness;

          InsertDot(SegSrcPos, SegDirVec, ThickPerp);
        end;
      end;
  end;
end;

procedure TStrokeBuilder.BuildIntermEllipse(const Center, Radius: TPointF; const Opacity: Single);
var
  MajorAxis, MinorAxis, AxisSum, AxisSub, SubDivSumSq3: Single;
  StepSize, TentSegmentCount, SegInitDist: Single;
  CurScale: TPointF;
  PatternStepCount, TotalVertices, TotalIndices, StepIndex: Integer;
begin
  CurScale := GetMatrixScale;

  FThickness := FBrush.Thickness * (CurScale.X + CurScale.Y) * 0.5;
  FHalfThickness := FThickness * 0.5;
  FStrokeColor := PremultiplyAlpha(MakeColor(FBrush.Color, Opacity));
  FUndeterminedMode := False;

  FEllipseCenter := Center;
  FEllipseRadius := Radius;
  FEllipseTransfCenter := Center * FMatrix;

  if Radius.X > Radius.Y then
  begin
    MajorAxis := Radius.X * CurScale.X;
    MinorAxis := Radius.Y * CurScale.Y;
  end
  else
  begin
    MajorAxis := Radius.Y * CurScale.Y;
    MinorAxis := Radius.X * CurScale.X;
  end;

  AxisSum := MajorAxis + MinorAxis;
  AxisSub := MajorAxis - MinorAxis;

  if AxisSum <= 0 then
  begin
    InitArrays(0, 0);
    Exit;
  end;

  SubDivSumSq3 := 3 * Sqr(AxisSub / AxisSum);

  FEllipseCircumf := Pi * AxisSum * (1 + (SubDivSumSq3 / (10 + Sqrt(4 - SubDivSumSq3))));

  PatternStepCount := GetPatternStepCount;
  if PatternStepCount < 1 then
  begin
    InitArrays(0, 0);
    Exit;
  end;

  StepSize := FThickness * PatternStepCount;
  TentSegmentCount := FEllipseCircumf / StepSize;

  ComputeBuildEstimates(TentSegmentCount, TotalVertices, TotalIndices);
  if (FSegmentCount < 1) or (FEllipseCircumf < FThickness) then
  begin
    InitArrays(0, 0);
    Exit;
  end;

  InitArrays(TotalVertices, TotalIndices);

  SegInitDist := 0.0;

  for StepIndex := 0 to FSegmentCount - 1 do
  begin
    InsertEllipseSegment(SegInitDist, StepIndex = FSegmentCount - 1);
    SegInitDist := SegInitDist + StepSize;
  end;

  ArrayFillCheck;
end;

procedure TStrokeBuilder.BuildSolidEllipse(const Center, Radius: TPointF; const Opacity: Single);
var
  MajorAxis, MinorAxis, AxisSum, AxisSub, SubDivSumSq3: Single;
  StepSize, HalfStepSize, SegInitDist: Single;
  CurScale, SampleAt, SampleDirVec, ThickPerp: TPointF;
  TotalVertices, TotalIndices, StepIndex: Integer;
begin
  CurScale := GetMatrixScale;

  FThickness := FBrush.Thickness * (CurScale.X + CurScale.Y) * 0.5;
  FHalfThickness := FThickness * 0.5;
  FStrokeColor := PremultiplyAlpha(MakeColor(FBrush.Color, Opacity));
  FUndeterminedMode := False;

  FEllipseCenter := Center;
  FEllipseRadius := Radius;
  FEllipseTransfCenter := Center * FMatrix;

  if Radius.X > Radius.Y then
  begin
    MajorAxis := Radius.X * CurScale.X;
    MinorAxis := Radius.Y * CurScale.Y;
  end
  else
  begin
    MajorAxis := Radius.Y * CurScale.Y;
    MinorAxis := Radius.X * CurScale.X;
  end;

  AxisSum := MajorAxis + MinorAxis;
  AxisSub := MajorAxis - MinorAxis;

  if AxisSum <= 0 then
  begin
    InitArrays(0, 0);
    Exit;
  end;

  SubDivSumSq3 := 3 * Sqr(AxisSub / AxisSum);

  FEllipseCircumf := Pi * AxisSum * (1 + (SubDivSumSq3 / (10 + Sqrt(4 - SubDivSumSq3))));


  if MajorAxis > 100 then
    StepSize := FEllipseCircumf / (2.0 * Sqrt(FEllipseCircumf))
  else
    if MajorAxis > 50 then
      StepSize := MajorAxis * 0.1
    else
      StepSize := MajorAxis * 0.05;

  HalfStepSize := StepSize * 0.5;

  FSegmentCount := Round(FEllipseCircumf / StepSize);

  if (FSegmentCount < 1) or (FEllipseCircumf < FThickness) then
  begin
    InitArrays(0, 0);
    Exit;
  end;

  TotalVertices := FSegmentCount * 2;
  TotalIndices := FSegmentCount * 6;

  InitArrays(TotalVertices, TotalIndices);

  SegInitDist := 0;

  for StepIndex := 0 to FSegmentCount - 1 do
  begin
    SampleAt := GetEllipseTransfAt(SegInitDist);

    SampleDirVec := (GetEllipseTransfAt(SegInitDist + HalfStepSize) - GetEllipseTransfAt(SegInitDist -
      HalfStepSize)).Normalize;
    ThickPerp := PointF(-SampleDirVec.Y, SampleDirVec.X) * FHalfThickness;

    InsertVertex(SampleAt - ThickPerp, FStrokeColor);
    InsertVertex(SampleAt + ThickPerp, FStrokeColor);

    InsertIndex((StepIndex * 2 + 3) mod TotalVertices);
    InsertIndex((StepIndex * 2 + 1) mod TotalVertices);
    InsertIndex(StepIndex * 2);

    InsertIndex(StepIndex * 2);
    InsertIndex((StepIndex * 2 + 2) mod TotalVertices);
    InsertIndex((StepIndex * 2 + 3) mod TotalVertices);

    SegInitDist := SegInitDist + StepSize;
  end;

  ArrayFillCheck;
end;

procedure TStrokeBuilder.BuildIntermPolygon(const Points: TPolygon; const Opacity: Single; BreakAtEnd: Boolean);
var
  StepSize, Distance: Single;
  CurScale, SrcPos, DestPos, PieceDirVec, ThickPerp: TPointF;
  SrcPosValid, DestPosValid: Boolean;
  PatternStepCount, CurIndex, TempVertexCount, TempIndexCount: Integer;
begin
  if Length(Points) < 2 then
  begin
    InitArrays(0, 0);
    Exit;
  end;

  CurScale := GetMatrixScale;

  FThickness := FBrush.Thickness * (CurScale.X + CurScale.Y) * 0.5;
  FHalfThickness := FThickness * 0.5;
  FStrokeColor := PremultiplyAlpha(MakeColor(FBrush.Color, Opacity));

  FUndeterminedMode := True;
  InitArrayPointers;

  PatternStepCount := GetPatternStepCount;
  if PatternStepCount < 1 then
  begin
    InitArrays(0, 0);
    Exit;
  end;

  StepSize := FThickness * PatternStepCount;

  CurIndex := 0;

  SrcPosValid := False;
  DestPosValid := False;

  while CurIndex < Length(Points) do
  begin
    if (CurIndex >= Length(Points) - 1) and BreakAtEnd then
      Break;

    if not SrcPosValid then
    begin
      SrcPos := Points[CurIndex];

      if (SrcPos.X >= $FFFF) or (SrcPos.Y >= $FFFF) then
      begin
        DestPosValid := False;
        Inc(CurIndex);
        Continue;
      end;

      SrcPos := SrcPos * FMatrix;
    end
    else
      SrcPosValid := False;

    if not DestPosValid then
    begin
      DestPos := Points[(CurIndex + 1) mod Length(Points)];

      if (DestPos.X >= $FFFF) or (DestPos.Y >= $FFFF) then
      begin
        DestPos := Points[CurIndex];
        if (DestPos.X >= $FFFF) or (DestPos.Y >= $FFFF) then
        begin
          Inc(CurIndex);
          Continue;
        end;

        DestPos := DestPos * FMatrix;
      end
      else
        DestPos := DestPos * FMatrix;
    end
    else
      DestPosValid := False;

    Distance := DestPos.Distance(SrcPos);

    if Distance >= StepSize then
    begin
      PieceDirVec := (DestPos - SrcPos).Normalize;
      ThickPerp := TPointF.Create(-PieceDirVec.Y, PieceDirVec.X) * FHalfThickness;

      InsertSegment(SrcPos, PieceDirVec, ThickPerp, DestPos, False);

      SrcPos := SrcPos + (PieceDirVec * StepSize);

      SrcPosValid := True;
      DestPosValid := True;
      Continue;
    end;

    if (CurIndex = Length(Points) - 1) or (Points[CurIndex + 1].X >= $FFFF) or (Points[CurIndex + 1].Y >= $FFFF) or
      ((CurIndex < Length(Points) - 2) and (Points[CurIndex + 1].X < $FFFF) and (Points[CurIndex + 1].Y < $FFFF) and
      (Points[CurIndex + 2].X < $FFFF) and (Points[CurIndex + 2].Y < $FFFF) and
      (Points[CurIndex + 1].Distance(Points[CurIndex + 2]) > StepSize)) then
    begin
      ComputeBuildEstimates(1 + Distance / StepSize, TempVertexCount, TempIndexCount);

      if FSegmentCount > 1 then
      begin
        PieceDirVec := (DestPos - SrcPos).Normalize();
        ThickPerp := TPointF.Create(-PieceDirVec.Y, PieceDirVec.X) * FHalfThickness;

        InsertSegment(SrcPos, PieceDirVec, ThickPerp, DestPos, True);
      end;

      if CurIndex < Length(Points) - 1 then
      begin
        Inc(CurIndex);
        Continue;
      end
      else
        Break;
    end;

    SrcPosValid := True;
    Inc(CurIndex);
  end;

  FinalizeArrays;
end;

procedure TStrokeBuilder.BuildIntermPath(const Path: TPathData; const Opacity: Single);
var
  Points: TPolygon;
begin
  Path.FlattenToPolygon(Points, 1);
  BuildIntermPolygon(Points, Opacity, (Path.Count > 0) and (Path[Path.Count - 1].Kind <> TPathPointKind.Close));
end;

procedure TStrokeBuilder.BuildSolidPolygon(const Points: TPolygon; const Opacity: Single; BreakAtEnd: Boolean);
var
  StepSize, Distance: Single;
  CurScale, SrcPos, DestPos, PieceDirVec, ThickPerp: TPointF;
  SrcPosValid, DestPosValid, PrevVerticesPlaced: Boolean;
  CurIndex: Integer;
begin
  if Length(Points) < 2 then
  begin
    InitArrays(0, 0);
    Exit;
  end;

  CurScale := GetMatrixScale;

  FThickness := FBrush.Thickness * (CurScale.X + CurScale.Y) * 0.5;
  FHalfThickness := FThickness * 0.5;
  FStrokeColor := PremultiplyAlpha(MakeColor(FBrush.Color, Opacity));

  FUndeterminedMode := True;
  InitArrayPointers;

  //https://quality.embarcadero.com/browse/RSP-28136
  //StepSize := FThickness;
  //if StepSize < 2 then
  //  StepSize := 2;
  StepSize := 1;

  CurIndex := 0;

  SrcPosValid := False;
  DestPosValid := False;
  PrevVerticesPlaced := False;

  while CurIndex < Length(Points) do
  begin
    if (CurIndex >= Length(Points) - 1) and BreakAtEnd and (Points[0] <> Points[Length(Points) - 1]) then
      Break;

    if not SrcPosValid then
    begin
      SrcPos := Points[CurIndex];

      if (SrcPos.X >= $FFFF) or (SrcPos.Y >= $FFFF) then
      begin
        DestPosValid := False;
        PrevVerticesPlaced := False;
        Inc(CurIndex);
        Continue;
      end;

      SrcPos := SrcPos * FMatrix;
    end
    else
      SrcPosValid := False;

    if not DestPosValid then
    begin
      DestPos := Points[(CurIndex + 1) mod Length(Points)];

      if (DestPos.X >= $FFFF) or (DestPos.Y >= $FFFF) then
      begin
        DestPos := Points[CurIndex];
        if (DestPos.X >= $FFFF) or (DestPos.Y >= $FFFF) then
        begin
          PrevVerticesPlaced := False;
          Inc(CurIndex);
          Continue;
        end;

        DestPos := DestPos * FMatrix;
      end
      else
        DestPos := DestPos * FMatrix;
    end
    else
      DestPosValid := False;

    Distance := DestPos.Distance(SrcPos);

    if Distance >= StepSize then
    begin
      PieceDirVec := (DestPos - SrcPos).Normalize;
      ThickPerp := TPointF.Create(-PieceDirVec.Y, PieceDirVec.X) * FHalfThickness;

      InsertVertex(SrcPos - ThickPerp, FStrokeColor);
      InsertVertex(SrcPos + ThickPerp, FStrokeColor);

      if PrevVerticesPlaced then
      begin
        InsertIndex(FCurrentVertex - 3);
        InsertIndex(FCurrentVertex - 1);
        InsertIndex(FCurrentVertex - 2);

        InsertIndex(FCurrentVertex - 2);
        InsertIndex(FCurrentVertex - 4);
        InsertIndex(FCurrentVertex - 3);
      end;

      PrevVerticesPlaced := True;

      SrcPos := SrcPos + (PieceDirVec * StepSize);

      SrcPosValid := True;
      DestPosValid := True;
      Continue;
    end;

    if ((CurIndex < Length(Points) - 2) and (Points[CurIndex + 1].X < $FFFF) and (Points[CurIndex + 1].Y < $FFFF) and
      (Points[CurIndex + 2].X < $FFFF) and (Points[CurIndex + 2].Y < $FFFF) and
      (Points[CurIndex + 1].Distance(Points[CurIndex + 2]) > StepSize)) then
    begin
      PieceDirVec := (DestPos - SrcPos).Normalize;
      ThickPerp := TPointF.Create(-PieceDirVec.Y, PieceDirVec.X) * FHalfThickness;

      InsertVertex(DestPos - ThickPerp, FStrokeColor);
      InsertVertex(DestPos + ThickPerp, FStrokeColor);

      if PrevVerticesPlaced then
      begin
        InsertIndex(FCurrentVertex - 3);
        InsertIndex(FCurrentVertex - 1);
        InsertIndex(FCurrentVertex - 2);

        InsertIndex(FCurrentVertex - 2);
        InsertIndex(FCurrentVertex - 4);
        InsertIndex(FCurrentVertex - 3);
      end;

      if CurIndex < Length(Points) - 1 then
      begin
        Inc(CurIndex);
        Continue;
      end
      else
        Break;
    end;

    if (CurIndex = Length(Points) - 1) or (Points[CurIndex + 1].X >= $FFFF) or (Points[CurIndex + 1].Y >= $FFFF) then
    begin
      PieceDirVec := (DestPos - SrcPos).Normalize;
      ThickPerp := TPointF.Create(-PieceDirVec.Y, PieceDirVec.X) * FHalfThickness;

      InsertVertex(DestPos - ThickPerp, FStrokeColor);
      InsertVertex(DestPos + ThickPerp, FStrokeColor);

      if PrevVerticesPlaced then
      begin
        InsertIndex(FCurrentVertex - 3);
        InsertIndex(FCurrentVertex - 1);
        InsertIndex(FCurrentVertex - 2);

        InsertIndex(FCurrentVertex - 2);
        InsertIndex(FCurrentVertex - 4);
        InsertIndex(FCurrentVertex - 3);
      end;

      PrevVerticesPlaced := False;

      if CurIndex < Length(Points) - 1 then
      begin
        Inc(CurIndex);
        Continue;
      end
      else
        Break;
    end;

    SrcPosValid := True;
    Inc(CurIndex);
  end;

  FinalizeArrays;
end;

procedure TStrokeBuilder.BuildSolidPath(const Path: TPathData; const Opacity: Single);
var
  Points: TPolygon;
begin
  Path.FlattenToPolygon(Points, 1);
  BuildSolidPolygon(Points, Opacity, (Path.Count > 0) and (Path[Path.Count - 1].Kind <> TPathPointKind.Close));
end;

{$ENDREGION}

end.
