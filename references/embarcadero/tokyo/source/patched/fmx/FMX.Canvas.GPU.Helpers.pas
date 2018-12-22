{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Canvas.GPU.Helpers;

{ FireMonkey NextGen Canvas helper that provides low-level drawing interface. }

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.UITypes, FMX.Types, FMX.Types3D, FMX.Materials.Canvas, FMX.Graphics, 
  System.Math.Vectors, ALFMXTypes3D;

type
  TDrawingMode = (Normal, WriteStencilInvert, ReadStencil, ClearStencil);

  TDrawingModeHelper = record helper for TDrawingMode
  const
    dmNormal = TDrawingMode.Normal deprecated 'Use TDrawingMode.Normal';
    dmWriteStencilInvert = TDrawingMode.WriteStencilInvert deprecated 'Use TDrawingMode.WriteStencilInvert';
    dmReadStencil = TDrawingMode.ReadStencil deprecated 'Use TDrawingMode.ReadStencil';
    dmClearStencil = TDrawingMode.ClearStencil deprecated 'Use TDrawingMode.ClearStencil';
  end;
  TCanvasHelper = class
  public type
    TVertexArray = array of TPointF;
    TAlphaColorArray = array of TAlphaColor;
    TIndexArray = array of Integer;
    TTransformCallback = procedure(var Position: TPointF) of object;
  private const
    ZeroPt: TPointF = (X: 0; Y: 0);
    MaxBatchedVertices = 8192;
    MaxBatchedIndices = 12288;
  private type
    PVertexBufferItem = ^TVertexBufferItem;
    TVertexBufferItem = packed record
      Position: TPoint3D;
      TexCoord: TPointF;
      VtxColor: TAlphaColor;
    end;
    TIndexBufferItem = Word;
    TBatchVertexBuffer = packed array [0 .. MaxBatchedVertices - 1] of TVertexBufferItem;
    TBatchIndexBuffer = packed array [0 .. MaxBatchedIndices - 1] of TIndexBufferItem;
    TBatchingTopology = (None, Pixels, Lines, Triangles);
  private
    FCurrentTexture: TTexture;
    FCurrentTopology: TBatchingTopology;
    FCurrentMaterial: TMaterial;
    FBatchedVertices: Integer;
    FBatchedIndices: Integer;
    FBatchVertexBuffer: TBatchVertexBuffer;
    FBatchIndexBuffer: TBatchIndexBuffer;
    FFlushCountPerFrame: Integer;
    FTexMat: TCanvasTextureMaterial;
    FExternalOESTexMat: TALCanvasExternalOESTextureMaterial; // << https://quality.embarcadero.com/browse/RSP-16830
    F420YpCbCr8BiPlanarVideoRangeTexMat: TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial; // https://quality.embarcadero.com/browse/RSP-22947
    F420YpCbCr8PlanarTexMat: TALCanvas420YpCbCr8PlanarTextureMaterial; // https://quality.embarcadero.com/browse/RSP-22947
    FSolidMat: TCanvasSolidMaterial;
    FGradientMat: TCanvasGradientMaterial;
    [Weak] FContext: TContext3D;

    FDrawingMode: TDrawingMode;

    TempVertices4: TVertexArray;
    TempTexCoords4: TVertexArray;
    TempColors4: TAlphaColorArray;
    TempIndices6: TIndexArray;
    FPrimitiveCountPerFrame: Integer;

    procedure SetDrawingMode(const Value: TDrawingMode);
    procedure InsertIndex(const Value: Integer);
    procedure InsertVertex(const Position, TexCoord: TPointF;
      const Color: TAlphaColor);
    procedure ResetBatchingStatus(const Topology: TBatchingTopology; const Texture: TTexture;
      const Material: TMaterial);
    procedure UpdateBatchingStatus(const Topology: TBatchingTopology; const MinVertexCount, MinIndexCount: Integer;
      const Texture: TTexture; const Material: TMaterial);
    procedure UpdateScissorRect;
    procedure DrawBuffers;
    function CalculateTopLeft(const ARect: TRectF; const TransformCallback: TTransformCallback): TPointF;

    procedure FillTrianglesMultiBatch(const Vertices: TVertexArray; const Colors: TAlphaColorArray;
      const Indices: TIndexArray; const VertexCount, PrimitiveCount: Integer);
  protected
    // Scissor rectangle that limits drawing to certain portion of the screen.
    FScissorRect: TRect;

    // Implements actual code that changes the scissor rectangle.
    procedure SetScissorRect(const Value: TRect);

    // Does the actual clearing of the stencil buffer on hardware device.
    procedure DoClearStencil(const StencilValue: Cardinal);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure BeginRender;
    procedure EndRender;

    procedure UpdateDrawingMode;
    procedure Flush;

    procedure FillTriangles(const Vertices: TVertexArray; const Colors: TAlphaColorArray; const Indices: TIndexArray;
      const VertexCount, PrimitiveCount: Integer);

    procedure FillRect(const Corners: TCornersF; const Color1, Color2, Color3, Color4: TAlphaColor); overload;
    procedure FillRect(const Corners: TCornersF; const Color: TAlphaColor); overload;
    procedure FillRect(const X, Y, Width, Height: Single; const Color: TAlphaColor); overload;

    procedure FillEllipse(const ARect: TRectF; const AColor: TAlphaColor;
      const TransformCallback: TTransformCallback = nil);
    procedure FillQuad(const P1, P2, P3, P4: TPointF; const AColor: TAlphaColor);

    procedure TexTriangles(const Texture: TTexture; const Vertices: TVertexArray; const TexCoords: TVertexArray;
      const Colors: TAlphaColorArray; const Indices: TIndexArray; const VertexCount, PrimitiveCount: Integer);

    procedure TexRect(const DestCorners, SrcCorners: TCornersF; const Texture: TTexture;
      const Color1, Color2, Color3, Color4: TAlphaColor); overload;
    procedure TexRect(const DestCorners, SrcCorners: TCornersF; const Texture: TTexture; const Color: TAlphaColor); overload;
    procedure TexRect(const Corners: TCornersF; const Texture: TTexture; const Color: TAlphaColor); overload;
    procedure TexRect(const X, Y, Width, Height: Single; const Texture: TTexture; const Color: TAlphaColor); overload;

    procedure TexQuad(const P1, P2, P3, P4, TC1, TC2, TC3, TC4: TPointF;
      const AColor: TAlphaColor; const ATexture: TTexture);

    procedure TexEllipse(const ARect: TRectF; const AColor: TAlphaColor; const ATexture: TTexture;
      const TransformCallback: TTransformCallback = nil); overload;
    procedure TexEllipse(const ARect, ATexCoord: TRectF; const AColor: TAlphaColor; const ATexture: TTexture;
      const TransformCallback: TTransformCallback = nil); overload;

    procedure GradientRect(const DestCorners: TCornersF; const AGradient: TGradient); overload;
    procedure GradientEllipse(const ARect: TRectF; const AGradient: TGradient;
      const TransformCallback: TTransformCallback = nil);
    procedure GradientQuad(const P1, P2, P3, P4: TPointF;
      const AGradient: TGradient);
    procedure GradientTriangles(const AGradient: TGradient; const Vertices: TVertexArray; const TexCoords: TVertexArray;
      const Indices: TIndexArray; const VertexCount, PrimitiveCount: Integer);

    procedure ResetScissorRect;
    procedure SetScissorRectWithoutUpdate(const R: TRect);
    procedure SetContext(const Context: TContext3D);

    property ScissorRect: TRect read FScissorRect write SetScissorRect;
    property DrawingMode: TDrawingMode read FDrawingMode write SetDrawingMode;
    property FlushCountPerFrame: Integer read FFlushCountPerFrame;
    property PrimitiveCountPerFrame: Integer read FPrimitiveCountPerFrame;
  end;

implementation

uses
  System.Math, System.Generics.Collections, FMX.Consts, System.TypInfo;

constructor TCanvasHelper.Create;
begin
  inherited;

  SetLength(TempVertices4, 4);
  SetLength(TempTexCoords4, 4);
  SetLength(TempColors4, 4);
  SetLength(TempIndices6, 6);

  FTexMat := TCanvasTextureMaterial.Create;
  FExternalOESTexMat := TALCanvasExternalOESTextureMaterial.Create; // << https://quality.embarcadero.com/browse/RSP-16830
  F420YpCbCr8BiPlanarVideoRangeTexMat := TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial.create; // https://quality.embarcadero.com/browse/RSP-22947
  F420YpCbCr8PlanarTexMat := TALCanvas420YpCbCr8PlanarTextureMaterial.create; // https://quality.embarcadero.com/browse/RSP-22947
  FSolidMat := TCanvasSolidMaterial.Create;
  FGradientMat := TCanvasGradientMaterial.Create;
end;

destructor TCanvasHelper.Destroy;
begin
  FGradientMat.Free;
  FTexMat.Free;
  FExternalOESTexMat.Free; // << https://quality.embarcadero.com/browse/RSP-16830
  F420YpCbCr8BiPlanarVideoRangeTexMat.free; // << https://quality.embarcadero.com/browse/RSP-22947
  F420YpCbCr8PlanarTexMat.free; // << https://quality.embarcadero.com/browse/RSP-22947
  FSolidMat.Free;

  inherited;
end;

procedure TCanvasHelper.DoClearStencil(const StencilValue: Cardinal);
begin

end;

procedure TCanvasHelper.SetDrawingMode(const Value: TDrawingMode);
begin
  if FDrawingMode <> Value then
  begin
    Flush;

    FDrawingMode := Value;
    UpdateDrawingMode;
  end;
end;

procedure TCanvasHelper.ResetScissorRect;
begin
  Flush;

  if FContext <> nil then
    FContext.SetContextState(TContextState.csScissorOff);
end;

procedure TCanvasHelper.UpdateScissorRect;
begin
  Flush;

  if FContext <> nil then
  begin
    FContext.SetScissorRect(FScissorRect);
    FContext.SetContextState(TContextState.csScissorOn);
  end;
end;

procedure TCanvasHelper.SetScissorRect(const Value: TRect);
var
  NewValue: TRect;
begin
  if FContext <> nil then
  begin
    NewValue := Value;

    if (NewValue.Left < 0) then
      NewValue.Left := 0;
    if (NewValue.Top < 0) then
      NewValue.Top := 0;

    if (NewValue.Right > FContext.Width) then
      NewValue.Right := FContext.Width;

    if (NewValue.Bottom > FContext.Height) then
      NewValue.Bottom := FContext.Height;

    if (NewValue.Right < NewValue.Left) then
      NewValue.Right := NewValue.Left;
    if (NewValue.Bottom < NewValue.Top) then
      NewValue.Bottom := NewValue.Top;

    FScissorRect := NewValue;
    UpdateScissorRect;
  end;
end;

procedure TCanvasHelper.SetScissorRectWithoutUpdate(const R: TRect);
begin
  FScissorRect := R;
end;

procedure TCanvasHelper.BeginRender;
begin
  if FContext <> nil then
  begin
    FContext.SetMatrix(TMatrix3D.Identity);
    FContext.SetContextState(TContextState.cs2DScene);
    FContext.SetContextState(TContextState.csAllFace);
    FContext.SetContextState(TContextState.csZWriteOff);
    FContext.SetContextState(TContextState.csZTestOff);
    FContext.SetContextState(TContextState.csScissorOff);
  end;

  FFlushCountPerFrame := 0;
  FScissorRect := TRect.Create(0, 0, 0, 0);
  FPrimitiveCountPerFrame := 0;
  FCurrentTopology := TBatchingTopology.None;
  FCurrentTexture := nil;
  FCurrentMaterial := nil;
  FBatchedVertices := 0;
  FBatchedIndices := 0;
  FDrawingMode := TDrawingMode.Normal;

  UpdateDrawingMode;
end;

procedure TCanvasHelper.EndRender;
begin
  FContext := nil;
end;

procedure TCanvasHelper.FillRect(const Corners: TCornersF; const Color1, Color2, Color3, Color4: TAlphaColor);
begin
  TempVertices4[0] := Corners[0];
  TempVertices4[1] := Corners[1];
  TempVertices4[2] := Corners[2];
  TempVertices4[3] := Corners[3];

  TempColors4[0] := Color1;
  TempColors4[1] := Color2;
  TempColors4[2] := Color3;
  TempColors4[3] := Color4;

  TempIndices6[0] := 0;
  TempIndices6[1] := 2;
  TempIndices6[2] := 3;
  TempIndices6[3] := 0;
  TempIndices6[4] := 1;
  TempIndices6[5] := 2;

  FillTriangles(TempVertices4, TempColors4, TempIndices6, 4, 2);
end;

procedure TCanvasHelper.FillRect(const Corners: TCornersF; const Color: TAlphaColor);
begin
  FillRect(Corners, Color, Color, Color, Color);
end;

procedure TCanvasHelper.FillRect(const X, Y, Width, Height: Single; const Color: TAlphaColor);
begin
  FillRect(CornersF(X, Y, Width, Height), Color, Color, Color, Color);
end;

procedure TCanvasHelper.UpdateBatchingStatus(const Topology: TBatchingTopology; const MinVertexCount,
  MinIndexCount: Integer; const Texture: TTexture; const Material: TMaterial);
var
  BatchInvalid: Boolean;
  M: TMaterial;
begin
  if (MinVertexCount > MaxBatchedVertices) or (MinIndexCount > MaxBatchedIndices) then
    raise EInvalidCallingConditions.CreateResFmt(@SInvalidCallingConditions, [ClassName]);

  if Material <> nil then
    M := Material
  else if Texture <> nil then begin
    if (Texture is TalTexture) and TalTexture(Texture).isExternalOES then M := FExternalOESTexMat  // << https://quality.embarcadero.com/browse/RSP-16830
    else if (Texture is TALBiPlanarTexture) and (TALBiPlanarTexture(Texture).format = TALTextureFormat.f420YpCbCr8BiPlanarVideoRange) then M := F420YpCbCr8BiPlanarVideoRangeTexMat  // << https://quality.embarcadero.com/browse/RSP-22947
    else if (Texture is TALPlanarTexture) and (TALPlanarTexture(Texture).format = TALTextureFormat.f420YpCbCr8Planar) then M := F420YpCbCr8PlanarTexMat  // << https://quality.embarcadero.com/browse/RSP-22947
    else M := FTexMat
  end
  else
    M := FSolidMat;

  BatchInvalid := (FBatchedVertices + MinVertexCount >= MaxBatchedVertices) or
    (FBatchedIndices + MinIndexCount >= MaxBatchedIndices) or
    (FCurrentTopology = TBatchingTopology.None) or (FCurrentTopology <> Topology) or
    (FCurrentTexture <> Texture) or (FCurrentMaterial <> M);

  if BatchInvalid then
    ResetBatchingStatus(Topology, Texture, M);
end;

procedure TCanvasHelper.ResetBatchingStatus(const Topology: TBatchingTopology;
  const Texture: TTexture; const Material: TMaterial);
begin
  Flush;

  FCurrentMaterial := Material;
  FCurrentTexture := Texture;
  FCurrentTopology := Topology;
end;

procedure TCanvasHelper.Flush;
begin
  try
    if (FContext <> nil) and (FBatchedVertices > 0) then
    begin
      DrawBuffers;
      Inc(FFlushCountPerFrame);
    end;
  finally
    FBatchedVertices := 0;
    FBatchedIndices := 0;
    FCurrentTexture := nil;
    FCurrentMaterial := nil;
  end;
end;

procedure TCanvasHelper.UpdateDrawingMode;
begin
  if FContext <> nil then
  begin
    case DrawingMode of
      TDrawingMode.Normal:
        begin
          FContext.SetContextState(TContextState.csStencilOff);
          FContext.SetContextState(TContextState.csColorWriteOn);
        end;
      TDrawingMode.WriteStencilInvert:
        begin
          FContext.SetContextState(TContextState.csStencilOn);
          FContext.SetStencilFunc(TStencilFunc.Always, 0, $FF);
          FContext.SetStencilOp(TStencilOp.Keep, TStencilOp.Keep, TStencilOp.Invert);

          FContext.SetContextState(TContextState.csColorWriteOff);
        end;
      TDrawingMode.ReadStencil:
        begin
          FContext.SetContextState(TContextState.csStencilOn);
          FContext.SetStencilFunc(TStencilFunc.NotEqual, 0, $FF);
          FContext.SetStencilOp(TStencilOp.Keep, TStencilOp.Keep, TStencilOp.Keep);

          FContext.SetContextState(TContextState.csColorWriteOn);
        end;
      TDrawingMode.ClearStencil:
        begin
          FContext.SetContextState(TContextState.csStencilOn);
          FContext.SetStencilFunc(TStencilFunc.Always, 0, $FF);
          FContext.SetStencilOp(TStencilOp.Zero, TStencilOp.Zero, TStencilOp.Zero);

          FContext.SetContextState(TContextState.csColorWriteOff);
        end;
    end;

    if DrawingMode <> TDrawingMode.WriteStencilInvert then
      FContext.SetContextState(TContextState.csAlphaBlendOn)
    else
      FContext.SetContextState(TContextState.csAlphaBlendOff);
  end;
end;

procedure TCanvasHelper.InsertVertex(const Position, TexCoord: TPointF; const Color: TAlphaColor);
begin
  FBatchVertexBuffer[FBatchedVertices].Position.X := Position.X;
  FBatchVertexBuffer[FBatchedVertices].Position.Y := Position.Y;

  if (FCurrentTexture <> nil) and (FContext <> nil) and (TTextureStyle.RenderTarget in FCurrentTexture.Style) and
    (TContextStyle.RenderTargetFlipped in FContext.Style) then
    FBatchVertexBuffer[FBatchedVertices].TexCoord := TPointF.Create(TexCoord.X, 1 - TexCoord.Y)
  else
    FBatchVertexBuffer[FBatchedVertices].TexCoord := TexCoord;
  if FContext <> nil then
    AlphaColorToPixel(Color, @FBatchVertexBuffer[FBatchedVertices].VtxColor, FContext.PixelFormat)
  else
    FBatchVertexBuffer[FBatchedVertices].VtxColor := Color;

  Inc(FBatchedVertices);
end;

procedure TCanvasHelper.InsertIndex(const Value: Integer);
begin
  FBatchIndexBuffer[FBatchedIndices] := Value;
  Inc(FBatchedIndices);
end;

procedure TCanvasHelper.DrawBuffers;
var
  SolidDecl: TVertexDeclaration;
begin
  if (FCurrentMaterial = FTexMat) or
     (FCurrentMaterial = FExternalOESTexMat) or // << https://quality.embarcadero.com/browse/RSP-16830
     (FCurrentMaterial = F420YpCbCr8BiPlanarVideoRangeTexMat) or // << https://quality.embarcadero.com/browse/RSP-22947
     (FCurrentMaterial = F420YpCbCr8PlanarTexMat) then begin // << https://quality.embarcadero.com/browse/RSP-22947
    SetLength(SolidDecl, 3);
    SolidDecl[0].Format := TVertexFormat.Vertex;
    SolidDecl[0].Offset := 0;
    SolidDecl[1].Format := TVertexFormat.TexCoord0;
    SolidDecl[1].Offset := 12;
    SolidDecl[2].Format := TVertexFormat.Color0;
    SolidDecl[2].Offset := 20;
    if (FCurrentMaterial = FTexMat) then FTexMat.Texture := FCurrentTexture
    else if (FCurrentMaterial = FExternalOESTexMat) then FExternalOESTexMat.Texture := FCurrentTexture  // << https://quality.embarcadero.com/browse/RSP-16830
    else if (FCurrentMaterial = F420YpCbCr8BiPlanarVideoRangeTexMat) then begin
      F420YpCbCr8BiPlanarVideoRangeTexMat.Texture := FCurrentTexture;  // << https://quality.embarcadero.com/browse/RSP-22947
      F420YpCbCr8BiPlanarVideoRangeTexMat.CBCRTexture := TALBiplanarTexture(FCurrentTexture).SecondTexture; // << https://quality.embarcadero.com/browse/RSP-22947
    end
    else begin
      F420YpCbCr8PlanarTexMat.Texture := FCurrentTexture;  // << https://quality.embarcadero.com/browse/RSP-22947
      F420YpCbCr8PlanarTexMat.CBTexture := TALplanarTexture(FCurrentTexture).SecondTexture; // << https://quality.embarcadero.com/browse/RSP-22947
      F420YpCbCr8PlanarTexMat.CRTexture := TALplanarTexture(FCurrentTexture).ThirdTexture; // << https://quality.embarcadero.com/browse/RSP-22947
    end;
    FContext.DrawPrimitives(TPrimitivesKind.Triangles, @FBatchVertexBuffer[0], @FBatchIndexBuffer[0], SolidDecl,
      SizeOf(TVertexBufferItem), FBatchedVertices, SizeOf(TIndexBufferItem), FBatchedIndices, FCurrentMaterial, 1);
  end else if FCurrentMaterial <> FSolidMat then
  begin
    SetLength(SolidDecl, 3);
    SolidDecl[0].Format := TVertexFormat.Vertex;
    SolidDecl[0].Offset := 0;
    SolidDecl[1].Format := TVertexFormat.TexCoord0;
    SolidDecl[1].Offset := 12;
    SolidDecl[2].Format := TVertexFormat.Color0;
    SolidDecl[2].Offset := 20;
    FContext.DrawPrimitives(TPrimitivesKind.Triangles, @FBatchVertexBuffer[0], @FBatchIndexBuffer[0], SolidDecl,
      SizeOf(TVertexBufferItem), FBatchedVertices, SizeOf(TIndexBufferItem), FBatchedIndices, FCurrentMaterial, 1);
  end else begin
    SetLength(SolidDecl, 2);
    SolidDecl[0].Format := TVertexFormat.Vertex;
    SolidDecl[0].Offset := 0;
    SolidDecl[1].Format := TVertexFormat.Color0;
    SolidDecl[1].Offset := 20;
    FContext.DrawPrimitives(TPrimitivesKind.Triangles, @FBatchVertexBuffer[0], @FBatchIndexBuffer[0], SolidDecl,
      SizeOf(TVertexBufferItem), FBatchedVertices, SizeOf(TIndexBufferItem), FBatchedIndices, FCurrentMaterial, 1);
  end;
end;

procedure TCanvasHelper.TexRect(const DestCorners, SrcCorners: TCornersF; const Texture: TTexture;
  const Color1, Color2, Color3, Color4: TAlphaColor);
var
  InvTexSize: TPointF;
begin
  if (Texture = nil) or (Texture.Width < 1) or (Texture.Height < 1) then
    Exit;

  TempVertices4[0] := DestCorners[0];
  TempVertices4[1] := DestCorners[1];
  TempVertices4[2] := DestCorners[2];
  TempVertices4[3] := DestCorners[3];

  InvTexSize := PointF(1 / Texture.Width, 1 / Texture.Height);

  TempTexCoords4[0] := SrcCorners[0] * InvTexSize;
  TempTexCoords4[1] := SrcCorners[1] * InvTexSize;
  TempTexCoords4[2] := SrcCorners[2] * InvTexSize;
  TempTexCoords4[3] := SrcCorners[3] * InvTexSize;

  TempColors4[0] := Color1;
  TempColors4[1] := Color2;
  TempColors4[2] := Color3;
  TempColors4[3] := Color4;

  TempIndices6[0] := 0;
  TempIndices6[1] := 2;
  TempIndices6[2] := 3;
  TempIndices6[3] := 0;
  TempIndices6[4] := 1;
  TempIndices6[5] := 2;

  TexTriangles(Texture, TempVertices4, TempTexCoords4, TempColors4, TempIndices6, 4, 2);
end;

procedure TCanvasHelper.TexRect(const DestCorners, SrcCorners: TCornersF; const Texture: TTexture;
  const Color: TAlphaColor);
begin
  TexRect(DestCorners, SrcCorners, Texture, Color, Color, Color, Color);
end;

procedure TCanvasHelper.TexRect(const Corners: TCornersF; const Texture: TTexture; const Color: TAlphaColor);
begin
  if Texture = nil then
    Exit;

  TexRect(Corners, CornersF(0, 0, Texture.Width, Texture.Height), Texture, Color, Color, Color, Color);
end;

procedure TCanvasHelper.TexRect(const X, Y, Width, Height: Single; const Texture: TTexture; const Color: TAlphaColor);
begin
  TexRect(CornersF(X, Y, Width, Height), Texture, Color);
end;

procedure TCanvasHelper.SetContext(const Context: TContext3D);
begin
  FContext := Context;
  if FContext <> nil then
    FContext.SetContextState(TContextState.csScissorOff);
end;

function GetCircleSubdivCount(const ARect: TRectF): integer;
const
  MinFlatDistance = 8;
var
  CircleRadius: Single;
  {SubdivCount,} MinSubDivCount: Integer; // https://quality.embarcadero.com/browse/RSP-19665
begin
  CircleRadius := Max(ARect.Width, ARect.Height) / 2.0;
  if CircleRadius <= 50 then
    MinSubDivCount := 24
  else
    MinSubDivCount := 12;

  Result := Max(Ceil(2 * Pi * CircleRadius / MinFlatDistance), MinSubDivCount);
end;

procedure TCanvasHelper.FillEllipse(const ARect: TRectF; const AColor: TAlphaColor;
                                    const TransformCallback: TTransformCallback);
var
  Vertices: TVertexArray;
  Colors: TAlphaColorArray;
  Indices: TIndexArray;
  Index, SubdivCount: Integer;
  Angle: Single;
  Radius: TPointF;
  Center, CurPt: TPointF;
begin
  Radius.X := ARect.Width / 2;
  Radius.Y := ARect.Height / 2;

  SubdivCount := GetCircleSubdivCount(ARect);

  Center.X := (ARect.Left + ARect.Right) / 2;
  Center.Y := (ARect.Top + ARect.Bottom) / 2;

  SetLength(Vertices, 1 + SubdivCount);
  SetLength(Colors, 1 + SubdivCount);
  SetLength(Indices, SubdivCount * 3);

  CurPt := Center;

  if Assigned(TransformCallback) then
    TransformCallback(CurPt);

  Vertices[0] := CurPt;
  Colors[0] := AColor;

  for Index := 0 to SubdivCount - 1 do
  begin
    Angle := Index * 2 * Pi / SubdivCount;

    CurPt.X := Cos(Angle) * Radius.X;
    CurPt.Y := Sin(Angle) * Radius.Y;

    CurPt.Offset(Center);

    if Assigned(TransformCallback) then
      TransformCallback(CurPt);

    Vertices[1 + Index] := CurPt;
    Colors[1 + Index] := AColor;

    Indices[(Index * 3) + 0] := 0;
    Indices[(Index * 3) + 1] := 1 + Index;
    Indices[(Index * 3) + 2] := 1 + ((1 + Index) mod SubdivCount);
  end;

  FillTriangles(Vertices, Colors, Indices, Length(Vertices), SubdivCount);
end;

procedure TCanvasHelper.FillQuad(const P1, P2, P3, P4: TPointF; const AColor: TAlphaColor);
begin
  UpdateBatchingStatus(TBatchingTopology.Triangles, 4, 6, nil, nil);

  InsertIndex(FBatchedVertices + 0);
  InsertIndex(FBatchedVertices + 1);
  InsertIndex(FBatchedVertices + 3);
  InsertIndex(FBatchedVertices + 3);
  InsertIndex(FBatchedVertices + 1);
  InsertIndex(FBatchedVertices + 2);

  InsertVertex(P1, ZeroPt, AColor);
  InsertVertex(P2, ZeroPt, AColor);
  InsertVertex(P3, ZeroPt, AColor);
  InsertVertex(P4, ZeroPt, AColor);
end;

procedure TCanvasHelper.FillTrianglesMultiBatch(const Vertices: TVertexArray; const Colors: TAlphaColorArray;
  const Indices: TIndexArray; const VertexCount, PrimitiveCount: Integer);
var
  CurVertices: TList<TPointF>;
  CurColors: TList<TAlphaColor>;
  CurIndices: TList<Integer>;
  VertexMap: TDictionary<Integer, Integer>;

  function RemapVertex(const SourceIndex: Integer): Integer;
  begin
    if not VertexMap.TryGetValue(SourceIndex, Result) then
    begin
      Result := CurVertices.Add(Vertices[SourceIndex]);
      CurColors.Add(Colors[SourceIndex]);
      VertexMap.Add(SourceIndex, Result);
    end;
  end;

  procedure FlushBuffer;
  var
    I: Integer;
  begin
    UpdateBatchingStatus(TBatchingTopology.Triangles, CurVertices.Count, CurIndices.Count, nil, nil);

    for I := 0 to CurIndices.Count - 1 do
      InsertIndex(FBatchedVertices + CurIndices[I]);

    for I := 0 to CurVertices.Count - 1 do
      InsertVertex(CurVertices[I], ZeroPt, CurColors[I]);

    Inc(FPrimitiveCountPerFrame, CurIndices.Count div 3);

    CurVertices.Clear;
    CurColors.Clear;
    CurIndices.Clear;
    VertexMap.Clear;
  end;

var
  I: Integer;
begin
  CurVertices := TList<TPointF>.Create;
  CurColors := TList<TAlphaColor>.Create;
  CurIndices := TList<Integer>.Create;
  VertexMap := TDictionary<Integer, Integer>.Create;

  try
    for I := 0 to PrimitiveCount - 1 do
    begin
      CurIndices.Add(RemapVertex(Indices[I * 3]));
      CurIndices.Add(RemapVertex(Indices[(I * 3) + 1]));
      CurIndices.Add(RemapVertex(Indices[(I * 3) + 2]));

      if (CurVertices.Count >= MaxBatchedVertices - 2) or (CurIndices.Count >= MaxBatchedIndices - 2) then
        FlushBuffer;
    end;

    if CurIndices.Count > 0 then
      FlushBuffer;
  finally
    VertexMap.Free;
    CurIndices.Free;
    CurColors.Free;
    CurVertices.Free;
  end;
end;

procedure TCanvasHelper.FillTriangles(const Vertices: TVertexArray; const Colors: TAlphaColorArray;
  const Indices: TIndexArray; const VertexCount, PrimitiveCount: Integer);
var
  Index: Integer;
begin
  if (VertexCount < MaxBatchedVertices) and (PrimitiveCount * 3 < MaxBatchedIndices) then
  begin
    UpdateBatchingStatus(TBatchingTopology.Triangles, VertexCount, PrimitiveCount * 3, nil, nil);

    for Index := 0 to (PrimitiveCount * 3) - 1 do
      InsertIndex(FBatchedVertices + Indices[Index]);

    for Index := 0 to VertexCount - 1 do
      InsertVertex(Vertices[Index], ZeroPt, Colors[Index]);

    Inc(FPrimitiveCountPerFrame, PrimitiveCount);
  end
  else
    FillTrianglesMultiBatch(Vertices, Colors, Indices, VertexCount, PrimitiveCount);
end;

procedure TCanvasHelper.TexQuad(const P1, P2, P3, P4: TPointF; const TC1, TC2, TC3, TC4: TPointF;
  const AColor: TAlphaColor; const ATexture: TTexture);
begin
  UpdateBatchingStatus(TBatchingTopology.Triangles, 4, 6, ATexture, nil);

  InsertIndex(FBatchedVertices + 0);
  InsertIndex(FBatchedVertices + 1);
  InsertIndex(FBatchedVertices + 3);
  InsertIndex(FBatchedVertices + 3);
  InsertIndex(FBatchedVertices + 1);
  InsertIndex(FBatchedVertices + 2);

  InsertVertex(P1, TC1, AColor);
  InsertVertex(P2, TC2, AColor);
  InsertVertex(P3, TC3, AColor);
  InsertVertex(P4, TC4, AColor);

  Inc(FPrimitiveCountPerFrame, 2);
end;

procedure TCanvasHelper.TexTriangles(const Texture: TTexture;
  const Vertices, TexCoords: TVertexArray; const Colors: TAlphaColorArray;
  const Indices: TIndexArray; const VertexCount, PrimitiveCount: Integer);
var
  Index: Integer;
begin
  UpdateBatchingStatus(TBatchingTopology.Triangles, VertexCount, PrimitiveCount * 3, Texture, nil);

  for Index := 0 to (PrimitiveCount * 3) - 1 do
    InsertIndex(FBatchedVertices + Indices[Index]);

  for Index := 0 to VertexCount - 1 do
    InsertVertex(Vertices[Index], TexCoords[Index], Colors[Index]);

  Inc(FPrimitiveCountPerFrame, PrimitiveCount);
end;

procedure TCanvasHelper.TexEllipse(const ARect, ATexCoord: TRectF;
  const AColor: TAlphaColor; const ATexture: TTexture; const TransformCallback: TTransformCallback = nil);
const
  MinFlatDistance = 8;
var
  TexCoords, Vertices: TVertexArray;
  Colors: TAlphaColorArray;
  Indices: TIndexArray;
  Index, SubdivCount: Integer;
  Angle: Single;
  Radius: TPointF;
  TexPt, Center, LocalPt, CurPt: TPointF;
begin
  Radius.X := ARect.Width / 2;
  Radius.Y := ARect.Height / 2;

  SubdivCount := GetCircleSubdivCount(ARect);

  Center.X := (ARect.Left + ARect.Right) / 2.0;
  Center.Y := (ARect.Top + ARect.Bottom) / 2.0;

  SetLength(Vertices, 1 + SubdivCount);
  SetLength(Colors, 1 + SubdivCount);
  SetLength(TexCoords, 1 + SubdivCount);
  SetLength(Indices, SubdivCount * 3);

  LocalPt := Center;
  CurPt := LocalPt;
  if Assigned(TransformCallback) then
    TransformCallback(CurPt);

  Vertices[0] := CurPt;
  Colors[0] := AColor;
  TexPt := TPointF.Create((LocalPt.X - ARect.Left) / ARect.Width,
    (LocalPt.Y - ARect.Top) / ARect.Height);
  TexCoords[0] := TPointF.Create(ATexCoord.Left + TexPt.X * ATexCoord.Width,
    ATexCoord.Top + TexPt.Y * ATexCoord.Height);

  for Index := 0 to SubdivCount - 1 do
  begin
    Angle := Index * 2.0 * Pi / SubdivCount;

    LocalPt.X := Cos(Angle) * Radius.X;
    LocalPt.Y := Sin(Angle) * Radius.Y;

    LocalPt.Offset(Center);

    CurPt := LocalPt;
    if Assigned(TransformCallback) then
      TransformCallback(CurPt);

    Vertices[1 + Index] := CurPt;
    Colors[1 + Index] := AColor;
    TexPt := TPointF.Create((LocalPt.X - ARect.Left) / ARect.Width,
      (LocalPt.Y - ARect.Top) / ARect.Height);
    TexCoords[1 + Index] := TPointF.Create(ATexCoord.Left + TexPt.X * ATexCoord.Width,
      ATexCoord.Top + TexPt.Y * ATexCoord.Height);

    Indices[(Index * 3) + 0] := 0;
    Indices[(Index * 3) + 1] := 1 + Index;
    Indices[(Index * 3) + 2] := 1 + ((1 + Index) mod SubdivCount);
  end;

  TexTriangles(ATexture, Vertices, TexCoords, Colors, Indices, Length(Vertices), SubdivCount);
end;

procedure TCanvasHelper.TexEllipse(const ARect: TRectF; const AColor: TAlphaColor; const ATexture: TTexture;
  const TransformCallback: TTransformCallback = nil);
begin
  TexEllipse(ARect, RectF(0, 0, 1, 1), AColor, ATexture, TransformCallback);
end;

procedure TCanvasHelper.GradientRect(const DestCorners: TCornersF;
  const AGradient: TGradient);
begin
  TempVertices4[0] := DestCorners[0];
  TempVertices4[1] := DestCorners[1];
  TempVertices4[2] := DestCorners[2];
  TempVertices4[3] := DestCorners[3];

  TempTexCoords4[0] := TPointF.Zero;
  TempTexCoords4[1] := TPointF.Create(1, 0);
  TempTexCoords4[2] := TPointF.Create(1, 1);
  TempTexCoords4[3] := TPointF.Create(0, 1);

  TempIndices6[0] := 0;
  TempIndices6[1] := 2;
  TempIndices6[2] := 3;
  TempIndices6[3] := 0;
  TempIndices6[4] := 1;
  TempIndices6[5] := 2;

  GradientTriangles(AGradient, TempVertices4, TempTexCoords4, TempIndices6, 4, 2);
end;

procedure TCanvasHelper.GradientQuad(const P1, P2, P3, P4: TPointF;
  const AGradient: TGradient);
begin
  if not FGradientMat.Gradient.Equal(AGradient) then
    Flush;
  FGradientMat.Gradient := AGradient;
  UpdateBatchingStatus(TBatchingTopology.Triangles, 4, 6, nil, FGradientMat);

  InsertIndex(FBatchedVertices + 0);
  InsertIndex(FBatchedVertices + 1);
  InsertIndex(FBatchedVertices + 3);
  InsertIndex(FBatchedVertices + 3);
  InsertIndex(FBatchedVertices + 1);
  InsertIndex(FBatchedVertices + 2);

  InsertVertex(P1, PointF(0, 0), $FFFFFFFF);
  InsertVertex(P2, PointF(1, 0), $FFFFFFFF);
  InsertVertex(P3, PointF(1, 1), $FFFFFFFF);
  InsertVertex(P4, PointF(0, 1), $FFFFFFFF);

  Inc(FPrimitiveCountPerFrame, 2);
end;

function TCanvasHelper.CalculateTopLeft(const ARect: TRectF; const TransformCallback: TTransformCallback): TPointF;
var
  Temp: TPointF;
begin
  // Top-left corner.
  Result := ARect.TopLeft;

  if Assigned(TransformCallback) then
    TransformCallback(Result);

  // Top-right corner.
  Temp := PointF(ARect.Right, ARect.Top);

  if Assigned(TransformCallback) then
    TransformCallback(Temp);

  Result.X := Min(Result.X, Temp.X);
  Result.Y := Min(Result.Y, Temp.Y);

  // Bottom-right corner.
  Temp := ARect.BottomRight;

  if Assigned(TransformCallback) then
    TransformCallback(Temp);

  Result.X := Min(Result.X, Temp.X);
  Result.Y := Min(Result.Y, Temp.Y);

  // Bottom-left corner.
  Temp := PointF(ARect.Left, ARect.Bottom);

  if Assigned(TransformCallback) then
    TransformCallback(Temp);

  Result.X := Min(Result.X, Temp.X);
  Result.Y := Min(Result.Y, Temp.Y);
end;

procedure TCanvasHelper.GradientEllipse(const ARect: TRectF; const AGradient: TGradient;
  const TransformCallback: TTransformCallback = nil);
const
  MinFlatDistance = 8;
var
  TexCoords, Vertices: TVertexArray;
  Indices: TIndexArray;
  Index, SubdivCount: Integer;
  Angle: Single;
  Radius: TPointF;
  Center, CurPt, TopLeft: TPointF;
begin
  Radius.X := ARect.Width / 2;
  Radius.Y := ARect.Height / 2;

  SubdivCount := Max(Ceil(2 * Pi * Max(Radius.X, Radius.Y) / MinFlatDistance), 12);

  TopLeft := CalculateTopLeft(ARect, TransformCallback);

  Center.X := (ARect.Left + ARect.Right) / 2.0;
  Center.Y := (ARect.Top + ARect.Bottom) / 2.0;

  SetLength(Vertices, 1 + SubdivCount);
  SetLength(TexCoords, 1 + SubdivCount);
  SetLength(Indices, SubdivCount * 3);

  CurPt := Center;

  if Assigned(TransformCallback) then
    TransformCallback(CurPt);

  Vertices[0] := CurPt;
  TexCoords[0] := TPointF.Create((CurPt.X - TopLeft.X) / ARect.Width, (CurPt.Y - TopLeft.Y) / ARect.Height);

  for Index := 0 to SubdivCount - 1 do
  begin
    Angle := Index * 2 * Pi / SubdivCount;

    CurPt.X := Cos(Angle) * Radius.X;
    CurPt.Y := Sin(Angle) * Radius.Y;

    CurPt.Offset(Center);

    if Assigned(TransformCallback) then
      TransformCallback(CurPt);

    Vertices[1 + Index] := CurPt;
    TexCoords[1 + Index] := TPointF.Create((CurPt.X - TopLeft.X) / ARect.Width, (CurPt.Y - TopLeft.Y) / ARect.Height);

    Indices[(Index * 3) + 0] := 0;
    Indices[(Index * 3) + 1] := 1 + Index;
    Indices[(Index * 3) + 2] := 1 + ((1 + Index) mod SubdivCount);
  end;

  GradientTriangles(AGradient, Vertices, TexCoords, Indices, Length(Vertices), SubdivCount);
end;

procedure TCanvasHelper.GradientTriangles(const AGradient: TGradient; const Vertices, TexCoords: TVertexArray;
  const Indices: TIndexArray; const VertexCount, PrimitiveCount: Integer);
var
  Index: Integer;
begin
  if not FGradientMat.Gradient.Equal(AGradient) then
    Flush;

  FGradientMat.Gradient := AGradient;
  UpdateBatchingStatus(TBatchingTopology.Triangles, VertexCount, PrimitiveCount * 3, nil, FGradientMat);

  for Index := 0 to (PrimitiveCount * 3) - 1 do
    InsertIndex(FBatchedVertices + Indices[Index]);

  for Index := 0 to VertexCount - 1 do
    InsertVertex(Vertices[Index], TexCoords[Index], $FFFFFFFF);

  Inc(FPrimitiveCountPerFrame, PrimitiveCount);
end;

procedure RegisterAliases;
begin
  AddEnumElementAliases(TypeInfo(TDrawingMode), ['dmNormal', 'dmWriteStencilInvert', 'dmReadStencil', 'dmClearStencil']);
end;

procedure UnregisterAliases;
begin
  RemoveEnumElementAliases(TypeInfo(TDrawingMode));
end;

initialization
  RegisterAliases;

finalization
  UnregisterAliases;
end.
