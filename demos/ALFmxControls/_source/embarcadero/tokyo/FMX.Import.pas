{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Import;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.Types, System.UITypes, System.Math.Vectors, FMX.Types3D, FMX.Objects3D, FMX.MaterialSources,
  FMX.Graphics;

type
  IModelImporter = interface
    ['{DBEB065D-3BE7-4289-8F24-9CD128902366}']
    function GetDescription: string;
    function GetExt: string;

    function LoadFromFile(const AFileName: string; out AMesh: TMeshDynArray;
      const AOwner: TComponent): Boolean;
  end;

  TModelImportServices = class(TPersistent)
  published
    class function GetSupportedFileTypesCount: Integer;
    class function GetFileExt(const index: integer): string;
    class function GetFileDescription(const index: integer): string;

    class function RegisterImporter(const AMeshImporter: IModelImporter): Integer; virtual;
    class procedure UnregisterImporter(AMeshImporterIndex: Integer); virtual;
    class function LoadFromFile(const AFileName: string;
      out AMesh: TMeshDynArray; const AOwner: TComponent): boolean;
  end;

  TModelImporter = class(TInterfacedObject, IModelImporter)
  public
    function GetDescription: string; virtual; abstract;
    function GetExt: string; virtual; abstract;

    function LoadFromFile(const AFileName: string;
      out AMesh: TMeshDynArray; const AOwner: TComponent): Boolean; virtual; abstract;
  end;

  TCustomModel = class
  public
    procedure LoadFromFile(const AFileName: string); virtual; abstract;
  end;

  TGEImage = class
  public
    FId: String;
    FName: String;
    FFileName: String;
  end;

  TGEBitmap = class(TBitmap)
  private
    FFileName: string;
  public
    constructor CreateFromFile(const AFileName: string); override;
    property FileName: string read FFileName;
  end;

  TGEMaterial = class
  public
    FName: string;
    FDiffuse: TAlphaColorF;
    FAmbient: TAlphaColorF;
    FSpecular: TAlphaColorF;
    FDiffuseMap: string;
    FDiffuseBitmap: TGEBitmap;
  end;

  TGEMaterialDynArray = array of TGEMaterial;
  TGEMaterials = class
  private
    FMaterialsOwner: boolean;
    FMaterials: TGEMaterialDynArray;
    FBitmaps: array of TGEBitmap;

    function CreateBitmap(const AFileName: string): TGEBitmap;
    procedure SetLocalTexturePath(const APath: string);

    function GetCount: Integer;
    procedure SetCount(const ACount: Integer);

    function GetItems(AIndex: Integer): TGEMaterial;
    procedure SetItems(AIndex: Integer; AItem: TGEMaterial);
  public
    constructor Create(AMaterialsOwner: boolean);
    destructor Destroy; override;

    procedure LoadImages(const APath : string);
    function Add(const AMaterial: TGEMaterial): TGEMaterial;

    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: TGEMaterial read GetItems write SetItems; default;
    property Materials: TGEMaterialDynArray read FMaterials;
  end;

  PGEVertex = ^TGEVertex;
  TGEVertex = record
    Pos: TPoint3D;
    Nor: TPoint3D;
    Tex: TPointF;
    Sth: Integer;   // smooth group
    GenerateNormal: Boolean;
  end;

  TGEVertexID = record
    Position:  Integer;
    Normal: Integer;
    Texture0: Integer;
    SmoothGroup: Integer;
  end;

const
  NullVertexID : TGEVertexID = (Position:0; Normal:-1; Texture0: -1; SmoothGroup: -1);

type
  TGEPoligonID = array of TGEVertexID;

  TGETriangleID = array [0..2] of TGEVertexID;

  TGETriangleMeshID = array of TGETriangleID;

  TGEVertexArray = array of TGEVertex;

  TGEVertexSource = class
  private
    FPositionSource : TPoint3DDynArray;
    FNormalSource : TPoint3DDynArray;
    FTexture0Source : TPointFDynArray;
    procedure SetTextue0SourceArray(const ASource : TPointFDynArray);
  public
    procedure SetPositionSource(const ASource : TSingleDynArray);
    procedure SetNormalSource(const ASource : TSingleDynArray);
    procedure SetTextue0Source(const ASource : TSingleDynArray; AStride: Integer = 2);

    function AddPositionSource(const ASource : TSingleDynArray): Integer;
    function AddNormalSource(const ASource : TSingleDynArray): Integer;
    function AddTextue0Source(const ASource : TSingleDynArray): Integer;

    property PositionSource : TPoint3DDynArray write FPositionSource;
    property NormalSource : TPoint3DDynArray write FNormalSource;
    property Texture0Source : TPointFDynArray write SetTextue0SourceArray;
  end;

  TGEMesh = class
  private
    FTriangleCount: Integer;
    FTriangles: TGEVertexArray;
    FSource: TGEVertexSource;
    FMaterialName: String;
    procedure CalculateTriNormal(var Av1, Av2, Av3: TGEVertex);
  public
    constructor Create(const ASource: TGEVertexSource);
    property MaterialName: String read FMaterialName write FMaterialName;
    procedure AddVertex(const AVertex: TGEVertexID);
    procedure AddTriangle(const ATriangle: TGETriangleID);
    procedure AddPoligon(const APoligon: TGEPoligonID);
    procedure AddTriangleMesh(const ATriangleMesh: TGETriangleMeshID);

    function CreateMesh(AOwner: TComponent;
      const ATransform: TMatrix3D;
      const AMaterials : TGEMaterialDynArray): TMesh;
  end;

  TGEMeshDynArray = array of TGEMesh;

function SetLocalPath(const APath: string; var AFileName: string):boolean;
function FloatStringsToSingleDynArray(const AStr: string): TSingleDynArray;
function IntStringsToIntegerDynArray(const AStr: string): TIntegerDynArray; overload;
function StringsToStringDynArray(const AStr: string): TStringDynArray; overload;

implementation

uses
  System.SysUtils, System.UIConsts, System.RTLConsts, FMX.Forms;

{ TModelImportServices }

type
  TModelImportService = record
    Id: Integer;
    Service: IModelImporter;
  end;

function TModelImportService_Create(AId: Integer;
  const AService: IModelImporter): TModelImportService;
begin
  Result.Id := AId;
  Result.Service := AService;
end;

var
  FModelImportServicesID: Integer = 0;
  FModelImportServices: array of TModelImportService;

class function TModelImportServices.GetFileDescription(
  const index: integer): string;
begin
  if (index < 0) or (index > High(FModelImportServices)) then
  begin
    Result := '';
    Exit;
  end;
  Result := FModelImportServices[index].Service.GetDescription;
end;

class function TModelImportServices.GetFileExt(const index: integer): string;
begin
  if (index < 0) or (index > High(FModelImportServices)) then
  begin
    Result := '';
    Exit;
  end;
  Result := FModelImportServices[index].Service.GetExt;
end;

class function TModelImportServices.GetSupportedFileTypesCount: Integer;
begin
  Result := Length(FModelImportServices);
end;

class function TModelImportServices.LoadFromFile(const AFileName: string;
  out AMesh: TMeshDynArray; const AOwner: TComponent): Boolean;
var
  LImporter: IModelImporter;
  i: Integer;
  LExt: string;
  LMesh: TMesh;
begin
  Result := False;
  LExt := ExtractFileExt(AFileName);

  if LExt = '' then
    Exit;

  for i := 0 to High(FModelImportServices) do
  begin
    LImporter := FModelImportServices[i].Service;
    if SameText('.' + LImporter.GetExt, LExt) then
    begin
      Result := LImporter.LoadFromFile(AFileName, AMesh, AOwner);
      for LMesh in AMesh do
        LMesh.Stored := False;
      Exit;
    end;
  end;

end;

class function TModelImportServices.RegisterImporter(
  const AMeshImporter: IModelImporter): Integer;
begin
  Result := Length(FModelImportServices);
  SetLength(FModelImportServices, Result + 1);
  FModelImportServices[Result] :=
    TModelImportService_Create(FModelImportServicesID, AMeshImporter);
  Inc(FModelImportServicesID);
end;

class procedure TModelImportServices.UnregisterImporter(AMeshImporterIndex: Integer);
var
  i: Integer;
begin
  for i := AMeshImporterIndex to High(FModelImportServices) - 1 do
    FModelImportServices[i] := FModelImportServices[i + 1];

  if Length(FModelImportServices) > 0 then
    SetLength(FModelImportServices, High(FModelImportServices));
end;

function TGEVertexSource.AddPositionSource(const ASource : TSingleDynArray): Integer;
begin
   result := Length(FPositionSource);
   SetLength(FPositionSource, result + 1);
   FPositionSource[result] := Point3D(ASource[0], ASource[1], ASource[2]);
end;

function TGEVertexSource.AddNormalSource(const ASource : TSingleDynArray): Integer;
begin
   result := Length(FNormalSource);
   SetLength(FNormalSource, result + 1);
   FNormalSource[result] := Point3D(ASource[0], ASource[1], ASource[2]);
end;

function TGEVertexSource.AddTextue0Source(const ASource : TSingleDynArray): Integer;
begin
   result := Length(FTexture0Source);
   SetLength(FTexture0Source, result + 1);
   FTexture0Source[result] := PointF(ASource[0], 1-ASource[1]);
end;

procedure TGEVertexSource.SetTextue0SourceArray(const ASource : TPointFDynArray);
var
  i : Integer;
begin
  FTexture0Source :=  ASource;
  for i := 0 to High(FTexture0Source) do
     FTexture0Source[i].Y := 1 - FTexture0Source[i].Y;
end;

procedure TGEVertexSource.SetPositionSource(const ASource : TSingleDynArray);
var
  i , id: Integer;
begin
  SetLength(FPositionSource, Length(ASource) div 3);
  for i := 0 to High(FPositionSource) do
  begin
     id  := i * 3;
     FPositionSource[i] := Point3D( ASource[id + 0], ASource[id + 1], ASource[id + 2]);
  end;
end;

procedure TGEVertexSource.SetNormalSource(const ASource : TSingleDynArray);
var
  i , id: Integer;
begin
  SetLength(FNormalSource, Length(ASource) div 3);
  for i := 0 to High(FNormalSource) do
  begin
     id  := i * 3;
     FNormalSource[i] := Point3D(ASource[id + 0], ASource[id + 1], ASource[id + 2])
  end;
end;

procedure TGEVertexSource.SetTextue0Source(const ASource : TSingleDynArray; AStride: Integer = 2);
var
  i , id: Integer;
begin
  SetLength(FTexture0Source, Length(ASource) div AStride);
  for i := 0 to High(FTexture0Source) do
  begin
     id  := i * AStride;
     FTexture0Source[i] := PointF( ASource[id + 0], 1 - ASource[id + 1]);   // change y value to firemonkey coordinate
  end;
end;

constructor TGEMesh.Create(const ASource: TGEVertexSource);
begin
  FSource := ASource;
end;

procedure TGEMesh.CalculateTriNormal(var Av1, Av2, Av3: TGEVertex);
var
  LTangent1: TPoint3D;
  LTangent2: TPoint3D;
  LNormal: TPoint3D;
begin
  LTangent1 := (Av1.Pos - Av2.Pos).Normalize;
  LTangent2 := (Av1.Pos - Av3.Pos).Normalize;
  LNormal   := LTangent2.CrossProduct(LTangent1).Normalize;

  Av1.Nor := LNormal;
  Av2.Nor := LNormal;
  Av3.Nor := LNormal;
end;

procedure TGEMesh.AddVertex(const AVertex: TGEVertexID);
var
  LVertex: TGEVertex;
begin
  LVertex.Pos := FSource.FPositionSource[AVertex.Position];
  LVertex.Sth := AVertex.SmoothGroup;

  if (AVertex.Texture0 >= 0) then
    LVertex.Tex :=  FSource.FTexture0Source[AVertex.Texture0]
  else
    LVertex.Tex := Pointf(0,0);

  LVertex.GenerateNormal :=  (AVertex.Normal < 0) or (AVertex.Normal > High(FSource.FNormalSource));

  if LVertex.GenerateNormal then
    LVertex.Nor := NullPoint3D
  else
    LVertex.Nor := FSource.FNormalSource[AVertex.Normal];

  SetLength(FTriangles, FTriangleCount + 1);
  FTriangles[FTriangleCount] := LVertex;
  Inc(FTriangleCount);

  // automatic calculate normals
  if (LVertex.GenerateNormal and (FTriangleCount mod 3 = 0)) then
    CalculateTriNormal(
      FTriangles[FTriangleCount-3],
      FTriangles[FTriangleCount-2],
      FTriangles[FTriangleCount-1]);
end;

procedure TGEMesh.AddTriangle(const ATriangle: TGETriangleID);
begin
  AddVertex(ATriangle[0]);
  AddVertex(ATriangle[2]);
  AddVertex(ATriangle[1]);
end;

procedure TGEMesh.AddTriangleMesh(const ATriangleMesh: TGETriangleMeshID);
var
  i : Integer;
begin
  for i := 0 to High(ATriangleMesh) do
   AddTriangle(ATriangleMesh[i]);
end;


procedure TGEMesh.AddPoligon(const APoligon: TGEPoligonID);
var
  i : Integer;
begin
  for i := 1 to High(APoligon)-1 do
  begin
    AddVertex(APoligon[0]);
    AddVertex(APoligon[i+1]);
    AddVertex(APoligon[i]);
  end;
end;

function PointEqual(const v1, v2: TPoint3D): Boolean; overload;
begin
  Result := CompareMem(@v1, @v2, SizeOf(v1));
end;

function PointEqual(const v1, v2: TPointF): Boolean; overload;
begin
  Result := CompareMem(@v1, @v2, SizeOf(v1));
end;

function TGEMesh.CreateMesh(AOwner: TComponent;
   const ATransform: TMatrix3D;
   const AMaterials: TGEMaterialDynArray): TMesh;
var
  V, F, I: Integer;
  LNum: Integer;
  LMesh: TMesh;
  AMaterial: TGEMaterial;
  LVec: TPoint3D;
  LVerexV: PGEVertex;
  FGeneratedNormals: array of TPoint3D;
  ANormalTransform: TMatrix3D;
  LFound: Boolean;
  LNewTriangles: array of PGEVertex;
  LMeshMaterial: TLightMaterialSource;
  Root: TComponent;
begin
  ANormalTransform := ATransform;
  ANormalTransform.m41 := 0;
  ANormalTransform.m42 := 0;
  ANormalTransform.m43 := 0;
  ANormalTransform.m44 := 1;

  ANormalTransform.m14 := 0;
  ANormalTransform.m24 := 0;
  ANormalTransform.m34 := 0;
  ANormalTransform.m44 := 1;

  LMesh := TMesh.Create(AOwner);
  LMesh.Stored := False;

  {
  //  problematic transformation conversion from matrix
  LTransform := Matrix3DToTransform(ATransform);
  LMesh.RotationAngle.Vector := NullVector3D;
  LMesh.Position.Vector := ATransform.Position;
  LMesh.RotationAngle.X := ATransform.Rotation.X;
  LMesh.RotationAngle.Y := ATransform.Rotation.Y;
  LMesh.RotationAngle.Z := ATransform.Rotation.Z;
  LMesh.Scale.Vector := ATransform.Scale;
  }

  LMesh.Locked := true;
  LMesh.HitTest := false;

  LMesh.Data.IndexBuffer.Length := Length(FTriangles);
  SetLength(LNewTriangles, Length(FTriangles));
  LNum := 0;

  SetLength(FGeneratedNormals, LMesh.Data.IndexBuffer.Length);
  for V := 0 to LMesh.Data.IndexBuffer.Length - 1 do
  begin
    LVerexV := @FTriangles[V];

    if LVerexV.GenerateNormal then
    begin
      FGeneratedNormals[V] := NullPoint3D;
      for F := 0 to LMesh.Data.IndexBuffer.Length - 1 do
        if (LVerexV.Sth = FTriangles[F].Sth) and PointEqual(LVerexV.Pos, FTriangles[F].Pos) then
          FGeneratedNormals[V] := FGeneratedNormals[V] + FTriangles[F].Nor;
    end;
  end;

  for V := 0 to LMesh.Data.IndexBuffer.Length - 1 do
  begin
    LVerexV := @FTriangles[V];
    if LVerexV.GenerateNormal then
      LVerexV.Nor := FGeneratedNormals[V];

    LFound := false;
    for F := 0 to LNum - 1 do
      if PointEqual(LVerexV.Pos, LNewTriangles[F].Pos) and
         PointEqual(LVerexV.Tex, LNewTriangles[F].Tex) and
         PointEqual(LVerexV.Nor, LNewTriangles[F].Nor) then
      begin
        LFound := true;
        LMesh.Data.IndexBuffer[V] := F;
        break;
      end;

    if not LFound then
    begin
      LNewTriangles[LNum] := LVerexV;
      LMesh.Data.IndexBuffer[V] := LNum;
      Inc(LNum);
    end;
  end;

  LMesh.Data.VertexBuffer.Length := LNum;
  for V := 0 to LNum - 1 do
  begin
    LVec := LNewTriangles[V].Pos * ATransform;
    LMesh.Data.VertexBuffer.Vertices[V] := LVec;

    LVec := TPoint3D(TVector3D.Create(LNewTriangles[V].Nor, 0) * ANormalTransform).Normalize;
    LMesh.Data.VertexBuffer.Normals[V] := LVec;
    LMesh.Data.VertexBuffer.TexCoord0[V] := LNewTriangles[V].Tex;
  end;

  for I := 0 to High(AMaterials) do
  begin
    AMaterial := AMaterials[I];
    if SameText(AMaterial.FName, FMaterialName) or
       SameText(AMaterial.FName, FMaterialName + 'ID') then
    begin
      if LMesh.Root <> nil then
        Root := LMesh.Root.GetObject
      else if AOwner.Owner <> nil then
        Root := AOwner.Owner
      else
        Root := AOwner;
      if (FileExists(AMaterial.FDiffuseMap)) then
      begin
        LMeshMaterial := TLightMaterialSource.Create(Root);
        LMeshMaterial.Texture := AMaterial.FDiffuseBitmap;
        LMeshMaterial.Diffuse := MakeColor(255, 255, 255, 255);
        LMesh.MaterialSource := LMeshMaterial;
      end else
      begin
        LMeshMaterial := TLightMaterialSource.Create(Root);
        LMeshMaterial.Diffuse := AMaterial.FDiffuse.ToAlphaColor;
        LMeshMaterial.Specular := AMaterial.FSpecular.ToAlphaColor;
        LMeshMaterial.Ambient := AMaterial.FAmbient.ToAlphaColor;
        LMesh.MaterialSource := LMeshMaterial;
      end;
      {$IFDEF MSWINDOWS}
      if (csDesigning in AOwner.ComponentState) and (Root is TCommonCustomForm) then
        LMeshMaterial.Name := TCommonCustomForm(Root).Designer.UniqueName(AOwner.Name + 'Mat' + IntToStr(I));
      {$ENDIF}
    end;
  end;

  LMesh.Locked := true;
  LMesh.TwoSide := false;
  result := LMesh;

end;

{ TGEBitmap }

constructor TGEBitmap.CreateFromFile(const AFileName: string);
begin
  inherited ;
  LoadFromFile(AFileName);
  FFileName := AFileName;
end;

{ TGEMaterials }

function TGEMaterials.Add(const AMaterial: TGEMaterial): TGEMaterial;
var
  LLength: Integer;
begin
  LLength := Length(FMaterials);
  SetLength(FMaterials, LLength + 1);
  FMaterials[LLength] := AMaterial;
  Result := AMaterial;
end;

function TGEMaterials.CreateBitmap(const AFileName: string): TGEBitmap;
var
  LLength: Integer;
begin
  LLength := Length(FBitmaps);
  SetLength(FBitmaps, LLength + 1);
  Result := TGEBitmap.CreateFromFile(AFileName);
  FBitmaps[LLength] := Result;
end;

destructor TGEMaterials.Destroy;
var
  i: Integer;
  B: TGEBitmap;
begin
  if FMaterialsOwner then
    for i := 0 to High(FMaterials) do
      FMaterials[i].Free;

  for i := 0 to High(FBitmaps) do
  begin
    B := FBitmaps[i];
    FreeAndNil(B);
  end;

  inherited;
end;

function TGEMaterials.GetCount: Integer;
begin
  Result := Length(FMaterials);
end;

function TGEMaterials.GetItems(AIndex: Integer): TGEMaterial;
begin
  Result := FMaterials[AIndex];
end;

constructor TGEMaterials.Create(AMaterialsOwner: boolean);
begin
  FMaterialsOwner := AMaterialsOwner;
end;

procedure TGEMaterials.LoadImages(const APath: string);
var
  LImages: array of TGEBitmap;
  j : Integer;

  function AddImage(AFileName:String): TGEBitmap;
  var
    i, len: Integer;
  begin
    Result := nil;

    len := Length(LImages);
    for i := 0 to len-1 do
      if SameText(LImages[i].FileName, AFileName) then
      begin
         Result := LImages[i];
         Exit;
      end;

    if FileExists(AFileName) then
    begin
      SetLength(LImages, len + 1);
      Result := CreateBitmap(AFileName);
      LImages[len] := Result;
    end;
  end;

begin
  SetLocalTexturePath(APath);

  LImages := nil;
  for j := 0 to High(FMaterials) do
    FMaterials[j].FDiffuseBitmap := AddImage(FMaterials[j].FDiffuseMap);
end;

procedure TGEMaterials.SetCount(const ACount: Integer);
begin
  SetLength(FMaterials, ACount);
end;

procedure TGEMaterials.SetItems(AIndex: Integer; AItem: TGEMaterial);
begin
  FMaterials[AIndex] := AItem;
end;


function SetLocalPath(const APath: string; var AFileName: string):boolean;
var
  LFileName : String;
  function SetFileExists(const ATestFileName: String):boolean;
  begin
    result := FileExists(ATestFileName);
    if result then
      LFileName := ATestFileName;
  end;
begin
  result := true;

  LFileName := AFileName.Replace('/', '\', [rfReplaceAll]);

  if not SetFileExists(LFileName) then
  if not SetFileExists(APath + LFileName) then
  if not SetFileExists(ExtractFileName(LFileName)) then
  if not SetFileExists(APath + ExtractFileName(LFileName)) then
    result := false;

  AFileName := LFileName;
end;

procedure TGEMaterials.SetLocalTexturePath(const APath: string);
var
  i : Integer;
begin
  for i := 0 to High(FMaterials) do
     SetLocalPath(APath, FMaterials[i].FDiffuseMap);
end;

function FloatStringsToSingleDynArray(const AStr: string): TSingleDynArray;
var
  str: string;
  i, l : Integer;
  ch : Char ;

  LFormatSettings: TFormatSettings;

  procedure TryAdd;
  begin
    if str = '' then
      Exit;

    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := StrToFloat(str, LFormatSettings);
    str := '';
  end;

begin
  Result := nil;

  if AStr = '' then
    Exit;

  LFormatSettings := TFormatSettings.Create;

  i := 0;
  l := AStr.Length;
  while i < l do
  begin
    ch := AStr.Chars[i];
    case ch of
      chr(1)..chr(32): TryAdd;
      else
        if (ch = '.') or (ch = ',') then
          LFormatSettings.DecimalSeparator := ch;
        str := str + ch;
    end;

    Inc(i);
  end;

  TryAdd;
end;

function IntStringsToIntegerDynArray(const AStr: string): TIntegerDynArray;
var
  str: string;
  i, l : Integer;
  ch : Char ;

  procedure TryAdd;
  begin
    if str = '' then
      Exit;
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := StrToInt(str);
    str := '';
  end;

begin
  Result := nil;
  if AStr = '' then
    Exit;
  i := 0;
  l := AStr.Length ;
  while i < l do
  begin
    ch := AStr.Chars[i];
    case ch of
      chr(1)..chr(32): TryAdd;
    else
      str := str + ch;
    end;
    Inc(i);
  end;
  TryAdd;
end;

function StringsToStringDynArray(const AStr: string): TStringDynArray; overload;
var
  str: string;
  ch: Char;
  i, l : Integer;

  procedure TryAdd;
  begin
    if str = '' then
      Exit;

    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := str;
    str := '';
  end;

begin
  Result := nil;
  if AStr = '' then
    Exit;
  i := 0;
  l := AStr.Length;
  while i < l do
  begin
    ch := AStr.Chars[i];
    case ch of
      #1..#32: TryAdd;
    else
      str := str + ch;
    end;
    Inc(i);
  end;
  TryAdd;
end;

end.
