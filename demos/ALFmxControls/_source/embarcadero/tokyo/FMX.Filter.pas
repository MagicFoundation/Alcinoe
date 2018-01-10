{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Filter;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.Types, System.Rtti, System.UITypes, System.Generics.Collections, FMX.Types3D, FMX.Graphics;

type

{ TFilter }

  TFilterValueType = (Float, Point, Color, Bitmap);

  TFilterValueTypeHelper = record helper for TFilterValueType
  const
    vtFloat = TFilterValueType.Float deprecated 'Use TFilterValueType.Float';
    vtPoint = TFilterValueType.Point deprecated 'Use TFilterValueType.Point';
    vtColor = TFilterValueType.Color deprecated 'Use TFilterValueType.Color';
    vtBitmap = TFilterValueType.Bitmap deprecated 'Use TFilterValueType.Bitmap';
  end;

  TFilterValueRec = record
    Name: string;
    Desc: string;
    ValueType: TFilterValueType;
    Value: TValue;
    Min, Max, Default: TValue;
    Bitmap: TBitmap;
    constructor Create(const AName, ADesc: string; AType: TFilterValueType; ADefault, AMin, AMax: TValue); overload;
    constructor Create(const AName, ADesc: string; AType: TFilterValueType); overload;
    constructor Create(const AName, ADesc: string; ADefault: TAlphaColor); overload;
    constructor Create(const AName, ADesc: string; ADefault, AMin, AMax: Single); overload;
    constructor Create(const AName, ADesc: string; ADefault, AMin, AMax: TPointF); overload;
  end;

  TFilterValueRecArray = array of TFilterValueRec;

  TFilterRec = record
    Name: string;
    Desc: string;
    Values: TFilterValueRecArray;
    constructor Create(const AName, ADesc: string; AValues: array of TFilterValueRec); overload;
  end;

  TFilterClass = class of TFilter;
  TFilter = class(TPersistent)
  private
    function GetFilterValues(const Index: string): TValue;
    procedure SetFilterValues(const Index: string; Value: TValue);
    function GetFilterValuesAsBitmap(const Index: string): TBitmap;
    procedure SetFilterValuesAsBitmap(const Index: string; const Value: TBitmap);
    function GetFilterValuesAsPoint(const Index: string): TPointF;
    procedure SetFilterValuesAsPoint(const Index: string; const Value: TPointF);
    procedure SetInputFilter(const Value: TFilter);
    function GetFilterValuesAsFloat(const Index: string): Single;
    procedure SetFilterValuesAsFloat(const Index: string; const Value: Single);
    function GetFilterValuesAsColor(const Index: string): TAlphaColor;
    procedure SetFilterValuesAsColor(const Index: string; const Value: TAlphaColor);
    function GetFilterValuesAsTexture(const Index: string): TTexture;
    procedure SetFilterValuesAsTexture(const Index: string; const Value: TTexture);
    property Values[const Index: string]: TValue read GetFilterValues write SetFilterValues; default;
  private
    class var FNoise: TTexture;
    procedure CreateNoise;
    procedure RenderTextureToContext(const Context: TContext3D;
      const Texture: TTexture; const ARect: TRect; const DstPos: TPoint);
  protected
    class var FVertexShader: TContextShader;
  protected
    FValues: TFilterValueRecArray;
    FInputRT: TTexture;
    FInputRTContext: TContext3D;
    FTargetRT: TTexture;
    FTargetRTContext: TContext3D;
    FPassInputRT: TTexture;
    FPassInputRTContext: TContext3D;
    FInput: TTexture;
    [Weak] FInputBitmap: TBitmap;
    FTarget: TTexture;
    [Weak] FTargetBitmap: TBitmap;
    FOutputSize: TSize;
    FOutputBitmap: TBitmap;
    FProcessing: Boolean;
    FModified: Boolean;
    FInputFilter: TFilter;
    FNeedInternalSecondTex: string; // need internal texture as second
    FNoCopyForOutput: Boolean; // not copy result to targer
    FAntiAlise: Boolean; // add transparent border for antialising
    FShaders: array of TContextShader; // shaders
    FPass, FPassCount: Integer; // for multipass - default shader have 1 pass
    function CreateFilterMaterial: TMaterial; virtual;
    procedure CalcSize(var W, H: Integer); virtual;
    procedure LoadShaders; virtual;
    procedure LoadTextures; virtual;
    procedure Render(W, H: Integer); virtual;
    function InputSize: TSize;
    function InputTexture: TTexture;
    function TargetTexture: TTexture;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    class procedure UnInitialize;
    class function FilterAttr: TFilterRec; virtual;
    { Class static method for C++ access }
    class function FilterAttrForClass(C: TFilterClass): TFilterRec; static;
    procedure Apply; virtual;
    procedure ApplyWithoutCopyToOutput;
    property ValuesAsBitmap[const Index: string]: TBitmap read GetFilterValuesAsBitmap
      write SetFilterValuesAsBitmap;
    property ValuesAsTexture[const Index: string]: TTexture read GetFilterValuesAsTexture
      write SetFilterValuesAsTexture;
    property ValuesAsPoint[const Index: string]: TPointF read GetFilterValuesAsPoint
      write SetFilterValuesAsPoint;
    property ValuesAsFloat[const Index: string]: Single read GetFilterValuesAsFloat
      write SetFilterValuesAsFloat;
    property ValuesAsColor[const Index: string]: TAlphaColor read GetFilterValuesAsColor
      write SetFilterValuesAsColor;
    property InputFilter: TFilter read FInputFilter write SetInputFilter;
  end;

  TFilterClassDict = TDictionary<TFilterClass, string>;

  TFilterManager = class sealed
  strict private type
    TContextRec = record
      Texture: TTexture;
      Context: TContext3D;
    end;
  strict private
    class var FFilterList: TFilterClassDict;
    class var FContextList: TList<TContextRec>;
    class var FCurrentContext: Integer;
    class function GetFilterContext: TContext3D; static;
    class function GetFilterTexture: TTexture; static;
    class procedure SetCurrentContext(const Value: Integer); static;
    class function GetContextContext: Integer; static;
    class procedure SetContextCount(const Value: Integer); static;
  public
    // Reserved for internal use only - do not call directly!
    class procedure UnInitialize;
    // Register a filter in category
    class procedure RegisterFilter(const Category: string; Filter: TFilterClass);
    // Return filter
    class function FilterByName(const AName: string): TFilter;
    class function FilterClassByName(const AName: string): TFilterClass;
    // Categoties
    class procedure FillCategory(AList: TStrings);
    class procedure FillFiltersInCategory(const Category: string; AList: TStrings);
    // Context
    class procedure ResizeContext(Width, Height: Integer);
    class function GetTexture(const Index: Integer): TTexture;
    class property CurrentContext: Integer read FCurrentContext write SetCurrentContext;
    class property ContextCount: Integer read GetContextContext write SetContextCount;
    class property FilterContext: TContext3D read GetFilterContext;
    class property FilterTexture: TTexture read GetFilterTexture;
  end;

implementation

uses
  System.Math.Vectors, System.TypInfo, System.SysUtils, FMX.Materials, FMX.Surfaces;

{$R *.res}

{ TFilterValueRec }

constructor TFilterValueRec.Create(const AName, ADesc: string; AType: TFilterValueType; ADefault, AMin, AMax: TValue);
begin
  Self.Name := AName;
  Self.Desc := ADesc;
  Self.ValueType := AType;
  Self.Value := ADefault;
  Self.Default := ADefault;
  Self.Min := AMin;
  Self.Max := AMax;
end;

constructor TFilterValueRec.Create(const AName, ADesc: string; ADefault: TAlphaColor);
begin
  Self.Name := AName;
  Self.Desc := ADesc;
  Self.ValueType := TFilterValueType.Color;
  Self.Value := TValue.From<TAlphaColor>(ADefault);
  Self.Default := TValue.From<TAlphaColor>(ADefault);
end;

constructor TFilterValueRec.Create(const AName, ADesc: string; ADefault, AMin, AMax: Single);
begin
  Self.Name := AName;
  Self.Desc := ADesc;
  Self.ValueType := TFilterValueType.Float;
  Self.Value := ADefault;
  Self.Default := ADefault;
  Self.Min := AMin;
  Self.Max := AMax;
end;

constructor TFilterValueRec.Create(const AName, ADesc: string; ADefault, AMin, AMax: TPointF);
begin
  Self.Name := AName;
  Self.Desc := ADesc;
  Self.ValueType := TFilterValueType.Point;
  Self.Value := TValue.From<TPointF>(ADefault);
  Self.Default := TValue.From<TPointF>(ADefault);
  Self.Min := TValue.From<TPointF>(AMin);
  Self.Max := TValue.From<TPointF>(AMax);
end;

constructor TFilterValueRec.Create(const AName, ADesc: string; AType: TFilterValueType);
begin
  Self.Name := AName;
  Self.Desc := ADesc;
  Self.ValueType := AType;
end;

{ TFilterRec }

constructor TFilterRec.Create(const AName, ADesc: string; AValues: array of TFilterValueRec);
var
  i: Integer;
begin
  Self.Name := AName;
  Self.Desc := ADesc;
  SetLength(Self.Values, Length(AValues));
  for i := 0 to High(AValues) do
    Self.Values[i] := AValues[i];
end;

{ TFilterManager }

class function TFilterManager.FilterByName(const AName: string): TFilter;
var
  f: TFilterClass;
begin
  Result := nil;
  if FFilterList = nil then
    Exit;
  for f in FFilterList.Keys do
    if CompareText(f.FilterAttr.Name, AName) = 0 then
    begin
      Result := f.Create;
      Exit;
    end;
end;

class function TFilterManager.FilterClassByName(const AName: string): TFilterClass;
var
  f: TFilterClass;
begin
  Result := nil;
  if FFilterList = nil then
    Exit;
  for f in FFilterList.Keys do
    if CompareText(f.FilterAttr.Name, AName) = 0
    then
    begin
      Result := f;
      Exit;
    end;
end;

class function TFilterManager.GetContextContext: Integer;
begin
  if Assigned(FContextList) then
    Result := FContextList.Count
  else
    Result := 0;
end;

class function TFilterManager.GetFilterContext: TContext3D;
begin
  if Assigned(FContextList) then
    Result := FContextList[FCurrentContext].Context
  else
    Result := nil;
end;

class function TFilterManager.GetFilterTexture: TTexture;
begin
  if Assigned(FContextList) then
    Result := FContextList[FCurrentContext].Texture
  else
    Result := nil;
end;

class function TFilterManager.GetTexture(const Index: Integer): TTexture;
begin
  if Assigned(FContextList) and (Index < FContextList.Count) then
    Result := FContextList[Index].Texture
  else
    Result := nil;
end;

class procedure TFilterManager.RegisterFilter(const Category: string; Filter: TFilterClass);
begin
  if FFilterList = nil then
  begin
    FFilterList := TFilterClassDict.Create;
  end;
  FFilterList.Add(Filter, Category);
end;

class procedure TFilterManager.ResizeContext(Width, Height: Integer);
var
  I: Integer;
  Rec: TContextRec;
  CurWidth, CurHeight: Integer;
begin
  if not Assigned(FContextList) then
    FContextList := TList<TContextRec>.Create;

  CurWidth := 0;
  CurHeight := 0;
  for I := 0 to FContextList.Count - 1 do
  begin
    Rec := FContextList[I];
    if Assigned(Rec.Context) then
      Rec.Context.DisposeOf;
    if Assigned(Rec.Texture) then
    begin
      if Rec.Texture.Width > CurWidth then
        CurWidth := Rec.Texture.Width;
      if Rec.Texture.Height > CurHeight then
        CurHeight := Rec.Texture.Height;
      Rec.Texture.DisposeOf;
    end;
    Rec.Texture := nil;
    Rec.Context := nil;
    FContextList[I] := Rec;
  end;

  if Width > CurWidth then
    CurWidth := Width;
  if Height > CurHeight then
    CurHeight := Height;

  for I := 0 to FContextList.Count - 1 do
  begin
    Rec.Texture := TTexture.Create;
    Rec.Texture.SetSize(CurWidth, CurHeight);
    Rec.Texture.Style := [TTextureStyle.RenderTarget];
    Rec.Context := TContextManager.CreateFromTexture(Rec.Texture, TMultisample.None, False);
    FContextList[I] := Rec;
  end;
end;

class procedure TFilterManager.SetContextCount(const Value: Integer);
begin
  if not Assigned(FContextList) then
    FContextList := TList<TContextRec>.Create;
  FContextList.Count := Value;
end;

class procedure TFilterManager.SetCurrentContext(const Value: Integer);
begin
  FCurrentContext := Value;
  if not Assigned(FContextList) then
    FContextList := TList<TContextRec>.Create;
  if (FCurrentContext > FContextList.Count) then
    FContextList.Count := FCurrentContext;
end;

class procedure TFilterManager.FillCategory(AList: TStrings);
var
  s: string;
begin
  AList.Clear;
  if FFilterList = nil then
    Exit;
  for s in FFilterList.Values do
    if AList.IndexOf(s) < 0 then
      AList.Add(s);
end;

class procedure TFilterManager.FillFiltersInCategory(const Category: string; AList: TStrings);
var
  pair: TPair<TFilterClass, string>;
begin
  AList.Clear;
  if FFilterList = nil then
    Exit;
  for pair in FFilterList do
    if pair.Value = Category then
      AList.Add(pair.Key.FilterAttr.Name);
end;

class procedure TFilterManager.UnInitialize;
var
  I: Integer;
  Rec: TContextRec;
begin
  TFilter.UnInitialize;
  if Assigned(FContextList) then
    for I := 0 to FContextList.Count - 1 do
    begin
      Rec := FContextList[I];
      FreeAndNil(Rec.Context);
      FreeAndNil(Rec.Texture);
    end;
  FreeAndNil(FContextList);
  FreeAndNil(FFilterList);
end;

{ TFilterMaterial }

type

  TFilterMaterial = class(TMaterial)
  private
    FFilter: TFilter;
    procedure SetFilter(const Value: TFilter);
  protected
    procedure DoApply(const Context: TContext3D); override;
    procedure DoInitialize; override;
    class function DoGetMaterialProperty(const Prop: TMaterial.TProperty): string; override;
  public
    property Filter: TFilter write SetFilter;
  end;

{ TFilterMaterial }

class function TFilterMaterial.DoGetMaterialProperty(const Prop: TMaterial.TProperty): string;
begin
  case Prop of
    TProperty.ModelViewProjection: Result := 'MVPMatrix';
  else
    Result := '';
  end;
end;

procedure TFilterMaterial.DoInitialize;
begin
end;

procedure TFilterMaterial.DoApply(const Context: TContext3D);
begin
  // Shaders
  FFilter.LoadShaders;
  // Textures
  FFilter.LoadTextures;
end;

procedure TFilterMaterial.SetFilter(const Value: TFilter);
begin
  FFilter := Value;
end;

{ TFilter }

constructor TFilter.Create;
begin
  inherited Create;
  FVertexShader := TShaderManager.RegisterShaderFromData('filter.fvs', TContextShaderKind.VertexShader, '', [
    TContextShaderSource.Create(TContextShaderArch.DX9, [
      $00, $02, $FE, $FF, $FE, $FF, $1F, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $53, $00, $00, $00, $00, $02, $FE, $FF, $01, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $4C, $00, $00, $00,
      $30, $00, $00, $00, $02, $00, $00, $00, $04, $00, $00, $00, $3C, $00, $00, $00, $00, $00, $00, $00, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $00, $AB, $AB, $03, $00, $03, $00, $04, $00, $04, $00,
      $01, $00, $00, $00, $00, $00, $00, $00, $76, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65,
      $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $0F, $90, $1F, $00, $00, $02, $05, $00, $00, $80, $01, $00, $0F, $90, $05, $00, $00, $03,
      $00, $00, $0F, $80, $00, $00, $55, $90, $01, $00, $E4, $A0, $04, $00, $00, $04, $00, $00, $0F, $80, $00, $00, $E4, $A0, $00, $00, $00, $90, $00, $00, $E4, $80, $04, $00, $00, $04, $00, $00, $0F, $80,
      $02, $00, $E4, $A0, $00, $00, $AA, $90, $00, $00, $E4, $80, $02, $00, $00, $03, $00, $00, $0F, $C0, $00, $00, $E4, $80, $03, $00, $E4, $A0, $01, $00, $00, $02, $00, $00, $03, $E0, $01, $00, $E4, $90,
      $FF, $FF, $00, $00], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 4)]
    ),
    TContextShaderSource.Create(TContextShaderArch.DX11_level_9, [
      $44, $58, $42, $43, $F8, $1F, $8F, $3D, $4B, $A9, $C9, $45, $B4, $6C, $29, $4E, $9F, $CF, $27, $39, $01, $00, $00, $00, $04, $04, $00, $00, $06, $00, $00, $00, $38, $00, $00, $00, $08, $01, $00, $00,
      $0C, $02, $00, $00, $88, $02, $00, $00, $58, $03, $00, $00, $AC, $03, $00, $00, $41, $6F, $6E, $39, $C8, $00, $00, $00, $C8, $00, $00, $00, $00, $02, $FE, $FF, $94, $00, $00, $00, $34, $00, $00, $00,
      $01, $00, $24, $00, $00, $00, $30, $00, $00, $00, $30, $00, $00, $00, $24, $00, $01, $00, $30, $00, $00, $00, $00, $00, $04, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $02, $FE, $FF,
      $1F, $00, $00, $02, $05, $00, $00, $80, $00, $00, $0F, $90, $1F, $00, $00, $02, $05, $00, $01, $80, $01, $00, $0F, $90, $05, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $55, $90, $02, $00, $E4, $A0,
      $04, $00, $00, $04, $00, $00, $0F, $80, $01, $00, $E4, $A0, $00, $00, $00, $90, $00, $00, $E4, $80, $04, $00, $00, $04, $00, $00, $0F, $80, $03, $00, $E4, $A0, $00, $00, $AA, $90, $00, $00, $E4, $80,
      $02, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $80, $04, $00, $E4, $A0, $04, $00, $00, $04, $00, $00, $03, $C0, $00, $00, $FF, $80, $00, $00, $E4, $A0, $00, $00, $E4, $80, $01, $00, $00, $02,
      $00, $00, $0C, $C0, $00, $00, $E4, $80, $01, $00, $00, $02, $00, $00, $03, $E0, $01, $00, $E4, $90, $FF, $FF, $00, $00, $53, $48, $44, $52, $FC, $00, $00, $00, $40, $00, $01, $00, $3F, $00, $00, $00,
      $59, $00, $00, $04, $46, $8E, $20, $00, $00, $00, $00, $00, $04, $00, $00, $00, $5F, $00, $00, $03, $72, $10, $10, $00, $00, $00, $00, $00, $5F, $00, $00, $03, $32, $10, $10, $00, $01, $00, $00, $00,
      $65, $00, $00, $03, $32, $20, $10, $00, $00, $00, $00, $00, $67, $00, $00, $04, $F2, $20, $10, $00, $01, $00, $00, $00, $01, $00, $00, $00, $68, $00, $00, $02, $01, $00, $00, $00, $36, $00, $00, $05,
      $32, $20, $10, $00, $00, $00, $00, $00, $46, $10, $10, $00, $01, $00, $00, $00, $38, $00, $00, $08, $F2, $00, $10, $00, $00, $00, $00, $00, $56, $15, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00,
      $00, $00, $00, $00, $01, $00, $00, $00, $32, $00, $00, $0A, $F2, $00, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $10, $10, $00, $00, $00, $00, $00,
      $46, $0E, $10, $00, $00, $00, $00, $00, $32, $00, $00, $0A, $F2, $00, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $02, $00, $00, $00, $A6, $1A, $10, $00, $00, $00, $00, $00,
      $46, $0E, $10, $00, $00, $00, $00, $00, $00, $00, $00, $08, $F2, $20, $10, $00, $01, $00, $00, $00, $46, $0E, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $03, $00, $00, $00,
      $3E, $00, $00, $01, $53, $54, $41, $54, $74, $00, $00, $00, $06, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $04, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $52, $44, $45, $46, $C8, $00, $00, $00, $01, $00, $00, $00, $48, $00, $00, $00, $01, $00, $00, $00, $1C, $00, $00, $00, $00, $04, $FE, $FF, $00, $11, $00, $00,
      $94, $00, $00, $00, $3C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $24, $47, $6C, $6F,
      $62, $61, $6C, $73, $00, $AB, $AB, $AB, $3C, $00, $00, $00, $01, $00, $00, $00, $60, $00, $00, $00, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $78, $00, $00, $00, $00, $00, $00, $00,
      $40, $00, $00, $00, $02, $00, $00, $00, $84, $00, $00, $00, $00, $00, $00, $00, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $00, $AB, $AB, $03, $00, $03, $00, $04, $00, $04, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $48, $4C, $53, $4C, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $39,
      $2E, $32, $36, $2E, $39, $35, $32, $2E, $32, $38, $34, $34, $00, $AB, $AB, $AB, $49, $53, $47, $4E, $4C, $00, $00, $00, $02, $00, $00, $00, $08, $00, $00, $00, $38, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $07, $07, $00, $00, $41, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $03, $03, $00, $00,
      $50, $4F, $53, $49, $54, $49, $4F, $4E, $00, $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $AB, $AB, $4F, $53, $47, $4E, $50, $00, $00, $00, $02, $00, $00, $00, $08, $00, $00, $00, $38, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $03, $0C, $00, $00, $41, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00,
      $0F, $00, $00, $00, $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $53, $56, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $00, $AB, $AB, $AB], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 64)]
    ),
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $32, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76,
      $65, $63, $33, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $3B, $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0D, $0A, $76, $65, $63,
      $34, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $72,
      $30, $30, $30, $33, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $76, $30, $30, $30, $33, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $34, $20, $5F, $4D, $56, $50, $4D, $61,
      $74, $72, $69, $78, $5B, $34, $5D, $3B, $0D, $0A, $76, $6F, $69, $64, $20, $6D, $61, $69, $6E, $28, $29, $0D, $0A, $7B, $0D, $0A, $20, $20, $20, $20, $5F, $76, $30, $30, $30, $33, $20, $3D, $20, $76,
      $65, $63, $34, $28, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $78, $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $79, $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69,
      $6F, $6E, $2E, $7A, $2C, $20, $31, $2E, $30, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $78, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72,
      $69, $78, $5B, $30, $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $79, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50,
      $4D, $61, $74, $72, $69, $78, $5B, $31, $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $7A, $20, $3D, $20, $64, $6F, $74, $28,
      $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $32, $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $77, $20, $3D, $20,
      $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $33, $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F, $70, $6F, $73, $31,
      $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F,
      $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $54, $45, $58, $30, $2E, $78, $79, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A,
      $20, $20, $20, $20, $67, $6C, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $7D, $20, $0D, $0A], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 4)]
    )
  ]);
  SetLength(FShaders, 10);
  FPassCount := 1;
  FValues := FilterAttr.Values;
  SetLength(FValues, Length(FValues) + 2);
  FValues[High(FValues) - 1] := TFilterValueRec.Create('Input', '', TFilterValueType.Bitmap, '', '', '');
  FValues[High(FValues)] := TFilterValueRec.Create('Output', '', TFilterValueType.Bitmap, '', '', '');
  FModified := True;
end;

destructor TFilter.Destroy;
begin
  if (FInputBitmap <> nil) and not (TCanvasStyle.NeedGPUSurface in TCanvasManager.DefaultCanvas.GetCanvasStyle) then
    FInput.Free;
  if (FTargetBitmap <> nil) and not (TCanvasStyle.NeedGPUSurface in TCanvasManager.DefaultCanvas.GetCanvasStyle) then
    FTarget.Free;
  FreeAndNil(FPassInputRTContext);
  FreeAndNil(FPassInputRT);
  FreeAndNil(FInputRTContext);
  FreeAndNil(FInputRT);
  FreeAndNil(FTargetRTContext);
  FreeAndNil(FTargetRT);
  FreeAndNil(FOutputBitmap);
  inherited;
end;

class procedure TFilter.UnInitialize;
begin
  FreeAndNil(FNoise);
end;

class function TFilter.FilterAttr: TFilterRec;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

class function TFilter.FilterAttrForClass(C: TFilterClass): TFilterRec;
begin
  Result := C.FilterAttr;
end;

function TFilter.GetFilterValues(const Index: string): TValue;
var
  I: Integer;
begin
  for I := 0 to High(FValues) do
    if CompareText(FValues[I].Name, Index) = 0 then
    begin
      if CompareText(FValues[I].Name, 'Output') = 0 then
      begin
        if not FProcessing and FModified then 
          Apply;
        Result := TFilterManager.GetTexture(FPass);
        FValues[I].Value := Result;
        Exit;
      end;
      Result := FValues[I].Value;
      Exit;
    end;
  Result := TValue.Empty;
end;

procedure TFilter.SetFilterValues(const Index: string; Value: TValue);
var
  I: Integer;
begin
  for I := 0 to High(FValues) do
    if CompareText(FValues[I].Name, Index) = 0 then
    begin
      case FValues[I].ValueType of
        TFilterValueType.Float:
          begin
            if Value.AsExtended < FValues[I].Min.AsExtended then
              Value := FValues[I].Min.AsExtended;
            if Value.AsExtended > FValues[I].Max.AsExtended then
              Value := FValues[I].Max.AsExtended;
            FValues[I].Value := Value
          end;
        TFilterValueType.Point:
          begin
            FValues[I].Value := Value
          end;
        TFilterValueType.Color:
          begin
            FValues[I].Value := Value;
          end;
        TFilterValueType.Bitmap:
          begin
            if Value.IsObject and ((Value.AsObject is TBitmap) or (Value.AsObject is TTexture)) then
            begin
              if CompareText(Index, 'Output') = 0 then
                Exit;
              FValues[I].Value := Value;
              if CompareText(Index, 'Input') = 0 then
              begin
                if Value.AsObject is TBitmap then
                begin
                  FInputBitmap := TBitmap(Value.AsObject);
                  if not (TCanvasStyle.NeedGPUSurface in TCanvasManager.DefaultCanvas.GetCanvasStyle) then
                    FreeAndNil(FInput);
                  FInput := TContextManager.DefaultContextClass.BitmapToTexture(FInputBitmap);
                end;
                if Value.AsObject is TTexture then
                  FInput := TTexture(Value.AsObject);
              end
              else
              if CompareText(Index, 'Target') = 0 then
              begin
                if Value.AsObject is TBitmap then
                begin
                  FTargetBitmap := TBitmap(Value.AsObject);
                  if not (TCanvasStyle.NeedGPUSurface in TCanvasManager.DefaultCanvas.GetCanvasStyle) then
                    FreeAndNil(FTarget);
                  FTarget := TContextManager.DefaultContextClass.BitmapToTexture(FTargetBitmap);
                end;
                if Value.AsObject is TTexture then
                  FTarget := TTexture(Value.AsObject);
              end
              else
              if Value.AsObject is TBitmap then
              begin
                FValues[I].Bitmap := TBitmap(Value.AsObject);
                FValues[I].Value := TContextManager.DefaultContextClass.BitmapToTexture(TBitmap(Value.AsObject));
              end;
            end;
          end;
      end;
      FModified := True;
      Exit;
    end;
end;

function TFilter.GetFilterValuesAsColor(const Index: string): TAlphaColor;
begin
  Result := Values[Index].AsType<TAlphaColor>;
end;

procedure TFilter.SetFilterValuesAsBitmap(const Index: string; const Value: TBitmap);
begin
  Values[Index] := Value;
end;

procedure TFilter.SetFilterValuesAsColor(const Index: string; const Value: TAlphaColor);
begin
  Values[Index] := TValue.From<TAlphaColor>(Value);
end;

function TFilter.GetFilterValuesAsPoint(const Index: string): TPointF;
begin
  Result := Values[Index].AsType<TPointF>;
end;

function TFilter.GetFilterValuesAsBitmap(const Index: string): TBitmap;
begin
  if SameText(Index, 'Output') then
  begin
    // real copy to output bitmap
    if FOutputBitmap = nil then
      FOutputBitmap := TBitmap.Create(0, 0);
    Values[Index]; // apply
    Result := FOutputBitmap;
  end
  else
    Result := nil;
end;

function TFilter.GetFilterValuesAsTexture(const Index: string): TTexture;
begin
  if not Values[Index].IsEmpty then
    Result := TTexture(Values[Index].AsObject)
  else
    Result := nil;
end;

function TFilter.GetFilterValuesAsFloat(const Index: string): Single;
begin
  Result := Values[Index].AsType<Single>;
end;

procedure TFilter.SetFilterValuesAsPoint(const Index: string; const Value: TPointF);
begin
  Values[Index] := TValue.From<TPointF>(Value);
end;

procedure TFilter.SetFilterValuesAsTexture(const Index: string; const Value: TTexture);
begin
  Values[Index] := Value;
end;

procedure TFilter.SetFilterValuesAsFloat(const Index: string; const Value: Single);
begin
  Values[Index] := TValue.From<Single>(Value);
end;

procedure TFilter.SetInputFilter(const Value: TFilter);
begin
  FInputFilter := Value;
  FModified := True;
end;

procedure TFilter.LoadShaders;
var
  i: Integer;
  C: TAlphaColorRec;
begin
  if FShaders[FPass] <> nil then
  begin
    // Shaders
    TFilterManager.FilterContext.SetShaders(FVertexShader, FShaders[FPass]);
    // Params
    for i := 0 to High(FValues) do
    begin
      case FValues[I].ValueType of
        TFilterValueType.Bitmap: ;
        TFilterValueType.Float:
          TFilterManager.FilterContext.SetShaderVariable(FValues[I].Name, [Vector3D(FValues[I].Value.AsExtended, FValues[I].Value.AsExtended, FValues[I].Value.AsExtended, FValues[I].Value.AsExtended)]);
        TFilterValueType.Point:
          if (InputSize.Width > 0) and (InputSize.Height > 0) then
          begin
            if TContextStyle.RenderTargetFlipped in TFilterManager.FilterContext.Style then
              TFilterManager.FilterContext.SetShaderVariable(FValues[I].Name, [Vector3D(FValues[I].Value.AsType<TPointF>.x / InputSize.Width,
                (InputSize.Height - FValues[I].Value.AsType<TPointF>.y) / InputSize.Height, 0, 0)])
            else
              TFilterManager.FilterContext.SetShaderVariable(FValues[I].Name, [Vector3D(FValues[I].Value.AsType<TPointF>.x / InputSize.Width,
                FValues[I].Value.AsType<TPointF>.y / InputSize.Height, 0, 0)]);
          end;
        TFilterValueType.Color:
          begin
            C := TAlphaColorRec(FValues[I].Value.AsType<TAlphaColor>());
            TFilterManager.FilterContext.SetShaderVariable(FValues[I].Name, [Vector3D(C.R / $FF, C.G / $FF, C.B / $FF, C.A / $FF)]);
          end;
      end;
    end;
  end;
end;

procedure TFilter.RenderTextureToContext(const Context: TContext3D; const Texture: TTexture; const ARect: TRect; const DstPos: TPoint);
var
  Ver: TVertexBuffer;
  Ind: TIndexBuffer;
  Mat: TTextureMaterial;
begin
  if Context.BeginScene then
  try
    Ver := TVertexBuffer.Create([TVertexFormat.Vertex, TVertexFormat.TexCoord0], 4);
    Ver.Vertices[0] := Point3D(ARect.Left, ARect.Top, 0);
    Ver.Vertices[1] := Point3D(ARect.Right, ARect.Top, 0);
    Ver.Vertices[2] := Point3D(ARect.Right, ARect.Bottom, 0);
    Ver.Vertices[3] := Point3D(ARect.Left, ARect.Bottom, 0);
    Ver.TexCoord0[0] := PointF((DstPos.X + ARect.Left) / Texture.Width, (DstPos.Y + ARect.Top) / Texture.Height);
    Ver.TexCoord0[1] := PointF((DstPos.X + ARect.Right) / Texture.Width, (DstPos.Y + ARect.Top) / Texture.Height);
    Ver.TexCoord0[2] := PointF((DstPos.X + ARect.Right) / Texture.Width, (DstPos.Y + ARect.Bottom) / Texture.Height);
    Ver.TexCoord0[3] := PointF((DstPos.X + ARect.Left) / Texture.Width, (DstPos.Y + ARect.Bottom) / Texture.Height);
    Ind := TIndexBuffer.Create(6);
    Ind[0] := 0;
    Ind[1] := 1;
    Ind[2] := 3;
    Ind[3] := 3;
    Ind[4] := 1;
    Ind[5] := 2;
    Mat := TTextureMaterial.Create;
    Mat.Texture := Texture;
    Context.SetMatrix(TMatrix3D.Identity);
    Context.SetContextState(TContextState.cs2DScene);
    Context.SetContextState(TContextState.csZWriteOff);
    Context.SetContextState(TContextState.csZTestOff);
    Context.SetContextState(TContextState.csAllFace);
    Context.SetContextState(TContextState.csAlphaBlendOff);
    Context.SetContextState(TContextState.csScissorOff);
    Context.Clear(0);
    Context.DrawTriangles(Ver, Ind, Mat, 1);
    Mat.Free;
    Ind.Free;
    Ver.Free;
  finally
    Context.EndScene;
  end;
end;

function TFilter.InputTexture: TTexture;
begin
  if TCanvasStyle.NeedGPUSurface in TCanvasManager.DefaultCanvas.GetCanvasStyle then
    Result := TContextManager.DefaultContextClass.BitmapToTexture(FInputBitmap)
  else
    Result := FInput;
end;

function TFilter.TargetTexture: TTexture;
begin
  if TCanvasStyle.NeedGPUSurface in TCanvasManager.DefaultCanvas.GetCanvasStyle then
    Result := TContextManager.DefaultContextClass.BitmapToTexture(FTargetBitmap)
  else
    Result := FTarget;
end;

procedure TFilter.LoadTextures;
var
  I: Integer;
begin
  if FPass = 0 then
  begin
    if (FInputFilter = nil) then
    begin
      if Assigned(FInputRT) then
        TFilterManager.FilterContext.SetShaderVariable('Input', FInputRT)
      else
        TFilterManager.FilterContext.SetShaderVariable('Input', InputTexture);
    end
    else
      TFilterManager.FilterContext.SetShaderVariable('Input', FInputRT);
  end
  else
    TFilterManager.FilterContext.SetShaderVariable('Input', FPassInputRT);
  if (FTarget <> nil) and not FTarget.IsEmpty then
  begin
    if Assigned(FTargetRT) then
      TFilterManager.FilterContext.SetShaderVariable('Target', FTargetRT)
    else
      TFilterManager.FilterContext.SetShaderVariable('Target', TargetTexture);
  end;
  if FNeedInternalSecondTex <> '' then
    TFilterManager.FilterContext.SetShaderVariable('Second', FNoise);
  { load another }
  for I := 0 to High(FValues) do
  begin
    if (FValues[I].ValueType = TFilterValueType.Bitmap) then
    begin
      if SameText(FValues[I].Name, 'input') then Continue;
      if SameText(FValues[I].Name, 'output') then Continue;
      if SameText(FValues[I].Name, 'second') then Continue;
      if SameText(FValues[I].Name, 'target') then Continue;
    end;
    if (FValues[I].ValueType = TFilterValueType.Bitmap) and not FValues[I].Value.IsEmpty and (FValues[I].Value.IsObject)
      and (FValues[I].Value.AsObject is TTexture) then
    begin
      if TCanvasStyle.NeedGPUSurface in TCanvasManager.DefaultCanvas.GetCanvasStyle then
        TFilterManager.FilterContext.SetShaderVariable(FValues[I].Name,
          TContextManager.DefaultContextClass.BitmapToTexture(FValues[I].Bitmap))
      else
        TFilterManager.FilterContext.SetShaderVariable(FValues[I].Name, TTexture(FValues[I].Value.AsObject));
    end;
  end;
end;

function TFilter.InputSize: TSize;
begin
  if Assigned(FInputFilter) then
    Result := FInputFilter.FOutputSize
  else if Assigned(FInput) then
    Result := TSize.Create(FInput.Width, FInput.Height)
  else
    Result := TSize.Create(0, 0);
end;

procedure TFilter.CalcSize(var W, H: Integer);
begin
  W := InputSize.Width;
  H := InputSize.Height;
end;

procedure TFilter.ApplyWithoutCopyToOutput;
begin
  FNoCopyForOutput := True;
  try
    Apply;
  finally
    FNoCopyForOutput := False;
  end;
end;

procedure TFilter.Apply;
var
  I, W, H: Integer;
begin
  if not FModified then
    Exit;
  FProcessing := True;
  try
    // Prepare
    if (FInputFilter <> nil) then
    begin
      TFilter(FInputFilter).FNoCopyForOutput := True;
      FInputFilter.Apply;
      CalcSize(W, H);
    end
    else
    begin
      if (FInput = nil) then
        Exit;
      CalcSize(W, H);
    end;
    if W * H = 0 then
      Exit;
    FOutputSize := TSize.Create(W, H);
    // Correct size
    if TFilterManager.ContextCount < FPassCount then
    begin
      TFilterManager.ContextCount := FPassCount;
      TFilterManager.ResizeContext(W, H);
    end;
    if (TFilterManager.FilterContext = nil) or ((W > TFilterManager.FilterTexture.Width) or (H > TFilterManager.FilterTexture.Height)) then
      TFilterManager.ResizeContext(W, H);
    // Prepare textures
    if (FInputFilter = nil) then
    begin
      if FAntiAlise then
      begin
        if FInputBitmap <> nil then
        begin
          if FInputRT = nil then
          begin
            FInputRT := TTexture.Create;
            FInputRT.Style := [TTextureStyle.RenderTarget];
            FInputRT.SetSize(FInput.Width + 2, FInput.Height + 2);
            FInputRTContext := TContextManager.CreateFromTexture(FInputRT, TMultisample.None, False);
          end;
          if (FInputRT.Width <> FInputBitmap.Width + 2) or (FInputRT.Height <> FInputBitmap.Height + 2) then
          begin
            FreeAndNil(FInputRTContext);
            FInputRT.SetSize(FInputBitmap.Width + 2, FInputBitmap.Height + 2);
            FInputRTContext := TContextManager.CreateFromTexture(FInputRT, TMultisample.None, False);
          end;
          RenderTextureToContext(FInputRTContext, InputTexture, TRect.Create(0, 0, FInput.Width, FInput.Height),
            TPoint.Create(1, 1));
        end
        else
        begin
          if FInputRT = nil then
          begin
            FInputRT := TTexture.Create;
            FInputRT.Style := [TTextureStyle.RenderTarget];
            FInputRT.SetSize(FInput.Width + 2, FInput.Height + 2);
            FInputRTContext := TContextManager.CreateFromTexture(FInputRT, TMultisample.None, False);
          end;
          if (FInputRT.Width <> FInput.Width + 2) or (FInputRT.Height <> FInput.Height + 2) then
          begin
            FreeAndNil(FInputRTContext);
            FInputRT.SetSize(FInput.Width + 2, FInput.Height + 2);
            FInputRTContext := TContextManager.CreateFromTexture(FInputRT, TMultisample.None, False);
          end;
          RenderTextureToContext(FInputRTContext, InputTexture, TRect.Create(0, 0, FInput.Width, FInput.Height),
            TPoint.Create(1, 1));
        end;
      end else begin
        if not (TTextureStyle.RenderTarget in FInput.Style) and (TContextStyle.RenderTargetFlipped in TFilterManager.FilterContext.Style) then
        begin
          if FInputRT = nil then
          begin
            FInputRT := TTexture.Create;
            FInputRT.Style := [TTextureStyle.RenderTarget];
            FInputRT.SetSize(FInput.Width, FInput.Height);
            FInputRTContext := TContextManager.CreateFromTexture(FInputRT, TMultisample.None, False);
          end;
          if (FInputRT.Width <> FInput.Width) or (FInputRT.Height <> FInput.Height) then
          begin
            FreeAndNil(FInputRTContext);
            FInputRT.SetSize(FInput.Width, FInput.Height);
            FInputRTContext := TContextManager.CreateFromTexture(FInputRT, TMultisample.None, False);
          end;
          RenderTextureToContext(FInputRTContext, InputTexture, TRect.Create(0, 0, FInput.Width, FInput.Height),
            TPoint.Zero);
        end;
      end;
    end
    else if Assigned(TFilterManager.FilterTexture) then
    begin
      if FInputRT = nil then
      begin
        FInputRT := TTexture.Create;
        FInputRT.Style := [TTextureStyle.RenderTarget];
        FInputRT.SetSize(InputSize.Width, InputSize.Height);
        FInputRTContext := TContextManager.CreateFromTexture(FInputRT, TMultisample.None, False);
      end;
      if (FInputRT.Width <> InputSize.Width) or (FInputRT.Height <> InputSize.Height) then
      begin
        FreeAndNil(FInputRTContext);
        FInputRT.SetSize(InputSize.Width, InputSize.Height);
        FInputRTContext := TContextManager.CreateFromTexture(FInputRT, TMultisample.None, False);
      end;
      RenderTextureToContext(FInputRTContext, TFilterManager.FilterTexture, TRect.Create(0, 0, InputSize.Width, InputSize.Height),
        TPoint.Zero);
    end;
    if (FTarget <> nil) and not FTarget.IsEmpty then
    begin
      if not (TTextureStyle.RenderTarget in FTarget.Style) and (TContextStyle.RenderTargetFlipped in TFilterManager.FilterContext.Style) then
      begin
        if FTargetRT = nil then
        begin
          FTargetRT := TTexture.Create;
          FTargetRT.Style := [TTextureStyle.RenderTarget];
          FTargetRT.SetSize(FTarget.Width, FTarget.Height);
          FTargetRTContext := TContextManager.CreateFromTexture(FTargetRT, TMultisample.None, False);
        end;
        if (FTargetRT.Width <> FTarget.Width) or (FTargetRT.Height <> FTarget.Height) then
        begin
          FreeAndNil(FTargetRTContext);
          FTargetRT.SetSize(FTarget.Width, FTarget.Height);
          FTargetRTContext := TContextManager.CreateFromTexture(FTargetRT, TMultisample.None, False);
        end;
        RenderTextureToContext(FTargetRTContext, TargetTexture, TRect.Create(0, 0, FTarget.Width, FTarget.Height),
          TPoint.Zero);
      end;
    end;
    if FNeedInternalSecondTex <> '' then
      CreateNoise;
    // Process passes
    for i := 0 to FPassCount - 1 do
    begin
      FPass := i;
      if FPass > 0 then
      begin
        if FPassInputRT = nil then
        begin
          FPassInputRT := TTexture.Create;
          FPassInputRT.Style := [TTextureStyle.RenderTarget];
          FPassInputRT.SetSize(W, H);
          FPassInputRTContext := TContextManager.CreateFromTexture(FPassInputRT, TMultisample.None, False);
        end;
        if (FPassInputRT.Width <> W) or (FPassInputRT.Height <> H) then
        begin
          FreeAndNil(FPassInputRTContext);
          FPassInputRT.SetSize(W, H);
          FPassInputRTContext := TContextManager.CreateFromTexture(FPassInputRT, TMultisample.None, False);
        end;
        RenderTextureToContext(FPassInputRTContext, TFilterManager.GetTexture(FPass - 1),
          TRect.Create(0, 0, FPassInputRT.Width, FPassInputRT.Height), TPoint.Zero);
      end;
      TFilterManager.CurrentContext := FPass;
      if TFilterManager.FilterContext.BeginScene then
      try
        TFilterManager.FilterContext.SetMatrix(TMatrix3D.Identity);
        TFilterManager.FilterContext.SetContextState(TContextState.cs2DScene);
        TFilterManager.FilterContext.SetContextState(TContextState.csZWriteOff);
        TFilterManager.FilterContext.SetContextState(TContextState.csZTestOff);
        TFilterManager.FilterContext.SetContextState(TContextState.csAllFace);
        TFilterManager.FilterContext.SetContextState(TContextState.csAlphaBlendOff);
        TFilterManager.FilterContext.SetContextState(TContextState.csScissorOff);
        TFilterManager.FilterContext.Clear(0);
        Render(W, H);
      finally
        TFilterManager.FilterContext.EndScene;
      end;
      // Copy result to output texture
      if not FNoCopyForOutput then
      begin
        if (FOutputBitmap <> nil) then
        begin
          FOutputBitmap.SetSize(FOutputSize.Width, FOutputSize.Height);
          TFilterManager.FilterContext.CopyToBitmap(FOutputBitmap,
            TRect.Create(0, 0, FOutputBitmap.Width, FOutputBitmap.Height));
        end;
      end;
    end;
  finally
    if (FInputFilter <> nil) then
    begin
      TFilter(FInputFilter).FNoCopyForOutput := False;
      FInput := nil;
    end;
    FProcessing := False;
    FModified := False;
  end;
end;

function CreateTextureFromData(const Data: array of Byte): TTexture;
var
  S: TStream;
  Surf: TBitmapSurface;
begin
  Result := TTexture.Create;
  Result.Style := [TTextureStyle.Dynamic];
  S := TMemoryStream.Create;
  try
    S.Write(Data[0], Length(Data));
    S.Position := 0;
    Surf := TBitmapSurface.Create;
    try
      if TBitmapCodecManager.LoadFromStream(S, Surf) then
        Result.Assign(Surf)
    finally
      Surf.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TFilter.CreateNoise;
var
  S: TStream;
begin
  if FNeedInternalSecondTex = '' then Exit;
  if FNoise = nil then
  begin
    S := TResourceStream.Create(HInstance, FNeedInternalSecondTex, RT_RCDATA);
    try
      FNoise := TTexture.Create;
      FNoise.Style := [TTextureStyle.Dynamic];
      FNoise.LoadFromStream(S);
    finally
      S.Free;
    end;
  end;
end;

function TFilter.CreateFilterMaterial: TMaterial;
begin
  Result := TFilterMaterial.Create;
  TFilterMaterial(Result).Filter := Self;
end;

procedure TFilter.Render(W, H: Integer);
var
  Ver: TVertexBuffer;
  Ind: TIndexBuffer;
  Mat: TMaterial;
  P: TPointF;
begin
  // Fill
  Ver := TVertexBuffer.Create([TVertexFormat.Vertex, TVertexFormat.TexCoord0], 4);
  P := TFilterManager.FilterContext.PixelToPixelPolygonOffset;
  if FAntiAlise then
  begin
    Ver.Vertices[0] := Point3D(-1 + P.X, -1 + P.Y, 0);
    Ver.Vertices[1] := Point3D(W + 1 + P.X, -1 + P.Y, 0);
    Ver.Vertices[2] := Point3D(W + 1 + P.X, H + 1 + P.Y, 0);
    Ver.Vertices[3] := Point3D(-1 + P.X, H + 1 + P.Y, 0);
  end
  else
  begin
    Ver.Vertices[0] := Point3D(0 + P.X, 0 + P.Y, 0);
    Ver.Vertices[1] := Point3D(W + P.X, 0 + P.Y, 0);
    Ver.Vertices[2] := Point3D(W + P.X, H + P.Y, 0);
    Ver.Vertices[3] := Point3D(0 + P.X, H + P.Y, 0);
  end;
  if TContextStyle.RenderTargetFlipped in TFilterManager.FilterContext.Style then
  begin
    Ver.TexCoord0[0] := PointF(0.0, 1.0);
    Ver.TexCoord0[1] := PointF(1.0, 1.0);
    Ver.TexCoord0[2] := PointF(1.0, 0.0);
    Ver.TexCoord0[3] := PointF(0.0, 0.0);
  end else begin
    Ver.TexCoord0[0] := PointF(0.0, 0.0);
    Ver.TexCoord0[1] := PointF(1.0, 0.0);
    Ver.TexCoord0[2] := PointF(1.0, 1.0);
    Ver.TexCoord0[3] := PointF(0.0, 1.0);
  end;
  Ind := TIndexBuffer.Create(6);
  Ind[0] := 0;
  Ind[1] := 1;
  Ind[2] := 3;
  Ind[3] := 3;
  Ind[4] := 1;
  Ind[5] := 2;
  Mat := CreateFilterMaterial;
  TFilterManager.FilterContext.DrawTriangles(Ver, Ind, Mat, 1);
  Mat.Free;
  Ind.Free;
  Ver.Free;
end;

procedure RegisterAliases;
begin
  AddEnumElementAliases(TypeInfo(TFilterValueType), ['vtFloat', 'vtPoint', 'vtColor', 'vtBitmap']);
end;

procedure UnregisterAliases;
begin
  RemoveEnumElementAliases(TypeInfo(TFilterValueType));
end;

initialization
  RegisterAliases;

finalization
  UnregisterAliases;
end.
