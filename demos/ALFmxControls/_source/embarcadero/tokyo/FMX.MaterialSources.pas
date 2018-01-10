{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.MaterialSources;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.Generics.Collections, System.UITypes, System.Messaging, FMX.Types, FMX.Types3D, FMX.Controls3D,
  FMX.Graphics;

type
  // This object used in design-time to link TMaterial
  TMaterialSource = class(TFmxObject)
  private
    FMaterial: TMaterial;
    FNotifierList: TList<TControl3D>;
    procedure DoMaterialChange(Sender: TObject);
  protected
    function CreateMaterial: TMaterial; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddChangeNotifier(const AObject: TControl3D);
    procedure RemoveChangeNotifier(const AObject: TControl3D);
    property Material: TMaterial read FMaterial;
    // Helpful function that check MaterialObject for nil
    class function ValidMaterial(const MaterialObject: TMaterialSource): TMaterial;
  end;

  TMaterialBook = class(TFmxObject)
  private
    FNoMaterialItems: TFmxObject;
    function GetMaterial(Index: Integer): TMaterialSource;
    function GetMaterialCount: Integer;
  protected
    procedure DoAddObject(const AObject: TFmxObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Materials[Index: Integer]: TMaterialSource read GetMaterial; default;
    property MaterialCount: Integer read GetMaterialCount;
  end;

  TColorMaterialSource = class(TMaterialSource)
  private
    function GetColor: TAlphaColor;
    procedure SetColor(const Value: TAlphaColor);
  protected
    function CreateMaterial: TMaterial; override;
  published
    property Color: TAlphaColor read GetColor write SetColor;
  end;

  TTextureMaterialSource = class(TMaterialSource)
  private
    FTexture: TBitmap;
    FContextResetId: Integer;
    procedure SetTexture(const Value: TBitmap);
    procedure DoTextureChanged(Sender: TObject);
    procedure ContextResetHandler(const Sender: TObject; const Msg: TMessage);
  protected
    function CreateMaterial: TMaterial; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Texture: TBitmap read FTexture write SetTexture;
  end;

  TLightMaterialSource = class(TMaterialSource)
  private
    FTexture: TBitmap;
    FContextResetId: Integer;
    function GetAmbient: TAlphaColor;
    function GetDiffuse: TAlphaColor;
    function GetEmissive: TAlphaColor;
    function GetShininess: Integer;
    function GetSpecular: TAlphaColor;
    procedure SetAmbient(const Value: TAlphaColor);
    procedure SetDiffuse(const Value: TAlphaColor);
    procedure SetEmissive(const Value: TAlphaColor);
    procedure SetShininess(const Value: Integer);
    procedure SetSpecular(const Value: TAlphaColor);
    procedure SetTexture(const Value: TBitmap);
    procedure DoTextureChanged(Sender: TObject);
    procedure ContextResetHandler(const Sender: TObject; const Msg: TMessage);
  protected
    function CreateMaterial: TMaterial; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Diffuse: TAlphaColor read GetDiffuse write SetDiffuse;
    property Ambient: TAlphaColor read GetAmbient write SetAmbient;
    property Emissive: TAlphaColor read GetEmissive write SetEmissive;
    property Specular: TAlphaColor read GetSpecular write SetSpecular;
    property Texture: TBitmap read FTexture write SetTexture;
    property Shininess: Integer read GetShininess write SetShininess;
  end;

implementation

uses
  System.SysUtils, FMX.Materials;

{ TMaterialSource }

constructor TMaterialSource.Create(AOwner: TComponent);
begin
  inherited;
  FMaterial := CreateMaterial;
  FMaterial.OnChange := DoMaterialChange;
end;

destructor TMaterialSource.Destroy;
begin
  FMaterial.Free;
  inherited;
  FreeAndNil(FNotifierList);
end;

procedure TMaterialSource.DoMaterialChange(Sender: TObject);
var
  Control: TControl3D;
begin
  if FNotifierList <> nil then
    for Control in FNotifierList do
      Control.Repaint;
end;

procedure TMaterialSource.AddChangeNotifier(const AObject: TControl3D);
begin
  if FNotifierList = nil then
    FNotifierList := TList<TControl3D>.Create;
  FNotifierList.Add(AObject);
end;

procedure TMaterialSource.RemoveChangeNotifier(const AObject: TControl3D);
begin
  if FNotifierList <> nil then
    FNotifierList.Remove(AObject);
end;

class function TMaterialSource.ValidMaterial(const MaterialObject: TMaterialSource): TMaterial;
begin
  if MaterialObject <> nil then
    Result := MaterialObject.Material
  else
    Result := nil;
end;

{ TMaterialBook }

constructor TMaterialBook.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TMaterialBook.Destroy;
begin
  FreeAndNil(FNoMaterialItems);
  inherited;
end;

procedure TMaterialBook.DoAddObject(const AObject: TFmxObject);
begin
  if AObject is TMaterialSource then
    inherited
  else
  begin
    FNoMaterialItems := TFmxObject.Create(nil);
    FNoMaterialItems.Stored := False;
    FNoMaterialItems.AddObject(AObject);
  end;
end;

function TMaterialBook.GetMaterial(Index: Integer): TMaterialSource;
begin
  Result := TMaterialSource(Children[Index]);
end;

function TMaterialBook.GetMaterialCount: Integer;
begin
  Result := ChildrenCount;
end;

{ TColorMaterialSource }

function TColorMaterialSource.CreateMaterial: TMaterial;
begin
  Result := TColorMaterial.Create;
end;

function TColorMaterialSource.GetColor: TAlphaColor;
begin
  Result := TColorMaterial(Material).Color;
end;

procedure TColorMaterialSource.SetColor(const Value: TAlphaColor);
begin
  TColorMaterial(Material).Color := Value;
end;

{ TTextureMaterialSource }

constructor TTextureMaterialSource.Create(AOwner: TComponent);
begin
  inherited;
  FTexture := TTextureBitmap.Create;
  FTexture.OnChange := DoTextureChanged;
  FContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, ContextResetHandler);
end;

destructor TTextureMaterialSource.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TContextResetMessage, FContextResetId);
  FreeAndNil(FTexture);
  inherited;
end;

function TTextureMaterialSource.CreateMaterial: TMaterial;
begin
  Result := TTextureMaterial.Create;
end;

procedure TTextureMaterialSource.DoTextureChanged(Sender: TObject);
begin
  if not FTexture.IsEmpty then
    TTextureMaterial(Material).Texture := TTextureBitmap(FTexture).Texture;
end;

procedure TTextureMaterialSource.SetTexture(const Value: TBitmap);
begin
  FTexture.Assign(Value);
end;

procedure TTextureMaterialSource.ContextResetHandler(const Sender: TObject; const Msg: TMessage);
begin
  DoTextureChanged(Self);
end;

{ TLightMaterialSource }

constructor TLightMaterialSource.Create(AOwner: TComponent);
begin
  inherited;
  FTexture := TTextureBitmap.Create;
  FTexture.OnChange := DoTextureChanged;
  FContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, ContextResetHandler);
end;

destructor TLightMaterialSource.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TContextResetMessage, FContextResetId);
  FreeAndNil(FTexture);
  inherited;
end;

procedure TLightMaterialSource.DoTextureChanged(Sender: TObject);
begin
  if not FTexture.IsEmpty then
    TLightMaterial(Material).Texture := TTextureBitmap(FTexture).Texture;
end;

function TLightMaterialSource.CreateMaterial: TMaterial;
begin
  Result := TLightMaterial.Create;
end;

function TLightMaterialSource.GetAmbient: TAlphaColor;
begin
  Result := TLightMaterial(Material).Ambient;
end;

function TLightMaterialSource.GetDiffuse: TAlphaColor;
begin
  Result := TLightMaterial(Material).Diffuse;
end;

function TLightMaterialSource.GetEmissive: TAlphaColor;
begin
  Result := TLightMaterial(Material).Emissive;
end;

function TLightMaterialSource.GetShininess: Integer;
begin
  Result := TLightMaterial(Material).Shininess;
end;

function TLightMaterialSource.GetSpecular: TAlphaColor;
begin
  Result := TLightMaterial(Material).Specular;
end;

procedure TLightMaterialSource.SetAmbient(const Value: TAlphaColor);
begin
  TLightMaterial(Material).Ambient := Value;
end;

procedure TLightMaterialSource.SetDiffuse(const Value: TAlphaColor);
begin
  TLightMaterial(Material).Diffuse := Value;
end;

procedure TLightMaterialSource.SetEmissive(const Value: TAlphaColor);
begin
  TLightMaterial(Material).Emissive := Value;
end;

procedure TLightMaterialSource.SetShininess(const Value: Integer);
begin
  TLightMaterial(Material).Shininess := Value;
end;

procedure TLightMaterialSource.SetSpecular(const Value: TAlphaColor);
begin
  TLightMaterial(Material).Specular := Value;
end;

procedure TLightMaterialSource.SetTexture(const Value: TBitmap);
begin
  FTexture.Assign(Value);
end;

procedure TLightMaterialSource.ContextResetHandler(const Sender: TObject; const Msg: TMessage);
begin
  DoTextureChanged(Self);
end;

initialization
  RegisterFmxClasses([TMaterialSource, TMaterialBook, TColorMaterialSource, TTextureMaterialSource, TLightMaterialSource]);
end.


