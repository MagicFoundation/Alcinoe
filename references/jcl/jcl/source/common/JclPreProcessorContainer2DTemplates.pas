{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclContainer2DTemplates.pas.                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet                                     }
{         <outchy att users dott sourceforge dott net>                                             }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet. All rights reserved.     }
{                                                                                                  }
{ Contributors:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclPreProcessorContainer2DTemplates;

interface

{$I jcl.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase,
  JclPreProcessorContainerTypes,
  JclPreProcessorContainer1DTemplates;

{$TYPEINFO ON}

type
  TJclContainerMapInfo = class
  private
    FCustomMapAttributes: TMapAttributes;
    FKnownMapAttributes: PKnownMapAttributes;
    FValueTypeInfo: TJclContainerTypeInfo;
    FKeyTypeInfo: TJclContainerTypeInfo;
    function GetCustomMapAttribute(Index: TMapAttributeID): string;
    function GetKeyAttribute(Index: TKeyAttributeID): string;
    function GetValueAttribute(Index: TValueAttributeID): string;
    procedure SetKeyAttribute(Index: TKeyAttributeID; const Value: string);
    procedure SetValueAttribute(Index: TValueAttributeID;
      const Value: string);
    function GetKeyOwnershipDeclaration: string;
    function GetValueOwnershipDeclaration: string;
  protected
    function GetKnownMap: Boolean;
    function GetMapAttribute(Index: TMapAttributeID): string;
    function IsMapAttributeStored(Index: TMapAttributeID): Boolean;
    procedure SetKnownMap(Value: Boolean);
    procedure SetMapAttribute(Index: TMapAttributeID; const Value: string);
    procedure TypeKnownTypeChange(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    property KnownMap: Boolean read GetKnownMap write SetKnownMap;
    property KnownMapAttributes: PKnownMapAttributes read FKnownMapAttributes;
    property CustomMapAttributes[Index: TMapAttributeID]: string read GetCustomMapAttribute;
    property MapAttributes[Index: TMapAttributeID]: string read GetMapAttribute write SetMapAttribute;
    property KeyAttributes[Index: TKeyAttributeID]: string read GetKeyAttribute write SetKeyAttribute;
    property KeyTypeInfo: TJclContainerTypeInfo read FKeyTypeInfo;
    property KeyOwnershipDeclaration: string read GetKeyOwnershipDeclaration;
    property ValueAttributes[Index: TValueAttributeID]: string read GetValueAttribute write SetValueAttribute;
    property ValueTypeInfo: TJclContainerTypeInfo read FValueTypeInfo;
    property ValueOwnershipDeclaration: string read GetValueOwnershipDeclaration;
  end;

  TJclMapInterfaceParams = class(TJclInterfaceParams)
  private
    FMapInfo: TJclContainerMapInfo;
    function GetKeyOwnershipDeclaration: string;
    function GetValueOwnershipDeclaration: string;
  protected
    function GetKeyAttribute(Index: TKeyAttributeID): string;
    function GetMapAttribute(Index: TMapAttributeID): string;
    function GetValueAttribute(Index: TValueAttributeID): string;
    function IsMapAttributeStored(Index: TMapAttributeID): Boolean;
    procedure SetKeyAttribute(Index: TKeyAttributeID; const Value: string);
    procedure SetMapAttribute(Index: TMapAttributeID; const Value: string);
    procedure SetValueAttribute(Index: TValueAttributeID; const Value: string);
  public
    property KeyOwnershipDeclaration: string read GetKeyOwnershipDeclaration;
    property MapInfo: TJclContainerMapInfo read FMapInfo write FMapInfo;
    property ValueOwnershipDeclaration: string read GetValueOwnershipDeclaration;
  end;

  TJclMapClassInterfaceParams = class(TJclMapInterfaceParams)
  protected
    FInterfaceAdditional: string;
    FSectionAdditional: string;
    function GetInterfaceAdditional: string; virtual;
    function GetSectionAdditional: string; virtual;
    function GetComparisonSectionAdditional: string; virtual; abstract;
  public
    property InterfaceAdditional: string read GetInterfaceAdditional write FInterfaceAdditional;
    property SectionAdditional: string read GetSectionAdditional write FSectionAdditional;
    property KeyTypeName: string index kaKeyTypeName read GetKeyAttribute write SetKeyAttribute stored False;
    property ValueTypeName: string index vaValueTypeName read GetValueAttribute write SetValueAttribute stored False;
  end;

  TJclMapImplementationParams = class(TJclImplementationParams)
  private
    function GetKeyOwnershipDeclaration: string;
    function GetValueOwnershipDeclaration: string;
    function GetMapInfo: TJclContainerMapInfo;
  protected
    function GetKeyAttribute(Index: TKeyAttributeID): string;
    function GetMapAttribute(Index: TMapAttributeID): string;
    function GetValueAttribute(Index: TValueAttributeID): string;
    procedure SetKeyAttribute(Index: TKeyAttributeID; const Value: string);
    procedure SetMapAttribute(Index: TMapAttributeID; const Value: string);
    procedure SetValueAttribute(Index: TValueAttributeID; const Value: string);
  public
    property KeyOwnershipDeclaration: string read GetKeyOwnershipDeclaration;
    property ValueOwnershipDeclaration: string read GetValueOwnershipDeclaration;
    property MapInfo: TJclContainerMapInfo read GetMapInfo;
  end;

  TJclMapClassImplementationParams = class(TJclMapImplementationParams)
  protected
    FCreateKeySet: string;
    FCreateKeyCollection: string;
    FCreateValueCollection: string;
    FMacroFooter: string;
    FOwnershipAssignments: string;
    function GetCreateKeySet: string;
    function GetCreateKeyCollection: string;
    function GetCreateValueCollection: string;
    function GetOwnershipAssignment: string;
    function GetSelfClassName: string; virtual; abstract;
  public
    function GetConstructorParameters: string; virtual; abstract;
    function GetMacroFooter: string; override;
    procedure ResetDefault(Value: Boolean); override;
    property MacroFooter: string read GetMacroFooter write FMacroFooter;
    property KeyTypeName: string index kaKeyTypeName read GetKeyAttribute write SetKeyAttribute stored False;
    property KeyDefault: string index kaKeyDefaultValue read GetKeyAttribute write SetKeyAttribute stored False;
    property KeyArraySetClassName: string index kaKeyArraySetClassName read GetKeyAttribute write SetKeyAttribute stored False;
    property KeyArrayListClassName: string index kaKeyArrayListClassName read GetKeyAttribute write SetKeyAttribute stored False;
    property ValueTypeName: string index vaValueTypeName read GetValueAttribute write SetValueAttribute stored False;
    property ValueDefault: string index vaValueDefaultValue read GetValueAttribute write SetValueAttribute stored False;
    property ValueArrayListClassName: string index vaValueArrayListClassName read GetValueAttribute write SetValueAttribute stored False;
    property OwnershipAssignments: string read GetOwnershipAssignment write FOwnershipAssignments;
    property CreateKeySet: string read GetCreateKeySet write FCreateKeySet;
    property CreateKeyCollection: string read GetCreateKeyCollection write FCreateKeyCollection;
    property CreateValueCollection: string read GetCreateValueCollection write FCreateValueCollection;
  end;

{$IFNDEF TYPEINFO_ON}
  {$TYPEINFO OFF}
{$ENDIF ~TYPEINFO_ON}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  System.TypInfo,
  System.SysUtils,
  Winapi.ActiveX,
  System.Win.ComObj,
  {$ELSE ~HAS_UNITSCOPE}
  TypInfo,
  SysUtils,
  ActiveX,
  ComObj,
  {$ENDIF ~HAS_UNITSCOPE}
  JclRTTI,
  JclSysUtils,
  JclContainerIntf,
  JclPreProcessorContainerKnownMaps;

//=== { TJclContainerMapInfo } ===============================================

constructor TJclContainerMapInfo.Create;
begin
  inherited Create;
  FKeyTypeInfo := TJclContainerTypeInfo.Create;
  FKeyTypeInfo.OnKnownTypeChange := TypeKnownTypeChange;
  FValueTypeInfo := TJclContainerTypeInfo.Create;
  FValueTypeInfo.OnKnownTypeChange := TypeKnownTypeChange;
end;

destructor TJclContainerMapInfo.Destroy;
begin
  FKeyTypeInfo.Free;
  FValueTypeInfo.Free;
  inherited Destroy;
end;

function TJclContainerMapInfo.GetCustomMapAttribute(Index: TMapAttributeID): string;
begin
  if (Index >= Low(TMapAttributeID)) and (Index <= High(TMapAttributeID)) then
    Result := FCustomMapAttributes[Index]
  else
  begin
    Error(reRangeError);
    Result := '';
  end;
end;

function TJclContainerMapInfo.GetKeyAttribute(Index: TKeyAttributeID): string;
begin
  if Index = kaKeyTypeName then
    Result := KeyTypeInfo.TypeName
  else
  if (Index >= Low(TKeyAttributeID)) and (Index <= High(TKeyAttributeID)) then
    Result := KeyTypeInfo.TypeAttributes[KeyAttributeInfos[Index]]
  else
  begin
    Error(reRangeError);
    Result := '';
  end;
end;

function TJclContainerMapInfo.GetKeyOwnershipDeclaration: string;
begin
  Result := GetKeyAttribute(kaKeyOwnershipParameterName);
  if Result <> '' then
    Result := 'AOwnsKeys: Boolean';
end;

function TJclContainerMapInfo.GetKnownMap: Boolean;
var
  Index: TMapAttributeID;
begin
  Result := Assigned(FKnownMapAttributes) and KeyTypeInfo.KnownType and ValueTypeInfo.KnownType;
  for Index := Low(TMapAttributeID) to High(TMapAttributeID) do
  begin
    Result := Result and (FCustomMapAttributes[Index] = '');
    if not Result then
      Break;
  end;
end;

function TJclContainerMapInfo.GetMapAttribute(
  Index: TMapAttributeID): string;
begin
  if (Index >= Low(TMapAttributeID)) and (Index <= High(TMapAttributeID)) then
  begin
    if FCustomMapAttributes[Index] = '' then
    begin
      if Assigned(FKnownMapAttributes) then
        Result := FKnownMapAttributes^.MapAttributes[Index]
      else
        Result := Format(MapAttributeInfos[Index].DefaultValue, [KeyTypeInfo.TypeName, ValueTypeInfo.TypeName]);
    end
    else
      Result := FCustomMapAttributes[Index];
  end
  else
  begin
    Error(reRangeError);
    Result := '';
  end;
end;

function TJclContainerMapInfo.GetValueAttribute(Index: TValueAttributeID): string;
begin
  if Index = vaValueTypeName then
    Result := ValueTypeInfo.TypeName
  else
  if (Index >= Low(TValueAttributeID)) and (Index <= High(TValueAttributeID)) then
    Result := ValueTypeInfo.TypeAttributes[ValueAttributeInfos[Index]]
  else
  begin
    Error(reRangeError);
    Result := '';
  end;
end;

function TJclContainerMapInfo.GetValueOwnershipDeclaration: string;
begin
  Result := GetValueAttribute(vaValueOwnershipParameterName);
  if Result <> '' then
    Result := 'AOwnsValues: Boolean';
end;

function TJclContainerMapInfo.IsMapAttributeStored(Index: TMapAttributeID): Boolean;
begin
  if (Index >= Low(TMapAttributeID)) and (Index <= High(TMapAttributeID)) then
    Result := FCustomMapAttributes[Index] <> ''
  else
  begin
    Error(reRangeError);
    Result := False;
  end;
end;

procedure TJclContainerMapInfo.SetKeyAttribute(Index: TKeyAttributeID;
  const Value: string);
begin
  if Index = kaKeyTypeName then
    KeyTypeInfo.TypeName := Value
  else
  if (Index >= Low(TKeyAttributeID)) and (Index <= High(TKeyAttributeID)) then
    KeyTypeInfo.TypeAttributes[KeyAttributeInfos[Index]] := Value
  else
    Error(reRangeError);
end;

procedure TJclContainerMapInfo.SetKnownMap(Value: Boolean);
var
  Index: TMapAttributeID;
  NewGUID: TGUID;
begin
  if Value then
  begin
    // reset to default values
    for Index := Low(TMapAttributeID) to High(TMapAttributeID) do
      FCustomMapAttributes[Index] := '';
  end
  else
  if {not Value and} Assigned(FKnownMapAttributes) then
  begin
    // copy with new GUIDs
    for Index := Low(TMapAttributeID) to High(TMapAttributeID) do
    begin
      if MapAttributeInfos[Index].IsGUID then
      begin
        OleCheck(CoCreateGuid(NewGUID));
        FCustomMapAttributes[Index] := GUIDToString(NewGUID);
      end
      else
        FCustomMapAttributes[Index] := FKnownMapAttributes^.MapAttributes[Index];
    end;
  end
  else
  begin
    {not Value and not Assigned(FKnownTypeAttributes)}
    // default names with new GUIDs
    for Index := Low(TMapAttributeID) to High(TMapAttributeID) do
    begin
      if MapAttributeInfos[Index].IsGUID then
      begin
        OleCheck(CoCreateGuid(NewGUID));
        FCustomMapAttributes[Index] := GUIDToString(NewGUID);
      end
      else
        FCustomMapAttributes[Index] := Format(MapAttributeInfos[Index].DefaultValue,
          [KeyTypeInfo.TypeName, ValueTypeInfo.TypeName]);
    end;
  end;
end;

procedure TJclContainerMapInfo.SetMapAttribute(Index: TMapAttributeID;
  const Value: string);
begin
  if (Index >= Low(TMapAttributeID)) and (Index <= High(TMapAttributeID)) then
    FCustomMapAttributes[Index] := Value
  else
    Error(reRangeError);
end;

procedure TJclContainerMapInfo.SetValueAttribute(Index: TValueAttributeID;
  const Value: string);
begin
  if Index = vaValueTypeName then
    ValueTypeInfo.TypeName := Value
  else
  if (Index >= Low(TValueAttributeID)) and (Index <= High(TValueAttributeID)) then
    ValueTypeInfo.TypeAttributes[ValueAttributeInfos[Index]] := Value
  else
    Error(reRangeError);
end;

procedure TJclContainerMapInfo.TypeKnownTypeChange(Sender: TObject);
begin
  if KeyTypeInfo.KnownType and ValueTypeInfo.KnownType then
  begin
    FKnownMapAttributes := IsKnownMap(KeyTypeInfo.TypeName, ValueTypeInfo.TypeName);
    SetKnownMap(True);
  end;
end;

//=== { TJclMapInterfaceParams } =============================================

function TJclMapInterfaceParams.GetKeyAttribute(Index: TKeyAttributeID): string;
begin
  Result := MapInfo.KeyAttributes[Index];
end;

function TJclMapInterfaceParams.GetKeyOwnershipDeclaration: string;
begin
  Result := MapInfo.KeyOwnershipDeclaration;
  if Result <> '' then
    Result := '; ' + Result;
end;

function TJclMapInterfaceParams.GetMapAttribute(
  Index: TMapAttributeID): string;
begin
  Result := MapInfo.MapAttributes[Index];
end;

function TJclMapInterfaceParams.GetValueAttribute(
  Index: TValueAttributeID): string;
begin
  Result := MapInfo.ValueAttributes[Index];
end;

function TJclMapInterfaceParams.GetValueOwnershipDeclaration: string;
begin
  Result := MapInfo.ValueOwnershipDeclaration;
  if Result <> '' then
    Result := '; ' + Result;
end;

function TJclMapInterfaceParams.IsMapAttributeStored(
  Index: TMapAttributeID): Boolean;
begin
  Result := MapInfo.CustomMapAttributes[Index] <> '';
end;

procedure TJclMapInterfaceParams.SetKeyAttribute(Index: TKeyAttributeID;
  const Value: string);
begin
  MapInfo.KeyAttributes[Index] := Value;
end;

procedure TJclMapInterfaceParams.SetMapAttribute(Index: TMapAttributeID;
  const Value: string);
begin
  MapInfo.MapAttributes[Index] := Value;
end;

procedure TJclMapInterfaceParams.SetValueAttribute(Index: TValueAttributeID;
  const Value: string);
begin
  MapInfo.ValueAttributes[Index] := Value;
end;

//=== { TJclMapClassInterfaceParams } ========================================

function TJclMapClassInterfaceParams.GetInterfaceAdditional: string;
begin
  Result := FInterfaceAdditional;
  if Result = '' then
  begin
    if MapInfo.KeyTypeInfo.StringType or MapInfo.ValueTypeInfo.StringType then
      Result := ' IJclStrBaseContainer,';
    if MapInfo.KeyTypeInfo.TypeAttributes[taContainerInterfaceName] <> '' then
      Result := Format('%s %s,', [Result, MapInfo.KeyTypeInfo.TypeAttributes[taContainerInterfaceName]]);
    if (MapInfo.KeyTypeInfo.TypeName <> MapInfo.ValueTypeInfo.TypeName) and (MapInfo.ValueTypeInfo.TypeAttributes[taContainerInterfaceName] <> '') then
      Result := Format('%s %s,', [Result, MapInfo.ValueTypeInfo.TypeAttributes[taContainerInterfaceName]]);
    if MapInfo.KeyTypeInfo.TObjectType then
      Result := Result + ' IJclKeyOwner,';
    if MapInfo.ValueTypeInfo.TObjectType then
      Result := Result + ' IJclValueOwner,';
  end;
end;

function TJclMapClassInterfaceParams.GetSectionAdditional: string;
begin
  Result := FSectionAdditional;
  if (Result = '') and MapInfo.KnownMap then
  begin
    Result := NativeLineBreak +
              'protected' + NativeLineBreak +
              '  function CreateEmptyContainer: TJclAbstractContainerBase; override;' + NativeLineBreak;

    if not MapInfo.KeyTypeInfo.TObjectType then
      Result := Format('%s  function FreeKey(var Key: %s): %s;' + NativeLineBreak,
                       [Result, KeyTypeName, KeyTypeName]);
    if not MapInfo.ValueTypeInfo.TObjectType then
      Result := Format('%s  function FreeValue(var Value: %s): %s;' + NativeLineBreak,
                       [Result, ValueTypeName, ValueTypeName]);

    Result := Result + GetComparisonSectionAdditional;

    if MapInfo.KeyTypeInfo.TObjectType or MapInfo.ValueTypeInfo.TObjectType then
    begin
      if MapInfo.ValueTypeInfo.TObjectType then
        Result := '  FOwnsValues: Boolean;' + Result;
      if MapInfo.KeyTypeInfo.TObjectType then
        Result := '  FOwnsKeys: Boolean;' + NativeLineBreak + Result;
      Result := NativeLineBreak + 'private' + NativeLineBreak + Result + NativeLineBreak + 'public' + NativeLineBreak;
      if MapInfo.KeyTypeInfo.TObjectType then
        Result := Result + '  { IJclKeyOwner }' + NativeLineBreak +
                  '  function FreeKey(var Key: TObject): TObject;' + NativeLineBreak +
                  '  function GetOwnsKeys: Boolean;' + NativeLineBreak +
                  '  property OwnsKeys: Boolean read FOwnsKeys;';
      if MapInfo.KeyTypeInfo.TObjectType and MapInfo.ValueTypeInfo.TObjectType then
        Result := Result + NativeLineBreak;
      if MapInfo.ValueTypeInfo.TObjectType then
        Result := Result + '  { IJclValueOwner }' + NativeLineBreak +
                  '  function FreeValue(var Value: TObject): TObject;' + NativeLineBreak +
                  '  function GetOwnsValues: Boolean;' + NativeLineBreak +
                  '  property OwnsValues: Boolean read FOwnsValues;';
    end;
  end
end;

//=== { TJclMapImplementationParams } ========================================

function TJclMapImplementationParams.GetKeyAttribute(
  Index: TKeyAttributeID): string;
begin
  Result := (InterfaceParams as TJclMapInterfaceParams).GetKeyAttribute(Index);
end;

function TJclMapImplementationParams.GetKeyOwnershipDeclaration: string;
begin
  Result := (InterfaceParams as TJclMapInterfaceParams).GetKeyOwnershipDeclaration;
end;

function TJclMapImplementationParams.GetMapAttribute(
  Index: TMapAttributeID): string;
begin
  Result := (InterfaceParams as TJclMapInterfaceParams).GetMapAttribute(Index);
end;

function TJclMapImplementationParams.GetMapInfo: TJclContainerMapInfo;
begin
  Result := (InterfaceParams as TJclMapInterfaceParams).MapInfo;
end;

function TJclMapImplementationParams.GetValueAttribute(
  Index: TValueAttributeID): string;
begin
  Result := (InterfaceParams as TJclMapInterfaceParams).GetValueAttribute(Index);
end;

function TJclMapImplementationParams.GetValueOwnershipDeclaration: string;
begin
  Result := (InterfaceParams as TJclMapInterfaceParams).GetValueOwnershipDeclaration;
end;

procedure TJclMapImplementationParams.SetKeyAttribute(Index: TKeyAttributeID;
  const Value: string);
begin
  (InterfaceParams as TJclMapInterfaceParams).SetKeyAttribute(Index, Value);
end;

procedure TJclMapImplementationParams.SetMapAttribute(Index: TMapAttributeID;
  const Value: string);
begin
  (InterfaceParams as TJclMapInterfaceParams).SetMapAttribute(Index, Value);
end;

procedure TJclMapImplementationParams.SetValueAttribute(
  Index: TValueAttributeID; const Value: string);
begin
  (InterfaceParams as TJclMapInterfaceParams).SetValueAttribute(Index, Value);
end;

//=== { TJclMapClassImplementationParams } ===================================

function TJclMapClassImplementationParams.GetCreateKeySet: string;
var
  Ownership: string;
begin
  Result := FCreateKeySet;
  if Result = '' then
  begin
    if MapInfo.KeyTypeInfo.TypeAttributes[taOwnershipParameterName] <> '' then
      Ownership := ', False'
    else
      Ownership := '';
    Result := Format('%s.Create(FSize%s)', [KeyArraySetClassName, Ownership]);
  end;
end;

function TJclMapClassImplementationParams.GetCreateKeyCollection: string;
var
  Ownership: string;
begin
  Result := FCreateKeyCollection;
  if Result = '' then
  begin
    if MapInfo.KeyTypeInfo.TypeAttributes[taOwnershipParameterName] <> '' then
      Ownership := ', False'
    else
      Ownership := '';
    Result := Format('%s.Create(FSize%s)', [KeyArrayListClassName, Ownership]);
  end;
end;

function TJclMapClassImplementationParams.GetCreateValueCollection: string;
var
  Ownership: string;
begin
  Result := FCreateValueCollection;
  if Result = '' then
  begin
    if MapInfo.ValueTypeInfo.TypeAttributes[taOwnershipParameterName] <> '' then
      Ownership := ', False'
    else
      Ownership := '';
    Result := Format('%s.Create(FSize%s)', [ValueArrayListClassName, Ownership]);
  end;
end;

function TJclMapClassImplementationParams.GetMacroFooter: string;
var
  Ownership, SelfClassName, ConstructorParameters,
  FuncBody: string;
begin
  Result := FMacroFooter;

  if (Result = '') and MapInfo.KnownMap then
  begin
    if GetKeyAttribute(kaKeyOwnershipParameterName) <> '' then
    begin
      if GetValueAttribute(vaValueOwnershipParameterName) <> '' then
        Ownership := 'False, False'
      else
        Ownership := 'False';
    end
    else
    begin
      if GetValueAttribute(vaValueOwnershipParameterName) <> '' then
        Ownership := 'False'
      else
        Ownership := '';
    end;

    SelfClassName := GetSelfClassName;
    ConstructorParameters := GetConstructorParameters;

    if (ConstructorParameters <> '') and (Ownership <> '') then
      ConstructorParameters := ConstructorParameters + ', ' + Ownership
    else
    if ConstructorParameters = '' then
      ConstructorParameters := Ownership;
    if ConstructorParameters <> '' then
      ConstructorParameters := '(' + ConstructorParameters + ')';
    Result := Format(NativeLineBreak + NativeLineBreak +
                     'function %s.CreateEmptyContainer: TJclAbstractContainerBase;' + NativeLineBreak +
                     'begin' + NativeLineBreak +
                     '  Result := %s.Create%s;' + NativeLineBreak +
                     '  AssignPropertiesTo(Result);' + NativeLineBreak +
                     'end;' + NativeLineBreak,
                     [SelfClassName, SelfClassName, ConstructorParameters]);

    if MapInfo.KeyTypeInfo.TObjectType then
      FuncBody := Format('  if FOwnsKeys then' + NativeLineBreak +
                         '  begin' + NativeLineBreak +
                         '    Result := %s;' + NativeLineBreak +
                         '    FreeAndNil(Key);' + NativeLineBreak +
                         '  end' + NativeLineBreak +
                         '  else' + NativeLineBreak +
                         '  begin' + NativeLineBreak +
                         '    Result := Key;' + NativeLineBreak +
                         '    Key := %s;' + NativeLineBreak +
                         '  end;' + NativeLineBreak,
                         [KeyDefault, KeyDefault])
    else
      FuncBody := Format('  Result := Key;' + NativeLineBreak +
                         '  Key := %s;' + NativeLineBreak, [KeyDefault]);

    Result := Format('%s' + NativeLineBreak +
                     'function %s.FreeKey(var Key: %s): %s;' + NativeLineBreak +
                     'begin' + NativeLineBreak +
                     '%s' +
                     'end;' +  NativeLineBreak,
                     [Result, SelfClassName, KeyTypeName, KeyTypeName, FuncBody]);

    if MapInfo.ValueTypeInfo.TObjectType then
      FuncBody := Format('  if FOwnsValues then' + NativeLineBreak +
                         '  begin' + NativeLineBreak +
                         '    Result := %s;' + NativeLineBreak +
                         '    FreeAndNil(Value);' + NativeLineBreak +
                         '  end' + NativeLineBreak +
                         '  else' + NativeLineBreak +
                         '  begin' + NativeLineBreak +
                         '    Result := Value;' + NativeLineBreak +
                         '    Value := %s;' + NativeLineBreak +
                         '  end;' + NativeLineBreak,
                         [ValueDefault, ValueDefault])
    else
      FuncBody := Format('  Result := Value;' + NativeLineBreak +
                         '  Value := %s;' + NativeLineBreak, [ValueDefault]);

    Result := Format('%s' + NativeLineBreak +
                     'function %s.FreeValue(var Value: %s): %s;' + NativeLineBreak +
                     'begin' + NativeLineBreak +
                     '%s' +
                     'end;' + NativeLineBreak,
                     [Result, SelfClassName, ValueTypeName, ValueTypeName, FuncBody]);

    if MapInfo.KeyTypeInfo.TObjectType then
    begin
      Result := Format('%s' + NativeLineBreak +
                       'function %s.GetOwnsKeys: Boolean;' + NativeLineBreak +
                       'begin' + NativeLineBreak +
                       '  Result := FOwnsKeys;' + NativeLineBreak +
                       'end;' + NativeLineBreak,
                       [Result, SelfClassName]);
    end;

    if MapInfo.ValueTypeInfo.TObjectType then
    begin
      Result := Format('%s' + NativeLineBreak +
                       'function %s.GetOwnsValues: Boolean;' + NativeLineBreak +
                       'begin' + NativeLineBreak +
                       '  Result := FOwnsValues;' + NativeLineBreak +
                       'end;' + NativeLineBreak,
                       [Result, SelfClassName]);
    end;
  end;
end;

function TJclMapClassImplementationParams.GetOwnershipAssignment: string;
begin
  Result := FOwnershipAssignments;
  if Result = '' then
  begin
    if MapInfo.KeyTypeInfo.TObjectType then
      Result := NativeLineBreak + '  FOwnsKeys := AOwnsKeys;';
    if MapInfo.ValueTypeInfo.TObjectType then
      Result := Result + NativeLineBreak + '  FOwnsValues := AOwnsValues;';
  end;
end;

procedure TJclMapClassImplementationParams.ResetDefault(Value: Boolean);
begin
  inherited ResetDefault(Value);
  FCreateKeySet := '';
  FCreateValueCollection := '';
  FMacroFooter := '';
  FOwnershipAssignments := '';
  if not Value then
  begin
    FCreateKeySet := GetCreateKeySet;
    FCreateValueCollection := GetCreateValueCollection;
    FMacroFooter := GetMacroFooter;
    FOwnershipAssignments := GetOwnershipAssignment;
  end;
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
