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
{ The Original Code is JclContainer1DTemplates.pas.                                                }
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

unit JclPreProcessorContainer1DTemplates;

interface

{$I jcl.inc}

uses
  {$IFDEF HAS_UNITSCOPE}
  System.Classes,
  {$ELSE ~HAS_UNITSCOPE}
  Classes,
  {$ENDIF ~HAS_UNITSCOPE}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase,
  JclPreProcessorContainerTypes;

{$TYPEINFO ON}

type
  TJclContainerTypeInfo = class
  private
    FCustomTypeAttributes: TTypeAttributes;
    FKnownTypeAttributes: PKnownTypeAttributes;
    FOnKnownTypeChange: TNotifyEvent;
    function GetCustomTypeAttribute(Index: TTypeAttributeID): string;
    function GetTypeName: string;
  protected
    function GetFloatType: Boolean;
    function GetKnownType: Boolean;
    function GetOwnershipDeclaration: string;
    function GetStringType: Boolean;
    function GetTObjectType: Boolean;
    function GetTypeAttribute(Index: TTypeAttributeID): string;
    procedure SetKnownType(Value: Boolean);
    procedure SetTypeAttribute(Index: TTypeAttributeID; const Value: string);
    procedure SetTypeName(const Value: string);
  public
    property FloatType: Boolean read GetFloatType;
    property KnownType: Boolean read GetKnownType write SetKnownType;
    property StringType: Boolean read GetStringType;
    property TObjectType: Boolean read GetTObjectType;
    property KnownTypeAttributes: PKnownTypeAttributes read FKnownTypeAttributes;
    property CustomTypeAttributes[Index: TTypeAttributeID]: string read GetCustomTypeAttribute;
    property TypeAttributes[Index: TTypeAttributeID]: string read GetTypeAttribute write SetTypeAttribute;
    property TypeName: string read GetTypeName write SetTypeName stored True;
    property OwnershipDeclaration: string read GetOwnershipDeclaration;
    property OnKnownTypeChange: TNotifyEvent read FOnKnownTypeChange write FOnKnownTypeChange;
  end;

  TJclContainerInterfaceParams = class(TJclInterfaceParams)
  private
    FTypeInfo: TJclContainerTypeInfo;
  protected
    function GetOwnershipDeclaration: string; virtual;
    function GetTypeAttribute(Index: TTypeAttributeID): string;
    function IsTypeAttributeStored(Index: TTypeAttributeID): Boolean;
    procedure SetTypeAttribute(Index: TTypeAttributeID; const Value: string);
  public
    property TypeInfo: TJclContainerTypeInfo read FTypeInfo write FTypeInfo;
    property OwnershipDeclaration: string read GetOwnershipDeclaration;
  end;

  TJclClassInterfaceParams = class(TJclContainerInterfaceParams)
  protected
    FAncestorClassName: string;
    FInterfaceAdditional: string;
    FSectionAdditional: string;
    function GetAncestorClassName: string; virtual;
    function GetInterfaceAdditional: string; virtual;
    function GetSectionAdditional: string; virtual;
  public
    property AncestorClassName: string read GetAncestorClassName write FAncestorClassName;
    property InterfaceAdditional: string read GetInterfaceAdditional write FInterfaceAdditional;
    property SectionAdditional: string read GetSectionAdditional write FSectionAdditional;
  end;

  TJclCollectionInterfaceParams = class(TJclClassInterfaceParams)
  protected
    FCollectionFlags: string;
    function GetAncestorClassName: string; override;
    function GetCollectionFlags: string; virtual;
  public
    property CollectionFlags: string read GetCollectionFlags write FCollectionFlags;
  end;

  TJclTypeParams = class(TJclContainerInterfaceParams)
  published
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property Condition: string index taCondition read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property Defines: string index taDefines read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property Undefs: string index taUndefs read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property Alias: string index taAlias read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property AliasCondition: string index taAliasCondition read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property DefaultValue: string index taDefaultValue read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property OwnershipParameterName: string index taOwnershipParameterName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ReleaserFunctionName: string index taReleaserFunctionName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property GetterFunctionName: string index taGetterFunctionName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property SetterProcedureName: string index taSetterProcedureName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property DynArrayTypeName: string index taDynArrayTypeName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ArrayPropertyName: string index taArrayPropertyName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property BaseContainerClassName: string index taBaseContainerClassName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property BaseCollectionClassName: string index taBaseCollectionClassName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ContainerInterfaceName: string index taContainerInterfaceName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ContainerInterfaceGUID: string index taContainerInterfaceGUID read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property FlatContainerInterfaceName: string index taFlatContainerInterfaceName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property FlatContainerInterfaceGUID: string index taFlatContainerInterfaceGUID read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
  end;

  TJclContainerImplementationParams = class(TJclImplementationParams)
  private
    function GetTypeInfo: TJclContainerTypeInfo;
  protected
    function GetOwnershipDeclaration: string; virtual;
    function GetTypeAttribute(Index: TTypeAttributeID): string;
    function IsTypeAttributeStored(Index: TTypeAttributeID): Boolean;
    procedure SetTypeAttribute(Index: TTypeAttributeID; const Value: string);
  public
    property OwnershipDeclaration: string read GetOwnershipDeclaration;
    property TypeInfo: TJclContainerTypeInfo read GetTypeInfo;
  end;

  TJclClassImplementationParams = class(TJclContainerImplementationParams)
  protected
    FMacroFooter: string;
    function GetConstructorParameters: string; virtual; abstract;
    function GetSelfClassName: string; virtual; abstract;
  public
    function GetMacroFooter: string; override;
    procedure ResetDefault(Value: Boolean); override;
    property MacroFooter: string read GetMacroFooter write FMacroFooter;
  end;

  TJclCollectionImplementationParams = class(TJclClassImplementationParams)

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
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF HAS_UNIT_RTLCONSTS}
  System.RTLConsts,
  {$ENDIF HAS_UNIT_RTLCONTST}
  System.TypInfo,
  System.SysUtils,
  Winapi.ActiveX,
  System.Win.ComObj,
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF HAS_UNIT_RTLCONSTS}
  RTLConsts,
  {$ENDIF HAS_UNIT_RTLCONTST}
  TypInfo,
  SysUtils,
  ActiveX,
  ComObj,
  {$ENDIF ~HAS_UNITSCOPE}
  JclRTTI,
  JclSysUtils,
  JclContainerIntf,
  JclPreProcessorContainerKnownTypes,
  JclPreProcessorContainerTemplates;

procedure RegisterJclContainers;
begin
  RegisterContainerParams('', TJclTypeParams);
end;

//=== { TJclContainerTypeInfo } ==============================================

function TJclContainerTypeInfo.GetTypeAttribute(Index: TTypeAttributeID): string;
begin
  if (Index >= Low(TTypeAttributeID)) and (Index <= High(TTypeAttributeID)) then
  begin
    if FCustomTypeAttributes[Index] = '' then
    begin
      if Assigned(FKnownTypeAttributes) then
        Result := FKnownTypeAttributes^[Index]
      else
        Result := Format(TypeAttributeInfos[Index].DefaultValue, [TypeName]);
    end
    else
      Result := FCustomTypeAttributes[Index];
  end
  else
  begin
    System.Error(reRangeError);
    Result := '';
  end;
end;

function TJclContainerTypeInfo.GetTypeName: string;
begin
  Result := FCustomTypeAttributes[taTypeName];
end;

function TJclContainerTypeInfo.GetCustomTypeAttribute(Index: TTypeAttributeID): string;
begin
  if (Index >= Low(TTypeAttributeID)) and (Index <= High(TTypeAttributeID)) then
    Result := FCustomTypeAttributes[Index]
  else
  begin
    System.Error(reRangeError);
    Result := '';
  end;
end;

function TJclContainerTypeInfo.GetFloatType: Boolean;
begin
  Result := KnownType;
  if Result then
    Result := (TypeName = SingleKnownType[taTypeName]) or
              (TypeName = DoubleKnownType[taTypeName]) or
              (TypeName = ExtendedKnownType[taTypeName]) or
              (TypeName = FloatKnownType[taTypeName]);
end;

function TJclContainerTypeInfo.GetKnownType: Boolean;
var
  Index: TTypeAttributeID;
begin
  Result := Assigned(FKnownTypeAttributes);
  for Index := Low(TTypeAttributeID) to High(TTypeAttributeID) do
    if Index <> taTypeName then
  begin
    Result := Result and (FCustomTypeAttributes[Index] = '');
    if not Result then
      Break;
  end;
end;

function TJclContainerTypeInfo.GetOwnershipDeclaration: string;
begin
  Result := GetTypeAttribute(taOwnershipParameterName);
  if Result <> '' then
    Result := Format('%s: Boolean', [Result]);
end;

function TJclContainerTypeInfo.GetTObjectType: Boolean;
begin
  Result := KnownType and (TypeName = TObjectKnownType[taTypeName]);
end;

function TJclContainerTypeInfo.GetStringType: Boolean;
begin
  Result := KnownType;
  if Result then
    Result := (TypeName = AnsiStringKnownType[taTypeName]) or
              (TypeName = WideStringKnownType[taTypeName]) or
              (TypeName = UnicodeStringKnownType[taTypeName]) or
              (TypeName = StringKnownType[taTypeName]);
end;

procedure TJclContainerTypeInfo.SetKnownType(Value: Boolean);
var
  Index: TTypeAttributeID;
  NewGUID: TGUID;
begin
  if Value then
  begin
    // reset to default values
    for Index := Low(TTypeAttributeID) to High(TTypeAttributeID) do
      if Index <> taTypeName then
        FCustomTypeAttributes[Index] := '';
  end
  else
  if {not Value and} Assigned(FKnownTypeAttributes) then
  begin
    // copy with new GUIDs
    for Index := Low(TTypeAttributeID) to High(TTypeAttributeID) do
      if Index <> taTypeName then
    begin
      if TypeAttributeInfos[Index].IsGUID then
      begin
        OleCheck(CoCreateGuid(NewGUID));
        FCustomTypeAttributes[Index] := GUIDToString(NewGUID);
      end
      else
        FCustomTypeAttributes[Index] := FKnownTypeAttributes[Index];
    end;
  end
  else
  begin
    {not Value and not Assigned(FKnownTypeAttributes)}
    // default names with new GUIDs
    for Index := Low(TTypeAttributeID) to High(TTypeAttributeID) do
      if Index <> taTypeName then
    begin
      if TypeAttributeInfos[Index].IsGUID then
      begin
        OleCheck(CoCreateGuid(NewGUID));
        FCustomTypeAttributes[Index] := GUIDToString(NewGUID);
      end
      else
        FCustomTypeAttributes[Index] := Format(TypeAttributeInfos[Index].DefaultValue, [TypeName]);
    end;
  end;
  if Assigned(FOnKnownTypeChange) then
    FOnKnownTypeChange(Self);
end;

procedure TJclContainerTypeInfo.SetTypeAttribute(Index: TTypeAttributeID;
  const Value: string);
begin
  if (Index >= Low(TTypeAttributeID)) and (Index <= High(TTypeAttributeID)) then
    FCustomTypeAttributes[Index] := Value
  else
    System.Error(reRangeError);
end;

procedure TJclContainerTypeInfo.SetTypeName(const Value: string);
begin
  FCustomTypeAttributes[taTypeName] := Value;
  FKnownTypeAttributes := IsKnownType(Value);
  if Assigned(FKnownTypeAttributes) then
    SetKnownType(True);
end;

//=== { TJclContainerInterfaceParams } =======================================

function TJclContainerInterfaceParams.GetOwnershipDeclaration: string;
begin
  Result := TypeInfo.OwnershipDeclaration;
  if Result <> '' then
    Result := '; ' + Result;
end;

function TJclContainerInterfaceParams.GetTypeAttribute(Index: TTypeAttributeID): string;
begin
  Result := TypeInfo.TypeAttributes[Index];
end;

function TJclContainerInterfaceParams.IsTypeAttributeStored(Index: TTypeAttributeID): Boolean;
begin
  Result := TypeInfo.CustomTypeAttributes[Index] <> '';
end;

procedure TJclContainerInterfaceParams.SetTypeAttribute(Index: TTypeAttributeID;
  const Value: string);
begin
  TypeInfo.TypeAttributes[Index] := Value;
end;

//=== { TJclClassInterfaceParams } ===========================================

function TJclClassInterfaceParams.GetAncestorClassName: string;
begin
  Result := FAncestorClassName;
  if Result = '' then
    Result := TypeInfo.TypeAttributes[taBaseContainerClassName];
end;

function TJclClassInterfaceParams.GetInterfaceAdditional: string;
begin
  Result := FInterfaceAdditional;
  if Result = '' then
  begin
    if TypeInfo.StringType then
      Result := ' IJclStrBaseContainer,'
    else
    if TypeInfo.TObjectType then
      Result := ' IJclObjectOwner,';
  end;
end;

function TJclClassInterfaceParams.GetSectionAdditional: string;
begin
  Result := FSectionAdditional;
  if (Result = '') and TypeInfo.KnownType then
    Result := NativeLineBreak +
              'protected' + NativeLineBreak +
              '  function CreateEmptyContainer: TJclAbstractContainerBase; override;';
end;

//=== { TJclCollectionInterfaceParams } ======================================

function TJclCollectionInterfaceParams.GetAncestorClassName: string;
begin
  Result := FAncestorClassName;
  if Result = '' then
  begin
    if TypeInfo.TypeAttributes[taBaseCollectionClassName] <> '' then
      Result := TypeInfo.TypeAttributes[taBaseCollectionClassName]
    else
      Result := inherited GetAncestorClassName;
  end;
end;

function TJclCollectionInterfaceParams.GetCollectionFlags: string;
begin
  Result := FCollectionFlags;
  if (Result = '') and (TypeInfo.TypeAttributes[taBaseCollectionClassName] <> '') then
    Result := ' override;';
end;

//=== { TJclContainerImplementationParams } =======================================

function TJclContainerImplementationParams.GetOwnershipDeclaration: string;
begin
  Result := (InterfaceParams as TJclContainerInterfaceParams).OwnershipDeclaration;
end;

function TJclContainerImplementationParams.GetTypeAttribute(Index: TTypeAttributeID): string;
begin
  Result := (InterfaceParams as TJclContainerInterfaceParams).GetTypeAttribute(Index);
end;

function TJclContainerImplementationParams.GetTypeInfo: TJclContainerTypeInfo;
begin
  Result := (InterfaceParams as TJclContainerInterfaceParams).TypeInfo;
end;

function TJclContainerImplementationParams.IsTypeAttributeStored(Index: TTypeAttributeID): Boolean;
begin
  Result := (InterfaceParams as TJclContainerInterfaceParams).IsTypeAttributeStored(Index);
end;

procedure TJclContainerImplementationParams.SetTypeAttribute(Index: TTypeAttributeID;
  const Value: string);
begin
  (InterfaceParams as TJclContainerInterfaceParams).SetTypeAttribute(Index, Value);
end;

//=== { TJclClassImplementationParams } ======================================

function TJclClassImplementationParams.GetMacroFooter: string;
var
  Ownership, SelfClassName, ConstructorParameters: string;
begin
  if GetTypeAttribute(taOwnershipParameterName) <> '' then
    Ownership := 'False'
  else
    Ownership := '';

  SelfClassName := GetSelfClassName;
  ConstructorParameters := GetConstructorParameters;

  Result := FMacroFooter;

  if (Result = '') and TypeInfo.KnownType then
  begin
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
  end;
end;

procedure TJclClassImplementationParams.ResetDefault(Value: Boolean);
begin
  inherited ResetDefault(Value);
  FMacroFooter := '';
  if not Value then
    FMacroFooter := GetMacroFooter;
end;

initialization
  RegisterJclContainers;
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
