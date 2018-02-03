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
{ The Original Code is JclHashMapsTemplates.pas.                                                   }
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

unit JclPreProcessorHashMapsTemplates;

interface

{$I jcl.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclPreProcessorContainerTypes,
  JclPreProcessorContainerTemplates,
  JclPreProcessorContainer2DTemplates;

type
  (* JCLHASHMAPTYPESINT(ENTRYTYPENAME, ENTRYARRAYTYPENAME, BUCKETTYPENAME, KEYTYPENAME, VALUETYPENAME) *)
  TJclHashMapTypeIntParams = class(TJclMapInterfaceParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property EntryTypeName: string index maHashMapEntryTypeName read GetMapAttribute write SetMapAttribute stored IsMapAttributeStored;
    property EntryArrayTypeName: string index maHashMapEntryArrayTypeName read GetMapAttribute write SetMapAttribute stored IsMapAttributeStored;
    property BucketTypeName: string index maHashMapBucketTypeName read GetMapAttribute write SetMapAttribute stored IsMapAttributeStored;
    property KeyTypeName: string index kaKeyTypeName read GetKeyAttribute write SetKeyAttribute stored False;
    property ValueTypeName: string index vaValueTypeName read GetValueAttribute write SetValueAttribute stored False;
  end;

  (* JCLHASHMAPINT(BUCKETTYPENAME, SELFCLASSNAME, ANCESTORNAME, MAPINTERFACENAME, KEYSETINTERFACENAME,
                   KEYCOLLECTIONINTERFACENAME,
                   VALUECOLLECTIONINTERFACENAME, INTERFACEADDITIONAL, SECTIONADDITIONAL,
                   KEYOWNERSHIPDECLARATION, VALUEOWNERSHIPDECLARATION, KEYCONSTKEYWORD,
                   KEYTYPENAME, VALUECONSTKEYWORD, VALUETYPENAME) *)
  TJclHashMapIntParams = class(TJclMapClassInterfaceParams)
  protected
    // function CodeUnit: string; override;
    function GetComparisonSectionAdditional: string; override;
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property BucketTypeName: string index maHashMapBucketTypeName read GetMapAttribute write SetMapAttribute stored False;
    property SelfClassName: string index maHashMapClassName read GetMapAttribute write SetMapAttribute stored IsMapAttributeStored;
    property AncestorName: string index maMapAncestorClassName read GetMapAttribute write SetMapAttribute stored False;
    property MapInterfaceName: string index maMapInterfaceName read GetMapAttribute write SetMapAttribute stored False;
    property KeySetInterfaceName: string index kaKeySetInterfaceName read GetKeyAttribute write SetKeyAttribute stored False;
    property KeyCollectionInterfaceName: string index kaKeyCollectionInterfaceName read GetKeyAttribute write SetKeyAttribute stored False;
    property ValueCollectionInterfaceName: string index vaValueCollectionInterfaceName read GetValueAttribute write SetValueAttribute stored False;
    property InterfaceAdditional;
    property SectionAdditional;
    property KeyOwnershipDeclaration;
    property ValueOwnershipDeclaration;
    property KeyConstKeyword: string index kaKeyConstKeyword read GetKeyAttribute write SetKeyAttribute stored False;
    property KeyTypeName;
    property ValueConstKeyword: string index vaValueConstKeyword read GetValueAttribute write SetValueAttribute stored False;
    property ValueTypeName;
  end;

  (* JCLHASHMAPTYPESIMP(ENTRYARRAYTYPENAME, BUCKETTYPENAME, KEYDEFAULT, VALUEDEFAULT) *)
  TJclHashMapTypeImpParams = class(TJclMapImplementationParams)
  published
    property EntryArrayTypeName: string index maHashMapEntryArrayTypeName read GetMapAttribute write SetMapAttribute stored False;
    property BucketTypeName: string index maHashMapBucketTypeName read GetMapAttribute write SetMapAttribute stored False;
    property KeyDefault: string index kaKeyDefaultValue read GetKeyAttribute write SetKeyAttribute stored False;
    property ValueDefault: string index vaValueDefaultValue read GetValueAttribute write SetValueAttribute stored False;
  end;

  (* JCLHASHMAPIMP(SELFCLASSNAME, BUCKETTYPENAME,
                   MAPINTERFACENAME, KEYSETINTERFACENAME, KEYCOLLECTIONINTERFACENAME, KEYITRINTERFACENAME, VALUECOLLECTIONINTERFACENAME,
                   KEYOWNERSHIPDECLARATION, VALUEOWNERSHIPDECLARATION, OWNERSHIPASSIGNMENTS,
                   KEYCONSTKEYWORD, KEYTYPENAME, KEYDEFAULT, VALUECONSTKEYWORD, VALUETYPENAME, VALUEDEFAULT,
                   CREATEKEYSET, CREATEKEYCOLLECTION, CREATEVALUECOLLECTION) *)
  TJclHashMapImpParams = class(TJclMapClassImplementationParams)
  protected
    // function CodeUnit: string; override;
  public
    function GetConstructorParameters: string; override;
    function GetMacroFooter: string; override;
    function GetSelfClassName: string; override;
  published
    property SelfClassName: string index maHashMapClassName read GetMapAttribute write SetMapAttribute stored False;
    property AncestorClassName: string index maMapAncestorClassName read GetMapAttribute write SetMapAttribute stored False;
    property BucketTypeName: string index maHashMapBucketTypeName read GetMapAttribute write SetMapAttribute stored False;
    property MapInterfaceName: string index maMapInterfaceName read GetMapAttribute write SetMapAttribute stored False;
    property KeySetInterfaceName: string index kaKeySetInterfaceName read GetKeyAttribute write SetKeyAttribute stored False;
    property KeyCollectionInterfaceName: string index kaKeyCollectionInterfaceName read GetKeyAttribute write SetKeyAttribute stored False;
    property KeyArraySetClassName;
    property KeyItrInterfaceName: string index kaKeyIteratorInterfaceName read GetKeyAttribute write SetKeyAttribute stored False;
    property ValueCollectionInterfaceName: string index vaValueCollectionInterfaceName read GetValueAttribute write SetValueAttribute stored False;
    property ValueArrayListClassName;
    property KeyOwnershipDeclaration;
    property ValueOwnershipDeclaration;
    property OwnershipAssignments;
    property KeyConstKeyword: string index kaKeyConstKeyword read GetKeyAttribute write SetKeyAttribute stored False;
    property KeyParameterName: string index kaKeyParameterName read GetKeyAttribute write SetKeyAttribute stored False;
    property KeyTypeName;
    property KeyDefault;
    property KeySimpleEqualityCompareFunctionName: string index kaKeySimpleEqualityCompareFunctionName read GetKeyAttribute write SetKeyAttribute stored False;
    property KeySimpleHashConvertFunctionName: string index kaKeySimpleHashConvertFunctionName read GetKeyAttribute write SetKeyAttribute stored False;
    property KeyBaseContainer: string index kaKeyBaseContainerClassName read GetKeyAttribute write SetKeyAttribute stored False;
    property ValueConstKeyword: string index vaValueConstKeyword read GetValueAttribute write SetValueAttribute stored False;
    property ValueTypeName;
    property ValueDefault;
    property ValueSimpleEqualityCompareFunctionName: string index vaValueSimpleEqualityCompareFunctionName read GetValueAttribute write SetValueAttribute stored False;
    property ValueBaseContainerClassName: string index vaValueBaseContainerClassName read GetValueAttribute write SetValueAttribute stored False;
    property CreateKeySet;
    property CreateKeyCollection;
    property CreateValueCollection;
    property MacroFooter;
  end;

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
  System.SysUtils,
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils,
  {$ENDIF ~HAS_UNITSCOPE}
  JclStrings;

procedure RegisterJclContainers;
begin
  RegisterContainerParams('JCLHASHMAPTYPESINT', TJclHashMapTypeIntParams);
  RegisterContainerParams('JCLHASHMAPTYPESIMP', TJclHashMapTypeImpParams, TJclHashMapTypeIntParams);
  RegisterContainerParams('JCLHASHMAPINT', TJclHashMapIntParams);
  RegisterContainerParams('JCLHASHMAPIMP', TJclHashMapImpParams, TJclHashMapIntParams);
end;

//=== { TJclHashMapTypeIntParams } ===========================================

function TJclHashMapTypeIntParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [maHashMapEntryTypeName, maHashMapBucketTypeName];
end;

//=== { TJclHashMapIntParams } ===============================================

function TJclHashMapIntParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [maHashMapClassName];
end;

function TJclHashMapIntParams.GetComparisonSectionAdditional: string;
begin
  Result := '';
  if AncestorName <> MapInfo.KeyTypeInfo.TypeAttributes[taBaseContainerClassName] then
  begin
    Result := Format('%s  function Hash(%s%s: %s): Integer;',
                     [Result, MapInfo.KeyTypeInfo.TypeAttributes[taConstKeyword],
                      MapInfo.KeyTypeInfo.TypeAttributes[taParameterName], KeyTypeName]);
    if AncestorName <> 'TJclAbstractContainerBase' then
      Result := Result + ' reintroduce;' + NativeLineBreak
    else
      Result := Result + NativeLineBreak;
  end;
  Result := Format('%s  function KeysEqual(%sA, B: %s): Boolean;' + NativeLineBreak +
                   '  function ValuesEqual(%sA, B: %s): Boolean;',
                   [Result, KeyConstKeyword, KeyTypeName, ValueConstKeyword, ValueTypeName]);
end;

//=== { TJclHashMapImpParams } ===============================================

function TJclHashMapImpParams.GetConstructorParameters: string;
begin
  Result := 'FCapacity';
end;

function TJclHashMapImpParams.GetMacroFooter: string;
var
  FuncName: string;
begin
  Result := inherited GetMacroFooter;
  if (FMacroFooter = '') and MapInfo.KnownMap then
  begin
    if AncestorClassName <> KeyBaseContainer then
    begin
      Result := Format('%s' + NativeLineBreak +
                       'function %s.Hash(%s%s: %s): Integer;' + NativeLineBreak +
                       'begin' + NativeLineBreak +
                       '  Result := %s(%s);' + NativeLineBreak +
                       'end;' + NativeLineBreak,
                       [Result, SelfClassName, KeyConstKeyword, KeyParameterName, KeyTypeName,
                        KeySimpleHashConvertFunctionName, KeyParameterName]);
    end;

    if AncestorClassName = KeyBaseContainer then
      FuncName := 'ItemsEqual'
    else
      FuncName := KeySimpleEqualityCompareFunctionName;

    Result := Format('%s' + NativeLineBreak +
                     'function %s.KeysEqual(%sA, B: %s): Boolean;' + NativeLineBreak +
                     'begin' + NativeLineBreak +
                     '  Result := %s(A, B);' + NativeLineBreak +
                     'end;' + NativeLineBreak,
                     [Result, SelfClassName, KeyConstKeyword, KeyTypeName, FuncName]);

    if AncestorClassName = ValueBaseContainerClassName then
      FuncName := 'ItemsEqual'
    else
      FuncName := ValueSimpleEqualityCompareFunctionName;

    Result := Format('%s' + NativeLineBreak +
                     'function %s.ValuesEqual(%sA, B: %s): Boolean;' + NativeLineBreak +
                     'begin' + NativeLineBreak +
                     '  Result := %s(A, B);' + NativeLineBreak +
                     'end;' + NativeLineBreak,
                     [Result, SelfClassName, ValueConstKeyword, ValueTypeName, FuncName]);
  end;
end;

function TJclHashMapImpParams.GetSelfClassName: string;
begin
  Result := SelfClassName;
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

