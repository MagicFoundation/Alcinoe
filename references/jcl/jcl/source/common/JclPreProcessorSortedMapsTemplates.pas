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
{ The Original Code is JclSortedMapsTemplates.pas.                                                 }
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

unit JclPreProcessorSortedMapsTemplates;

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
  (* JCLSORTEDMAPTYPESINT(ENTRYTYPENAME, ENTRYARRAYTYPENAME, KEYTYPENAME, VALUETYPENAME) *)
  TJclSortedMapTypeIntParams = class(TJclMapInterfaceParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property EntryTypeName: string index maSortedMapEntryTypeName read GetMapAttribute write SetMapAttribute stored IsMapAttributeStored;
    property EntryArrayTypeName: string index maSortedMapEntryArrayTypeName read GetMapAttribute write SetMapAttribute stored IsMapAttributeStored;
    property KeyTypeName: string index kaKeyTypeName read GetKeyAttribute write SetKeyAttribute stored False;
    property ValueTypeName: string index vaValueTypeName read GetValueAttribute write SetValueAttribute stored False;
  end;

  (* JCLSORTEDMAPINT(ENTRYTYPENAME, ENTRYARRAYTYPENAME, SELFCLASSNAME, ANCESTORNAME,
                     STDMAPINTERFACENAME, SORTEDMAPINTERFACENAME,
                     KEYSETINTERFACENAME, VALUECOLLECTIONINTERFACENAME,
                     INTERFACEADDITIONAL, SECTIONADDITIONAL, KEYOWNERSHIPDECLARATION,
                     VALUEOWNERSHIPDECLARATION, KEYCONSTKEYWORD, KEYTYPENAME,
                     VALUECONSTKEYWORD, VALUETYPENAME) *)
  TJclSortedMapIntParams = class(TJclMapClassInterfaceParams)
  protected
    // function CodeUnit: string; override;
    //function GetInterfaceAdditional: string; override;
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
    function GetComparisonSectionAdditional: string; override;
  published
    property EntryTypeName: string index maSortedMapEntryTypeName read GetMapAttribute write SetMapAttribute stored False;
    property EntryArrayTypeName: string index maSortedMapEntryArrayTypeName read GetMapAttribute write SetMapAttribute stored False;
    property SelfClassName: string index maSortedMapClassName read GetMapAttribute write SetMapAttribute stored IsMapAttributeStored;
    property AncestorName: string index maMapAncestorClassName read GetMapAttribute write SetMapAttribute stored False;
    property StdMapInterfaceName: string index maMapInterfaceName read GetMapAttribute write SetMapAttribute stored False;
    property SortedMapInterfaceName: string index maSortedMapInterfaceName read GetMapAttribute write SetMapAttribute stored False;
    property KeySetInterfaceName: string index kaKeySetInterfaceName read GetKeyAttribute write SetKeyAttribute stored False;
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

  (* JCLSORTEDMAPIMP(ENTRYTYPENAME, ENTRYARRAYTYPENAME, SELFCLASSNAME,
                     STDMAPINTERFACENAME, SORTEDMAPINTERFACENAME,
                     KEYSETINTERFACENAME, KEYITRINTERFACENAME,
                     VALUECOLLECTIONINTERFACENAME,
                     KEYOWNERSHIPDECLARATION, VALUEOWNERSHIPDECLARATION,
                     OWNERSHIPASSIGNMENTS, KEYCONSTKEYWORD, KEYTYPENAME, KEYDEFAULT,
                     VALUECONSTKEYWORD, VALUETYPENAME, VALUEDEFAULT,
                     CREATEKEYSET, CREATEVALUECOLLECTION) *)
  TJclSortedMapImpParams = class(TJclMapClassImplementationParams)
  protected
    // function CodeUnit: string; override;
  public
    function GetConstructorParameters: string; override;
    function GetMacroFooter: string; override;
    function GetSelfClassName: string; override;
  published
    property EntryTypeName: string index maSortedMapEntryTypeName read GetMapAttribute write SetMapAttribute stored False;
    property EntryArrayTypeName: string index maSortedMapEntryArrayTypeName read GetMapAttribute write SetMapAttribute stored False;
    property SelfClassName: string index maSortedMapClassName read GetMapAttribute write SetMapAttribute stored False;
    property AncestorClassName: string index maMapAncestorClassName read GetMapAttribute write SetMapAttribute stored False;
    property StdMapInterfaceName: string index maMapInterfaceName read GetMapAttribute write SetMapAttribute stored False;
    property SortedMapInterfaceName: string index maSortedMapInterfaceName read GetMapAttribute write SetMapAttribute stored False;
    property KeySetInterfaceName: string index kaKeySetInterfaceName read GetKeyAttribute write SetKeyAttribute stored False;
    property KeyItrInterfaceName: string index kaKeyIteratorInterfaceName read GetKeyAttribute write SetKeyAttribute stored False;
    property ValueCollectionInterfaceName: string index vaValueCollectionInterfaceName read GetValueAttribute write SetValueAttribute stored False;
    property KeyOwnershipDeclaration;
    property ValueOwnershipDeclaration;
    property OwnershipAssignments;
    property KeyConstKeyword: string index kaKeyConstKeyword read GetKeyAttribute write SetKeyAttribute stored False;
    property KeyTypeName;
    property KeyDefault;
    property KeySimpleCompareFunctionName: string index kaKeySimpleCompareFunctionName read GetKeyAttribute write SetKeyAttribute stored False;
    property KeyBaseContainer: string index kaKeyBaseContainerClassName read GetKeyAttribute write SetKeyAttribute stored False;
    property ValueConstKeyword: string index vaValueConstKeyword read GetValueAttribute write SetValueAttribute stored False;
    property ValueTypeName;
    property ValueDefault;
    property ValueSimpleCompareFunctionName: string index vaValueSimpleCompareFunctionName read GetValueAttribute write SetValueAttribute stored False;
    property ValueBaseContainerClassName: string index vaValueBaseContainerClassName read GetValueAttribute write SetValueAttribute stored False;
    property CreateKeySet;
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
  RegisterContainerParams('JCLSORTEDMAPTYPESINT', TJclSortedMapTypeIntParams);
  RegisterContainerParams('JCLSORTEDMAPINT', TJclSortedMapIntParams);
  RegisterContainerParams('JCLSORTEDMAPIMP', TJclSortedMapImpParams, TJclSortedMapIntParams);
end;

//=== { TJclSortedMapTypeIntParams } =========================================

function TJclSortedMapTypeIntParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [maSortedMapEntryTypeName];
end;

//=== { TJclSortedMapIntParams } =============================================

function TJclSortedMapIntParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [maSortedMapClassName];
end;

function TJclSortedMapIntParams.GetComparisonSectionAdditional: string;
begin
  Result := Format('  function KeysCompare(%sA, B: %s): Integer;' + NativeLineBreak +
                   '  function ValuesCompare(%sA, B: %s): Integer;',
                   [KeyConstKeyword, KeyTypeName, ValueConstKeyword, ValueTypeName]);
end;

//=== { TJclSortedMapImpParams } =============================================

function TJclSortedMapImpParams.GetConstructorParameters: string;
begin
  Result := 'FSize';
end;

function TJclSortedMapImpParams.GetMacroFooter: string;
var
  FuncName: string;
begin
  Result := inherited GetMacroFooter;
  if (FMacroFooter = '') and MapInfo.KnownMap then
  begin
    if AncestorClassName = KeyBaseContainer then
      FuncName := 'ItemsCompare'
    else
      FuncName := KeySimpleCompareFunctionName;

    Result := Format('%s' + NativeLineBreak +
                     'function %s.KeysCompare(%sA, B: %s): Integer;' + NativeLineBreak +
                     'begin' + NativeLineBreak +
                     '  Result := %s(A, B);' + NativeLineBreak +
                     'end;' + NativeLineBreak,
                     [Result, SelfClassName, KeyConstKeyword, KeyTypeName, FuncName]);

    if AncestorClassName = ValueBaseContainerClassName then
      FuncName := 'ItemsCompare'
    else
      FuncName := ValueSimpleCompareFunctionName;

    Result := Format('%s' + NativeLineBreak +
                     'function %s.ValuesCompare(%sA, B: %s): Integer;' + NativeLineBreak +
                     'begin' + NativeLineBreak +
                     '  Result := %s(A, B);' + NativeLineBreak +
                     'end;' + NativeLineBreak,
                     [Result, SelfClassName, ValueConstKeyword, ValueTypeName, FuncName]);
  end;
end;

function TJclSortedMapImpParams.GetSelfClassName: string;
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

