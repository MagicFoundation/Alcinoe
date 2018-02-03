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
{ The Original Code is JclContainerIntfTemplates.pas.                                              }
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

unit JclPreProcessorContainerIntfTemplates;

interface

{$I jcl.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclPreProcessorContainerTypes,
  JclPreProcessorContainerTemplates,
  JclPreProcessorContainer1DTemplates,
  JclPreProcessorContainer2DTemplates;

type
  TJclContainerIntf1DParams = class(TJclContainerInterfaceParams)
  protected
    // function CodeUnit: string; override;
  end;

  TJclContainerIntf2DParams = class(TJclMapInterfaceParams)
  protected
    // function CodeUnit: string; override;
  end;

  TJclContainerIntfAncestorParams = class(TJclContainerIntf1DParams)
  protected
    FAncestorName: string;
    function GetAncestorName: string; virtual;
    function IsAncestorNameStored: Boolean;
  public
    property AncestorName: string read GetAncestorName write FAncestorName stored IsAncestorNameStored;
  end;

  TJclContainerIntfFlatAncestorParams = class(TJclContainerIntfAncestorParams)
  protected
    function GetAncestorName: string; override;
  end;

  (* ITERPROCEDURE(ITERATEPROCEDURETYPENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME) *)
  TJclIterProcedureParams = class(TJclContainerIntf1DParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property IterateProcedureTypeName: string index taIterateProcedureTypeName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* APPLYFUNCTION(APPLYFUNCTIONTYPENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME) *)
  TJclApplyFunctionParams = class(TJclContainerIntf1DParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property ApplyFunctionTypeName: string index taApplyFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* COMPAREFUNCTION(COMPAREFUNCTIONTYPENAME, CONSTKEYWORD, TYPENAME) *)
  TJclCompareFunctionParams = class(TJclContainerIntf1DParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property CompareFunctionTypeName: string index taCompareFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* EQUALITYCOMPAREFUNCTION(EQUALITYCOMPAREFUNCTIONTYPENAME, CONSTKEYWORD, TYPENAME) *)
  TJclEqualityCompareFunctionParams = class(TJclContainerIntf1DParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property EqualityCompareFunctionTypeName: string index taEqualityCompareFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* HASHFUNCTION(HASHCONVERTFUNCTIONTYPENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME) *)
  TJclHashFunctionParams = class(TJclContainerIntf1DParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property HashConvertFunctionTypeName: string index taHashConvertFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* SORTPROC(SORTPROCEDURETYPENAME, LISTINTERFACENAME, COMPAREFUNCTIONTYPENAME) *)
  TJclSortFunctionParams = class(TJclContainerIntf1DParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property SortProcedureTypeName: string index taSortProcedureTypeName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ListInterfaceName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property CompareFunctionTypeName: string index taCompareFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* EQUALITYCOMPARER(INTERFACENAME, GUID, EQUALITYCOMPAREFUNCTIONTYPENAME, CONSTKEYWORD, TYPENAME) *)
  TJclEqualityComparerParams = class(TJclContainerIntf1DParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property InterfaceName: string index taEqualityComparerInterfaceName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property GUID: string index taEqualityComparerInterfaceGUID read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property EqualityCompareFunctionTypeName: string index taEqualityCompareFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* COMPARER(INTERFACENAME, GUID, COMPAREFUNCTIONTYPENAME, CONSTKEYWORD, TYPENAME) *)
  TJclComparerParams = class(TJclContainerIntf1DParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property InterfaceName: string index taComparerInterfaceName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property GUID: string index taComparerInterfaceGUID read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property CompareFunctionTypeName: string index taCompareFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* HASHCONVERTER(INTERFACENAME, GUID, HASHCONVERTFUNCTIONTYPENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME) *)
  TJclHashConverterParams = class(TJclContainerIntf1DParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property InterfaceName: string index taHashConverterInterfaceName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property GUID: string index taHashConverterInterfaceGUID read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property HashConvertFunctionTypeName: string index taHashConvertFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* RELEASEEVENT(EVENTTYPENAME, PARAMETERNAME, TYPENAME) *)
  TJclReleaseEventParams = class(TJclContainerIntf1DParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property EventTypeName: string index taReleaseEventTypeName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* OWNER(INTERFACENAME, ANCESTORNAME, GUID, RELEASERFUNCTIONNAME, RELEASEEVENTNAME, RELEASEEVENTTYPENAME, PARAMETERNAME, TYPENAME, OWNERADDITIONAL) *)
  TJclOwnerParams = class(TJclContainerIntfAncestorParams)
  protected
    FOwnerAdditional: string;
    function GetAncestorName: string; override;
    function GetOwnerAdditional: string;
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property InterfaceName: string index taOwnershipInterfaceName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property AncestorName;
    property GUID: string index taOwnershipInterfaceGUID read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ReleaserFunctionName: string index taReleaserFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property ReleaseEventName: string index taReleaseEventName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ReleaseEventTypeName: string index taReleaseEventTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property OwnerAdditional: string read GetOwnerAdditional write FOwnerAdditional;
  end;

  (* ITERATOR(INTERFACENAME, ANCESTORNAME, GUID, CONSTKEYWORD, PARAMETERNAME, TYPENAME, GETTERFUNCTIONNAME, SETTERPROCEDURENAME) *)
  TJclIteratorParams = class(TJclContainerIntfAncestorParams)
  protected
    function GetAncestorName: string; override;
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property InterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property AncestorName;
    property GUID: string index taIteratorInterfaceGUID read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property GetterFunctionName: string index taGetterFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterProcedureName: string index taSetterProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* TREEITERATOR(INTERFACENAME, ANCESTORNAME, GUID, CONSTKEYWORD, PARAMETERNAME, TYPENAME) *)
  TJclTreeIteratorParams = class(TJclContainerIntf1DParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property InterfaceName: string index taTreeIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property AncestorName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property GUID: string index taTreeIteratorInterfaceGUID read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* BINTREEITERATOR(INTERFACENAME, ANCESTORNAME, GUID, TYPENAME) *)
  TJclBinaryTreeIteratorParams = class(TJclContainerIntf1DParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property InterfaceName: string index taBinaryTreeIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property AncestorName: string index taTreeIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property GUID: string index taBinaryTreeIteratorInterfaceGUID read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* COLLECTION(INTERFACENAME, ANCESTORNAME, GUID, CONSTKEYWORD, PARAMETERNAME, TYPENAME, ITRNAME) *)
  TJclCollectionParams = class(TJclContainerIntfFlatAncestorParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property InterfaceName: string index taCollectionInterfaceName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property AncestorName;
    property GUID: string index taCollectionInterfaceGUID read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* LIST(INTERFACENAME, ANCESTORNAME, GUID, CONSTKEYWORD, PARAMETERNAME, TYPENAME, GETTERFUNCTIONNAME, SETTERPROCEDURENAME, ARRAYPROPERTYNAME) *)
  TJclListParams = class(TJclContainerIntf1DParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property InterfaceName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ListInterfaceName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property AncestorName: string index taCollectionInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property CollectionInterfaceName: string index taCollectionInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property GUID: string index taListInterfaceGUID read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property GetterFunctionName: string index taGetterFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterProcedureName: string index taSetterProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
    property ArrayPropertyName: string index taArrayPropertyName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* ARRAY(INTERFACENAME, ANCESTORNAME, GUID, CONSTKEYWORD, PARAMETERNAME, TYPENAME, GETTERFUNCTIONNAME, SETTERPROCEDURENAME, ARRAYPROPERTYNAME) *)
  TJclArrayParams = class(TJclContainerIntf1DParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property InterfaceName: string index taArrayInterfaceName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property AncestorName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property GUID: string index taArrayInterfaceGUID read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property GetterFunctionName: string index taGetterFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterProcedureName: string index taSetterProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
    property ArrayPropertyName: string index taArrayPropertyName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* SET(INTERFACENAME, ANCESTORNAME, GUID) *)
  TJclSetParams = class(TJclContainerIntf1DParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property InterfaceName: string index taSetInterfaceName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property SetInterfaceName: string index taSetInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property AncestorName: string index taCollectionInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property CollectionInterfaceName: string index taCollectionInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property GUID: string index taSetInterfaceGUID read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
  end;

  (* TREE(INTERFACENAME, ANCESTORNAME, GUID, ITRNAME) *)
  TJclTreeParams = class(TJclContainerIntf1DParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property InterfaceName: string index taTreeInterfaceName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property AncestorName: string index taCollectionInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property GUID: string index taTreeInterfaceGUID read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ItrName: string index taTreeIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* MAP(INTERFACENAME, ANCESTORNAME, GUID, KEYCONSTKEYWORD, KEYTYPENAME, KEYSETNAME,
         VALUECONSTKEYWORD, VALUETYPENAME, VALUECOLLECTIONNAME) *)
  TJclMapParams = class(TJclContainerIntf2DParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property InterfaceName: string index maMapInterfaceName read GetMapAttribute write SetMapAttribute stored IsMapAttributeStored;
    property AncestorName: string index maMapInterfaceAncestorName read GetMapAttribute write SetMapAttribute stored IsMapAttributeStored;
    property GUID: string index maMapInterfaceGUID read GetMapAttribute write SetMapAttribute stored IsMapAttributeStored;
    property ConstKeyword: string index kaKeyConstKeyword read GetKeyAttribute write SetKeyAttribute stored False;
    property TypeName: string index kaKeyTypeName read GetKeyAttribute write SetKeyAttribute stored False;
    property SetName: string index kaKeySetInterfaceName read GetKeyAttribute write SetKeyAttribute stored False;
    property CollectionName: string index vaValueCollectionInterfaceName read GetValueAttribute write SetValueAttribute stored False;
    property KeyConstKeyword: string index kaKeyConstKeyword read GetKeyAttribute write SetKeyAttribute stored False;
    property KeyTypeName: string index kaKeyTypeName read GetKeyAttribute write SetKeyAttribute stored False;
    property KeySetName: string index kaKeySetInterfaceName read GetKeyAttribute write SetKeyAttribute stored False;
    property ValueConstKeyword: string index vaValueConstKeyword read GetValueAttribute write SetValueAttribute stored False;
    property ValueTypeName: string index vaValueTypeName read GetValueAttribute write SetValueAttribute stored False;
    property ValueCollectionName: string index vaValueCollectionInterfaceName read GetValueAttribute write SetValueAttribute stored False;
  end;

  (* QUEUE(INTERFACENAME, ANCESTORNAME, GUID, CONSTKEYWORD, PARAMETERNAME, TYPENAME) *)
  TJclQueueParams = class(TJclContainerIntfAncestorParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property InterfaceName: string index taQueueInterfaceName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property AncestorName;
    property GUID: string index taQueueInterfaceGUID read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* SORTEDMAP(INTERFACENAME, ANCESTORNAME, GUID, KEYCONSTKEYWORD, KEYTYPENAME) *)
  TJclSortedMapParams = class(TJclContainerIntf2DParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property InterfaceName: string index maSortedMapInterfaceName read GetMapAttribute write SetMapAttribute stored IsMapAttributeStored;
    property AncestorName: string index maMapInterfaceName read GetMapAttribute write SetMapAttribute stored False;
    property GUID: string index maSortedMapInterfaceGUID read GetMapAttribute write SetMapAttribute stored IsMapAttributeStored;
    property KeyConstKeyword: string index kaKeyConstKeyword read GetKeyAttribute write SetKeyAttribute stored False;
    property KeyTypeName: string index kaKeyTypeName read GetKeyAttribute write SetKeyAttribute stored False;
  end;

  (* SORTEDSET(INTERFACENAME, ANCESTORNAME, GUID, CONSTKEYWORD, TYPENAME) *)
  TJclSortedSetParams = class(TJclContainerIntf1DParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property InterfaceName: string index taSortedSetInterfaceName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property AncestorName: string index taSetInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property GUID: string index taSortedSetInterfaceGUID read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* STACK(INTERFACENAME, ANCESTORNAME, GUID, CONSTKEYWORD, PARAMETERNAME, TYPENAME) *)
  TJclStackParams = class(TJclContainerIntfAncestorParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property InterfaceName: string index taStackInterfaceName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property AncestorName;
    property GUID: string index taStackInterfaceGUID read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
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
  JclStrings;

procedure RegisterJclContainers;
begin
  RegisterContainerParams('ITERPROCEDURE', TJclIterProcedureParams);
  RegisterContainerParams('APPLYFUNCTION', TJclApplyFunctionParams);
  RegisterContainerParams('COMPAREFUNCTION', TJclCompareFunctionParams);
  RegisterContainerParams('EQUALITYCOMPAREFUNCTION', TJclEqualityCompareFunctionParams);
  RegisterContainerParams('HASHFUNCTION', TJclHashFunctionParams);
  RegisterContainerParams('SORTPROC', TJclSortFunctionParams);
  RegisterContainerParams('EQUALITYCOMPARER', TJclEqualityComparerParams);
  RegisterContainerParams('COMPARER', TJclComparerParams);
  RegisterContainerParams('HASHCONVERTER', TJclHashConverterParams);
  RegisterContainerParams('RELEASEEVENT', TJclReleaseEventParams);
  RegisterContainerParams('OWNER', TJclOwnerParams);
  RegisterContainerParams('ITERATOR', TJclIteratorParams);
  RegisterContainerParams('TREEITERATOR', TJclTreeIteratorParams);
  RegisterContainerParams('BINTREEITERATOR', TJclBinaryTreeIteratorParams);
  RegisterContainerParams('COLLECTION', TJclCollectionParams);
  RegisterContainerParams('LIST', TJclListParams);
  RegisterContainerParams('ARRAY', TJclArrayParams);
  RegisterContainerParams('SET', TJclSetParams);
  RegisterContainerParams('TREE', TJclTreeParams);
  RegisterContainerParams('MAP', TJclMapParams);
  RegisterContainerParams('QUEUE', TJclQueueParams);
  RegisterContainerParams('SORTEDMAP', TJclSortedMapParams);
  RegisterContainerParams('SORTEDSET', TJclSortedSetParams);
  RegisterContainerParams('STACK', TJclStackParams);
end;

//=== { TJclContainerIntfAncestorParams } ====================================

function TJclContainerIntfAncestorParams.GetAncestorName: string;
begin
  Result := FAncestorName;
  if Result = '' then
    Result := TypeInfo.TypeAttributes[taContainerInterfaceName];
  if Result = '' then
    Result := 'IJclBaseContainer';
end;

function TJclContainerIntfAncestorParams.IsAncestorNameStored: Boolean;
begin
  Result := FAncestorName <> '';
end;

//=== { TJclIteratorParams } =================================================

function TJclIteratorParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taIteratorInterfaceName];
end;

function TJclIteratorParams.GetAncestorName: string;
begin
  Result := FAncestorName;
  if Result = '' then
    Result := 'IJclAbstractIterator';
end;

//=== { TJclContainerIntfFlatAncestorParams } ================================

function TJclContainerIntfFlatAncestorParams.GetAncestorName: string;
begin
  Result := FAncestorName;
  if Result = '' then
    Result := TypeInfo.TypeAttributes[taFlatContainerInterfaceName];
  if Result = '' then
    Result := TypeInfo.TypeAttributes[taContainerInterfaceName];
  if Result = '' then
    Result := 'IJclBaseContainer';
end;

//=== { TJclIterProcedureParams } ============================================

function TJclIterProcedureParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taIterateProcedureTypeName];
end;

//=== { TJclApplyFunctionParams } ============================================

function TJclApplyFunctionParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taApplyFunctionTypeName];
end;

//=== { TJclCompareFunctionParams } ==========================================

function TJclCompareFunctionParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taCompareFunctionTypeName];
end;

//=== { TJclEqualityCompareFunctionParams } ==================================

function TJclEqualityCompareFunctionParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taEqualityCompareFunctionTypeName];
end;

//=== { TJclHashFunctionParams } =============================================

function TJclHashFunctionParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taHashConvertFunctionTypeName];
end;

//=== { TJclSortFunctionParams } =============================================

function TJclSortFunctionParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taSortProcedureTypeName];
end;

//=== { TJclEqualityComparerParams } =========================================

function TJclEqualityComparerParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taEqualityComparerInterfaceName];
end;

//=== { TJclComparerParams } =================================================

function TJclComparerParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taComparerInterfaceName];
end;

//=== { TJclHashConverterParams } ============================================

function TJclHashConverterParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taHashConverterInterfaceName];
end;

//=== { TJclReleaseEventParams } =============================================

function TJclReleaseEventParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taReleaseEventTypeName];
end;

//=== { TJclOwnerParams } ====================================================

function TJclOwnerParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taOwnershipInterfaceName];
end;

function TJclOwnerParams.GetAncestorName: string;
begin
  Result := FAncestorName;
  if Result = '' then
    Result := 'IInterface';
end;

function TJclOwnerParams.GetOwnerAdditional: string;
begin
  Result := FOwnerAdditional;
  if (Result = '') and TypeInfo.TObjectType then
    Result := NativeLineBreak +
      '  function GetOwnsObjects: Boolean;' + NativeLineBreak +
      '  property OwnsObjects: Boolean read GetOwnsObjects;';
end;

//=== { TJclTreeIteratorParams } =============================================

function TJclTreeIteratorParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taTreeIteratorInterfaceName];
end;

//=== { TJclBinaryTreeIteratorParams } =======================================

function TJclBinaryTreeIteratorParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taBinaryTreeIteratorInterfaceName];
end;

//=== { TJclCollectionParams } ===============================================

function TJclCollectionParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taCollectionInterfaceName];
end;

//=== { TJclListParams } =====================================================

function TJclListParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taListInterfaceName];
end;

//=== { TJclArrayParams } ====================================================

function TJclArrayParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taArrayInterfaceName];
end;

//=== { TJclSetParams } ======================================================

function TJclSetParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taSetInterfaceName];
end;

//=== { TJclTreeParams } =====================================================

function TJclTreeParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taTreeInterfaceName];
end;

//=== { TJclMapParams } =====================================================

function TJclMapParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [maMapInterfaceName];
end;

//=== { TJclQueueParams } ====================================================

function TJclQueueParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taQueueInterfaceName];
end;

//=== { TJclSortedMapParams } ================================================

function TJclSortedMapParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [maSortedMapInterfaceName];
end;

//=== { TJclSortedSetParams } ================================================

function TJclSortedSetParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taSortedSetInterfaceName];
end;

//=== { TJclStackParams } ====================================================

function TJclStackParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taStackInterfaceName];
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

