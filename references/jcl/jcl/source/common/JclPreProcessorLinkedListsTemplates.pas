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
{ The Original Code is JclLinkedListsTemplates.pas.                                                }
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

unit JclPreProcessorLinkedListsTemplates;

interface

{$I jcl.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclPreProcessorContainerTypes,
  JclPreProcessorContainerTemplates,
  JclPreProcessorContainer1DTemplates;

type
  (* JCLLINKEDLISTTYPESINT(ITEMCLASSNAME, TYPENAME) *)
  TJclLinkedListTypeIntParams = class(TJclContainerInterfaceParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property ItemClassName: string index taLinkedListItemClassName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* JCLLINKEDLISTINT(ITEMCLASSNAME, SELFCLASSNAME, ANCESTORCLASSNAME,
                      BASECONTAINTERINTERFACENAME, FLATCONTAINERINTERFACENAME, COLLECTIONINTERFACENAME,
                      LISTINTERFACENAME, ITRINTERFACENAME, EQUALITYCOMPARERINTERFACENAME,
                      INTERFACEADDITIONAL, SECTIONADDITIONAL,
                      COLLECTIONFLAGS, OWNERSHIPDECLARATION, CONSTKEYWORD, PARAMETERNAME,
                      TYPENAME, GETTERFUNCTIONNAME, SETTERPROCEDURENAME) *)
  TJclLinkedListIntParams = class(TJclCollectionInterfaceParams)
  protected
    // function CodeUnit: string; override;
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property ItemClassName: string index taLinkedListItemClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property SelfClassName: string index taLinkedListClassName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property AncestorClassName;
    property BaseContainerInterfaceName: string index taContainerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property FlatContainerInterfaceName: string index taFlatContainerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property CollectionInterfaceName: string index taCollectionInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ListInterfaceName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property EqualityComparerInterfaceName: string index taEqualityComparerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property InterfaceAdditional;
    property SectionAdditional;
    property CollectionFlags;
    property OwnershipDeclaration;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property GetterFunctionName: string index taGetterFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterProcedureName: string index taSetterProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* JCLLINKEDLISTITRINT(SELFCLASSNAME, ITRINTERFACENAME, LISTCLASSNAME, EQUALITYCOMPARERINTERFACENAME,
                         ITEMCLASSNAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, DEFAULTVALUE,
                         GETTERFUNCTIONNAME, SETTERPROCEDURENAME) *)
  TJclLinkedListItrIntParams = class(TJclContainerInterfaceParams)
  protected
    // function CodeUnit: string; override;
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property SelfClassName: string index taLinkedListIteratorClassName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ListClassName: string index taLinkedListClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property EqualityComparerInterfaceName: string index taEqualityComparerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItemClassName: string index taLinkedListItemClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property DefaultValue: string index taDefaultValue read GetTypeAttribute write SetTypeAttribute stored False;
    property GetterFunctionName: string index taGetterFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterProcedureName: string index taSetterProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* JCLLINKEDLISTIMP(SELFCLASSNAME, ITEMCLASSNAME, COLLECTIONINTERFACENAME, LISTINTERFACENAME,
                      ITRINTERFACENAME, ITRCLASSNAME, OWNERSHIPDECLARATION, OWNERSHIPPARAMETERNAME,
                      CONSTKEYWORD, PARAMETERNAME, TYPENAME, DEFAULTVALUE,
                      GETTERFUNCTIONNAME, SETTERPROCEDURENAME, RELEASERFUNCTIONNAME) *)
  TJclLinkedListImpParams = class(TJclCollectionImplementationParams)
  protected
    // function CodeUnit: string; override;
  public
    function GetConstructorParameters: string; override;
    function GetSelfClassName: string; override;
  published
    property SelfClassName: string index taLinkedListClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItemClassName: string index taLinkedListItemClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property CollectionInterfaceName: string index taCollectionInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ListInterfaceName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrClassName: string index taLinkedListIteratorClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property OwnershipDeclaration;
    property OwnershipParameterName: string index taOwnershipParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property DefaultValue: string index taDefaultValue read GetTypeAttribute write SetTypeAttribute stored False;
    property GetterFunctionName: string index taGetterFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterProcedureName: string index taSetterProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
    property ReleaserFunctionName: string index taReleaserFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property MacroFooter;
  end;

  (* JCLLINKEDLISTITRIMP(SELFCLASSNAME, ITRINTERFACENAME, LISTCLASSNAME, EQUALITYCOMPARERINTERFACENAME,
                         ITEMCLASSNAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, DEFAULTVALUE,
                         GETTERFUNCTIONNAME, SETTERPROCEDURENAME, RELEASERCALL) *)
  TJclLinkedListItrImpParams = class(TJclContainerImplementationParams)
  private
    FReleaserCall: string;
    function GetReleaserCall: string;
  protected
    // function CodeUnit: string; override;
  public
    procedure ResetDefault(Value: Boolean); override;
  published
    property SelfClassName: string index taLinkedListIteratorClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ListClassName: string index taLinkedListClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property EqualityComparerInterfaceName: string index taEqualityComparerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItemClassName: string index taLinkedListItemClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property DefaultValue: string index taDefaultValue read GetTypeAttribute write SetTypeAttribute stored False;
    property GetterFunctionName: string index taGetterFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterProcedureName: string index taSetterProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
    property ReleaserCall: string read GetReleaserCall write FReleaserCall;
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
  RegisterContainerParams('JCLLINKEDLISTTYPESINT', TJclLinkedListTypeIntParams);
  RegisterContainerParams('JCLLINKEDLISTINT', TJclLinkedListIntParams);
  RegisterContainerParams('JCLLINKEDLISTITRINT', TJclLinkedListItrIntParams);
  RegisterContainerParams('JCLLINKEDLISTIMP', TJclLinkedListImpParams, TJclLinkedListIntParams);
  RegisterContainerParams('JCLLINKEDLISTITRIMP', TJclLinkedListItrImpParams, TJclLinkedListItrIntParams);
end;

//=== { TJclLinkedListTypeIntParams } ========================================

function TJclLinkedListTypeIntParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taLinkedListItemClassName];
end;

//=== { TJclLinkedListIntParams } ============================================

function TJclLinkedListIntParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taLinkedListClassName];
end;

//=== { TJclLinkedListItrIntParams } =========================================

function TJclLinkedListItrIntParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taLinkedListIteratorClassName];
end;

//=== { TJclLinkedListImpParams } ============================================

function TJclLinkedListImpParams.GetConstructorParameters: string;
begin
  Result := 'nil';
end;

function TJclLinkedListImpParams.GetSelfClassName: string;
begin
  Result := SelfClassName;
end;

//=== { TJclLinkedListItrImpParams } =========================================

function TJclLinkedListItrImpParams.GetReleaserCall: string;
begin
  Result := FReleaserCall;
  if (Result = '') and TypeInfo.KnownType then
  begin
    if TypeInfo.TObjectType then
      Result := '(FownList as IJclObjectOwner).FreeObject(FCursor.Value);'
    else
      Result := Format('FCursor.Value := %s;', [TypeInfo.TypeAttributes[taDefaultValue]]);
  end;
end;

procedure TJclLinkedListItrImpParams.ResetDefault(Value: Boolean);
begin
  inherited ResetDefault(Value);
  FReleaserCall := '';
  if not Value then
    FReleaserCall := GetReleaserCall;
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

