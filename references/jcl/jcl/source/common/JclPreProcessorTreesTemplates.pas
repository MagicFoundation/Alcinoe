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
{ The Original Code is JclTreesTemplates.pas.                                                      }
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

unit JclPreProcessorTreesTemplates;

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
  (* JCLTREETYPESINT(NODETYPENAME, EQUALITYCOMPARERINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME) *)
  TJclTreeTypeIntParams = class(TJclContainerInterfaceParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property NodeTypeName: string index taTreeNodeClassName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property EqualityComparerInterfaceName: string index taEqualityComparerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* JCLTREEINT(NODETYPENAME, SELFCLASSNAME, ANCESTORCLASSNAME,
                BASECONTAINERINTERFACENAME, FLATCONTAINERINTERFACENAME, EQUALITYCOMPARERINTERFACENAME,
                COLLECTIONINTERFACENAME, TREEINTERFACENAME, STDITRINTERFACENAME, TREEITRINTERFACENAME,
                INTERFACEADDITIONAL, SECTIONADDITIONAL, COLLECTIONFLAGS, OWNERSHIPDECLARATION,
                CONSTKEYWORD, PARAMETERNAME, TYPENAME, DEFAULTVALUE) *)
  TJclTreeIntParams = class(TJclCollectionInterfaceParams)
  protected
    // function CodeUnit: string; override;
    function GetOwnershipDeclaration: string; override;
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property NodeTypeName: string index taTreeNodeClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property SelfClassName: string index taTreeClassName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property AncestorClassName;
    property BaseContainerInterfaceName: string index taContainerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property FlatContainerInterfaceName: string index taFlatContainerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property EqualityComparerInterfaceName: string index taEqualityComparerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property CollectionInterfaceName: string index taCollectionInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property TreeInterfaceName: string index taTreeInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property StdItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property TreeItrInterfaceName: string index taTreeIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property InterfaceAdditional;
    property SectionAdditional;
    property OwnershipDeclaration;
    property CollectionFlags;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property DefaultValue: string index taDefaultValue read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* JCLTREEITRINT(BASEITRCLASSNAME, PREORDERITRCLASSNAME, POSTORDERITRCLASSNAME, NODETYPENAME,
                   TREECLASSNAME, STDITRINTERFACENAME, TREEITRINTERFACENAME, EQUALITYCOMPARERINTERFACENAME,
                   CONSTKEYWORD, PARAMETERNAME, TYPENAME, DEFAULTVALUE, GETTERFUNCTIONNAME, SETTERPROCEDURENAME) *)
  TJclTreeItrIntParams = class(TJclContainerInterfaceParams)
  protected
    // function CodeUnit: string; override;
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property BaseItrClassName: string index taTreeBaseIteratorClassName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property PreOrderItrClassName: string index taTreePreOrderIteratorClassName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property PostOrderItrClassName: string index taTreePostOrderIteratorClassName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property NodeTypeName: string index taTreeNodeClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property TreeClassName: string index taTreeClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property StdItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property TreeItrInterfaceName: string index taTreeIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property EqualityComparerInterfaceName: string index taEqualityComparerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property DefaultValue: string index taDefaultValue read GetTypeAttribute write SetTypeAttribute stored False;
    property GetterFunctionName: string index taGetterFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterProcedureName: string index taSetterProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* JCLTREETYPESIMP(NODETYPENAME, EQUALITYCOMPARERINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME) *)
  TJclTreeTypeImpParams = class(TJclContainerImplementationParams)
  published
    property NodeTypeName: string index taTreeNodeClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property EqualityComparerInterfaceName: string index taEqualityComparerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* JCLTREEIMP(NODETYPENAME, SELFCLASSNAME, PREORDERITRCLASSNAME, POSTORDERITRCLASSNAME,
                COLLECTIONINTERFACENAME, STDITRINTERFACENAME, TREEITRINTERFACENAME,
                EQUALITYCOMPARERINTERFACENAME, OWNERSHIPDECLARATION, OWNERSHIPPARAMETERNAME,
                CONSTKEYWORD, PARAMETERNAME, TYPENAME, DEFAULTVALUE, RELEASERFUNCTIONNAME) *)
  TJclTreeImpParams = class(TJclCollectionImplementationParams)
  protected
    // function CodeUnit: string; override;
    function GetConstructorParameters: string; override;
    function GetSelfClassName: string; override;
    function GetOwnershipDeclaration: string; override;
  published
    property SelfClassName: string index taTreeClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property NodeTypeName: string index taTreeNodeClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property PreOrderItrClassName: string index taTreePreOrderIteratorClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property PostOrderItrClassName: string index taTreePostOrderIteratorClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property CollectionInterfaceName: string index taCollectionInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property StdItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property TreeItrInterfaceName: string index taTreeIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property EqualityComparerInterfaceName: string index taEqualityComparerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property OwnershipDeclaration;
    property OwnershipParameterName: string index taOwnershipParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property DefaultValue: string index taDefaultValue read GetTypeAttribute write SetTypeAttribute stored False;
    property ReleaserFunctionName: string index taReleaserFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property MacroFooter;
  end;

  (* JCLTREEITRIMP(BASEITRCLASSNAME, PREORDERITRCLASSNAME, POSTORDERITRCLASSNAME, NODETYPENAME, TREECLASSNAME,
                   STDITRINTERFACENAME, TREEITRINTERFACENAME, EQUALITYCOMPARERINTERFACENAME,
                   CONSTKEYWORD, PARAMETERNAME, TYPENAME, DEFAULTVALUE, GETTERFUNCTIONNAME, SETTERPROCEDURENAME, RELEASERFUNCTIONNAME) *)
  TJclTreeItrImpParams = class(TJclContainerImplementationParams)
  protected
    // function CodeUnit: string; override;
  published
    property BaseItrClassName: string index taTreeBaseIteratorClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property PreOrderItrClassName: string index taTreePreOrderIteratorClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property PostOrderItrClassName: string index taTreePostOrderIteratorClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property NodeTypeName: string index taTreeNodeClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property TreeClassName: string index taTreeClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property StdItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property TreeItrInterfaceName: string index taTreeIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property EqualityComparerInterfaceName: string index taEqualityComparerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property DefaultValue: string index taDefaultValue read GetTypeAttribute write SetTypeAttribute stored False;
    property GetterFunctionName: string index taGetterFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterProcedureName: string index taSetterProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
    property ReleaserFunctionName: string index taReleaserFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
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
  RegisterContainerParams('JCLTREETYPESINT', TJclTreeTypeIntParams);
  RegisterContainerParams('JCLTREETYPESIMP', TJclTreeTypeImpParams, TJclTreeTypeIntParams);
  RegisterContainerParams('JCLTREEINT', TJclTreeIntParams);
  RegisterContainerParams('JCLTREEITRINT', TJclTreeItrIntParams);
  RegisterContainerParams('JCLTREEIMP', TJclTreeImpParams, TJclTreeIntParams);
  RegisterContainerParams('JCLTREEITRIMP', TJclTreeItrImpParams, TJclTreeItrIntParams);
end;

//=== { TJclTreeTypeIntParams } ==============================================

function TJclTreeTypeIntParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taTreeNodeClassName];
end;

//=== { TJclTreeIntParams } ==================================================

function TJclTreeIntParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taTreeClassName];
end;

function TJclTreeIntParams.GetOwnershipDeclaration: string;
begin
  Result := TypeInfo.OwnershipDeclaration;
end;

//=== { TJclTreeItrIntParams } ===============================================

function TJclTreeItrIntParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taTreeBaseIteratorClassName, taTreePreOrderIteratorClassName,
             taTreePostOrderIteratorClassName];
end;

//=== { TJclTreeImpParams } ==================================================

function TJclTreeImpParams.GetConstructorParameters: string;
begin
  Result := '';
end;

function TJclTreeImpParams.GetOwnershipDeclaration: string;
begin
  Result := TypeInfo.OwnershipDeclaration;
end;

function TJclTreeImpParams.GetSelfClassName: string;
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

