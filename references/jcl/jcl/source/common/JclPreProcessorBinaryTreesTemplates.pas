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
{ The Original Code is JclBinaryTreesTemplates.pas.                                                }
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

unit JclPreProcessorBinaryTreesTemplates;

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
  (* JCLBINARYTREETYPESINT(NODETYPENAME, TYPENAME) *)
  TJclBinaryTreeTypeIntParams = class(TJclContainerInterfaceParams)
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property NodeTypeName: string index taBinaryTreeNodeTypeName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* JCLBINARYTREEINT(NODETYPENAME, SELFCLASSNAME, ANCESTORCLASSNAME, BASECONTAINERINTERFACENAME,
                      FLATCONTAINERINTERFACENAME, COLLECTIONINTERFACENAME,
                      TREEINTERFACENAME, STDITRINTERFACENAME, TREEITRINTERFACENAME,
                      EQUALITYCOMPARERINTERFACENAME, COMPARERINTERFACENAME, INTERFACEADDITIONAL,
                      SECTIONADDITIONAL, CONSTRUCTORPARAMETERS, COLLECTIONFLAGS, CONSTKEYWORD,
                      PARAMETERNAME, TYPENAME) *)
  TJclBinaryTreeIntParams = class(TJclCollectionInterfaceParams)
  private
    FConstructorDeclarations: string;
  protected
    // function CodeUnit: string; override;
    function GetConstructorDeclarations: string;
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
    procedure ResetDefault(Value: Boolean); override;
  published
    property NodeTypeName: string index taBinaryTreeNodeTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property SelfClassName: string index taBinaryTreeClassName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property AncestorClassName;
    property BaseContainerInterfaceName: string index taContainerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property FlatContainerInterfaceName: string index taFlatContainerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property CollectionInterfaceName: string index taCollectionInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property CompareFunctionTypeName: string index taCompareFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property EqualityComparerInterfaceName: string index taEqualityComparerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ComparerInterfaceName: string index taComparerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property TreeInterfaceName: string index taTreeInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property StdItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property TreeItrInterfaceName: string index taTreeIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property InterfaceAdditional;
    property SectionAdditional;
    property ConstructorDeclarations: string read GetConstructorDeclarations write FConstructorDeclarations;
    property OwnershipDeclaration;
    property CollectionFlags;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* JCLBINARYTREEITRINT(BASEITRCLASSNAME, PREORDERITRCLASSNAME, INORDERITRCLASSNAME, POSTORDERITRCLASSNAME,
                         STDITRINTERFACENAME, STDTREEITRINTERFACENAME, BINTREEITRINTERFACENAME,
                         COLLECTIONINTERFACENAME, EQUALITYCOMPARERINTERFACENAME, NODETYPENAME,
                         CONSTKEYWORD, PARAMETERNAME, TYPENAME, GETTERFUNCTIONNAME, SETTERPROCEDURENAME) *)
  TJclBinaryTreeItrIntParams = class(TJclContainerInterfaceParams)
  protected
    // function CodeUnit: string; override;
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property BaseItrClassName: string index taBinaryTreeBaseIteratorClassName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property PreOrderItrClassName: string index taBinaryTreePreOrderIteratorClassName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property InOrderItrClassName: string index taBinaryTreeInOrderIteratorClassName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property PostOrderItrClassName: string index taBinaryTreePostOrderIteratorClassName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property StdItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property StdTreeItrInterfaceName: string index taTreeIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property BinTreeItrInterfaceName: string index taBinaryTreeIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property CollectionInterfaceName: string index taCollectionInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property EqualityComparerInterfaceName: string index taEqualityComparerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property NodeTypeName: string index taBinaryTreeNodeTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property GetterFunctionName: string index taGetterFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterProcedureName: string index taSetterProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* JCLBINARYTREEIMP(SELFCLASSNAME, NODETYPENAME, PREORDERITRCLASSNAME, INORDERITRCLASSNAME,
                      POSTORDERITRCLASSNAME, COLLECTIONINTERFACENAME, STDITRINTERFACENAME,
                      TREEITRINTERFACENAME, CONSTRUCTORPARAMETERS, CONSTRUCTORASSIGNMENTS,
                      OWNERSHIPPARAMETERNAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, DEFAULTVALUE,
                      RELEASERFUNCTIONNAME) *)
  TJclBinaryTreeImpParams = class(TJclCollectionImplementationParams)
  private
    FConstructorAssignments: string;
    FConstructorDeclarations: string;
  protected
    // function CodeUnit: string; override;
    function GetConstructorAssignments: string;
    function GetConstructorDeclarations: string;
  public
    function GetConstructorParameters: string; override;
    function GetSelfClassName: string; override;
    procedure ResetDefault(Value: Boolean); override;
  published
    property SelfClassName: string index taBinaryTreeClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property NodeTypeName: string index taBinaryTreeNodeTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property PreOrderItrClassName: string index taBinaryTreePreOrderIteratorClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property InOrderItrClassName: string index taBinaryTreeInOrderIteratorClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property PostOrderItrClassName: string index taBinaryTreePostOrderIteratorClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property CollectionInterfaceName: string index taCollectionInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property StdItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property TreeItrInterfaceName: string index taTreeIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstructorDeclarations: string read GetConstructorDeclarations write FConstructorDeclarations;
    property ConstructorAssignments: string read GetConstructorAssignments write FConstructorAssignments;
    property CompareFunctionTypeName: string index taCompareFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property OwnershipDeclaration;
    property OwnershipParameterName: string index taOwnershipParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property ReleaserFunctionName: string index taReleaserFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property DefaultValue: string index taDefaultValue read GetTypeAttribute write SetTypeAttribute stored False;
    property MacroFooter;
  end;

  (* JCLBINARYTREEITRIMP(BASEITRCLASSNAME, PREORDERITRCLASSNAME, INORDERITRCLASSNAME, POSTORDERITRCLASSNAME,
                         STDITRINTERFACENAME, COLLECTIONINTERFACENAME, EQUALITYCOMPARERINTERFACENAME,
                         NODETYPENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, DEFAULTVALUE,
                         GETTERFUNCTIONNAME, SETTERPROCEDURENAME, RELEASERFUNCTIONNAME) *)
  TJclBinaryTreeItrImpParams = class(TJclContainerImplementationParams)
  protected
    // function CodeUnit: string; override;
  published
    property BaseItrClassName: string index taBinaryTreeBaseIteratorClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property PreOrderItrClassName: string index taBinaryTreePreOrderIteratorClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property InOrderItrClassName: string index taBinaryTreeInOrderIteratorClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property PostOrderItrClassName: string index taBinaryTreePostOrderIteratorClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property StdItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property CollectionInterfaceName: string index taCollectionInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property EqualityComparerInterfaceName: string index taEqualityComparerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property NodeTypeName: string index taBinaryTreeNodeTypeName read GetTypeAttribute write SetTypeAttribute stored False;
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
  RegisterContainerParams('JCLBINARYTREETYPESINT', TJclBinaryTreeTypeIntParams);
  RegisterContainerParams('JCLBINARYTREEINT', TJclBinaryTreeIntParams);
  RegisterContainerParams('JCLBINARYTREEITRINT', TJclBinaryTreeItrIntParams);
  RegisterContainerParams('JCLBINARYTREEIMP', TJclBinaryTreeImpParams, TJclBinaryTreeIntParams);
  RegisterContainerParams('JCLBINARYTREEITRIMP', TJclBinaryTreeItrImpParams, TJclBinaryTreeItrIntParams);
end;

//=== { TJclBinaryTreeTypeIntParams } ========================================

function TJclBinaryTreeTypeIntParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taBinaryTreeNodeTypeName];
end;

//=== { TJclBinaryTreeIntParams } ============================================

function TJclBinaryTreeIntParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taBinaryTreeClassName];
end;

function TJclBinaryTreeIntParams.GetConstructorDeclarations: string;
begin
  Result := FConstructorDeclarations;
  if (Result = '') and TypeInfo.KnownType then
    Result := 'ACompare: ' + CompareFunctionTypeName;
end;

procedure TJclBinaryTreeIntParams.ResetDefault(Value: Boolean);
begin
  inherited ResetDefault(Value);
  FConstructorDeclarations := '';
  if not Value then
    FConstructorDeclarations := GetConstructorDeclarations;
end;

//=== { TJclBinaryTreeItrIntParams } =========================================

function TJclBinaryTreeItrIntParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taBinaryTreeBaseIteratorClassName, taBinaryTreePreOrderIteratorClassName,
             taBinaryTreeInOrderIteratorClassName, taBinaryTreePostOrderIteratorClassName];
end;

//=== { TJclBinaryTreeImpParams } ============================================

function TJclBinaryTreeImpParams.GetConstructorAssignments: string;
begin
  Result := FConstructorAssignments;
  if (Result = '') and TypeInfo.KnownType then
    Result := NativeLineBreak + '  SetCompare(ACompare);';
end;

function TJclBinaryTreeImpParams.GetConstructorDeclarations: string;
begin
  Result := FConstructorDeclarations;
  if (Result = '') and TypeInfo.KnownType then
    Result := 'ACompare: ' + CompareFunctionTypeName;
end;

function TJclBinaryTreeImpParams.GetConstructorParameters: string;
begin
  Result := 'Compare';
end;

function TJclBinaryTreeImpParams.GetSelfClassName: string;
begin
  Result := SelfClassName;
end;

procedure TJclBinaryTreeImpParams.ResetDefault(Value: Boolean);
begin
  inherited ResetDefault(Value);
  FConstructorAssignments := '';
  FConstructorDeclarations := '';
  if not Value then
  begin
    FConstructorAssignments := GetConstructorAssignments;
    FConstructorDeclarations := GetConstructorDeclarations;
  end;
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

