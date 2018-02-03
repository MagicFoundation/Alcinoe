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
{ The Original Code is JclAlgorithmsTemplates.pas.                                                 }
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

unit JclPreProcessorAlgorithmsTemplates;

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
  TJclAlgorithmsIntParams = class(TJclContainerInterfaceParams)
  protected
    // function CodeUnit: string; override;
  end;

  TJclAlgorithmsIntProcParams = class(TJclAlgorithmsIntParams)
  protected
    FOverload: string;
  public
    property Overload: string read FOverload write FOverload;
  end;

  TJclAlgorithmsImpProcParams = class(TJclContainerImplementationParams)
  protected
    // function CodeUnit: string; override;
  end;

  (* MOVEARRAYINT(MOVEARRAYPROCEDURENAME, DYNARRAYTYPENAME, OVERLOAD) *)
  TJclMoveArrayIntParams = class(TJclAlgorithmsIntProcParams)
  published
    property Overload;
    property MoveArrayProcedureName: string index taMoveArrayProcedureName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property DynArrayTypeName: string index taDynArrayTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* MOVEARRAYIMP(MOVEARRAYPROCEDURENAME, DYNARRAYTYPENAME, DEFAULTVALUE) *)
  TJclMoveArrayImpParams = class(TJclAlgorithmsImpProcParams)
  published
    property MoveArrayProcedureName: string index taMoveArrayProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
    property DynArrayTypeName: string index taDynArrayTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property DefaultValue: string index taDefaultValue read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* ITERATEINT(ITERATEPROCEDURENAME, ITRINTERFACENAME, ITERATEPROCEDURETYPENAME, OVERLOAD) *)
  TJclIterateIntParams = class(TJclAlgorithmsIntProcParams)
  published
    property Overload;
    property IterateProcedureName: string index taIterateProcedureName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property IterateProcedureTypeName: string index taIterateProcedureTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* ITERATEIMP(ITERATEPROCEDURENAME, ITRINTERFACENAME, ITERATEPROCEDURETYPENAME) *)
  TJclIterateImpParams = class(TJclAlgorithmsImpProcParams)
  published
    property IterateProcedureName: string index taIterateProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property IterateProcedureTypeName: string index taIterateProcedureTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* APPLYINT(APPLYPROCEDURENAME, ITRINTERFACENAME, APPLYFUNCTIONTYPENAME, OVERLOAD) *)
  TJclApplyIntParams = class(TJclAlgorithmsIntProcParams)
  published
    property Overload;
    property ApplyProcedureName: string index taApplyProcedureName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ApplyFunctionTypeName: string index taApplyFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* APPLYIMP(APPLYPROCEDURENAME, ITRINTERFACENAME, APPLYFUNCTIONNAME, SETTERPROCEDURENAME) *)
  TJclApplyImpParams = class(TJclAlgorithmsImpProcParams)
  published
    property ApplyProcedureName: string index taApplyProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ApplyFunctionTypeName: string index taApplyFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterProcedureName: string index taSetterProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* SIMPLECOMPAREINT(SIMPLECOMPAREFUNCTIONNAME, CONSTKEYWORD, TYPENAME) *)
  TJclSimpleCompareIntParams = class(TJclAlgorithmsIntParams)
  published
    property SimpleCompareFunctionName: string index taSimpleCompareFunctionName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* SIMPLEEQUALITYCOMPAREINT(SIMPLEEQUALITYCOMPAREFUNCTIONNAME, CONSTKEYWORD, TYPENAME) *)
  TJclSimpleEqualityCompareIntParams = class(TJclAlgorithmsIntParams)
  published
    property SimpleEqualityCompareFunctionName: string index taSimpleEqualityCompareFunctionName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* SIMPLEHASHCONVERTINT(SIMPLEHASHCONVERTFUNCTIONNAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME) *)
  TJclSimpleHashConvertIntParams = class(TJclAlgorithmsIntParams)
  published
    property SimpleHashConvertFunctionName: string index taSimpleHashConvertFunctionName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* FINDINT(FINDFUNCTIONNAME, ITRINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, COMPAREFUNCTIONTYPENAME, OVERLOAD) *)
  TJclFindIntParams = class(TJclAlgorithmsIntProcParams)
  published
    property FindFunctionName: string index taFindFunctionName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property CompareFunctionTypeName: string index taCompareFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* FINDIMP(FINDFUNCTIONNAME, ITRINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, COMPAREFUNCTIONTYPENAME) *)
  TJclFindImpParams = class(TJclAlgorithmsImpProcParams)
  published
    property FindFunctionName: string index taFindFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property CompareFunctionTypeName: string index taCompareFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* FINDEQINT(FINDFUNCTIONNAME,ITRINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, CALLBACKTYPE, OVERLOAD) *)
  TJclFindEqIntParams = class(TJclAlgorithmsIntProcParams)
  published
    property FindFunctionName: string index taFindFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property EqualityCompareFunctionTypeName: string index taEqualityCompareFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* FINDEQIMP(FINDFUNCTIONNAME, ITRINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, EQUALITYCOMPAREFUNCTIONTYPENAME) *)
  TJclFindEqImpParams = class(TJclAlgorithmsImpProcParams)
  published
    property FindFunctionName: string index taFindFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property EqualityCompareFunctionTypeName: string index taEqualityCompareFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* COUNTOBJECTINT(COUNTOBJECTFUNCTIONNAME, ITRINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, COMPAREFUNCTIONTYPENAME, OVERLOAD) *)
  TJclCountObjectIntParams = class(TJclAlgorithmsIntProcParams)
  published
    property CountObjectFunctionName: string index taCountObjectFunctionName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property CompareFunctionTypeName: string index taCompareFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* COUNTOBJECTIMP(COUNTOBJECTFUNCTIONNAME, ITRINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, COMPAREFUNCTIONTYPENAME) *)
  TJclCountObjectImpParams = class(TJclAlgorithmsImpProcParams)
  published
    property CountObjectFunctionName: string index taCountObjectFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property CompareFunctionTypeName: string index taCompareFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* COUNTOBJECTEQINT(COUNTOBJECTFUNCTIONNAME, ITRINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, EQUALITYCOMPAREFUNCTIONTYPENAME, OVERLOAD) *)
  TJclCountObjectEqIntParams = class(TJclAlgorithmsIntProcParams)
  published
    property CountObjectFunctionName: string index taCountObjectFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property EqualityCompareFunctionTypeName: string index taEqualityCompareFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* COUNTOBJECTEQIMP(COUNTOBJECTFUNCTIONNAME, ITRINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, EQUALITYCOMPAREFUNCTIONTYPENAME) *)
  TJclCountObjectEqImpParams = class(TJclAlgorithmsImpProcParams)
  published
    property CountObjectFunctionName: string index taCountObjectFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property EqualityCompareFunctionTypeName: string index taEqualityCompareFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* COPYINT(COPYPROCEDURENAME, ITRINTERFACENAME, OVERLOAD) *)
  TJclCopyIntParams = class(TJclAlgorithmsIntProcParams)
  published
    property CopyProcedureName: string index taCopyProcedureName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* COPYIMP(COPYPROCEDURENAME, ITRINTERFACENAME, SETTERPROCEDURENAME) *)
  TJclCopyImpParams = class(TJclAlgorithmsImpProcParams)
  published
    property CopyProcedureName: string index taCopyProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterProcedureName: string index taSetterProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* GENERATEINT(GENERATEPROCEDURENAME, LISTINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, OVERLOAD) *)
  TJclGenerateIntParams = class(TJclAlgorithmsIntProcParams)
  published
    property GenerateProcedureName: string index taGenerateProcedureName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ListInterfaceName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* GENERATEIMP(GENERATEPROCEDURENAME, LISTINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME) *)
  TJclGenerateImpParams = class(TJclAlgorithmsImpProcParams)
  published
    property GenerateProcedureName: string index taGenerateProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
    property ListInterfaceName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* FILLINT(FILLPROCEDURENAME, ITRINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, OVERLOAD) *)
  TJclFillIntParams = class(TJclAlgorithmsIntProcParams)
  published
    property FillProcedureName: string index taFillProcedureName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* FILLIMP(FILLPROCEDURENAME, ITRINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, SETTERPROCEDURENAME) *)
  TJclFillImpParams = class(TJclAlgorithmsImpProcParams)
  published
    property FillProcedureName: string index taFillProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterProcedureName: string index taSetterProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* REVERSEINT(REVERSEPROCEDURENAME, ITRINTERFACENAME, OVERLOAD) *)
  TJclReverseIntParams = class(TJclAlgorithmsIntProcParams)
  published
    property ReverseProcedureName: string index taReverseProcedureName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* REVERSEIMP(REVERSEPROCEDURENAME, ITRINTERFACENAME, TYPENAME, GETTERFUNCTIONNAME, SETTERPROCEDURENAME) *)
  TJclReverseImpParams = class(TJclAlgorithmsImpProcParams)
  published
    property ReverseProcedureName: string index taReverseProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property GetterFunctionName: string index taGetterFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterProcedureName: string index taSetterProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* SORTINT(SORTPROCEDURENAME, LISTINTERFACENAME, LEFT, RIGHT, COMPAREFUNCTIONTYPENAME, OVERLOAD) *)
  TJclSortIntParams = class(TJclAlgorithmsIntProcParams)
  private
    FLeft: string;
    FRight: string;
    function GetLeft: string;
    function GetRight: string;
    function IsLeftStored: Boolean;
    function IsRightStored: Boolean;
  published
    property SortProcedureName: string index taSortProcedureName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ListInterfaceName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property Left: string read GetLeft write FLeft stored IsLeftStored;
    property Right: string read GetRight write FRight stored IsRightStored;
    property CompareFunctionTypeName: string index taCompareFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* QUICKSORTINT(QUICKSORTPROCEDURENAME, LISTINTERFACENAME, LEFT, RIGHT, COMPAREFUNCTIONTYPENAME, OVERLOAD) *)
  TJclQuickSortIntParams = class(TJclAlgorithmsIntProcParams)
  private
    FLeft: string;
    FRight: string;
    function GetLeft: string;
    function GetRight: string;
    function IsLeftStored: Boolean;
    function IsRightStored: Boolean;
  published
    property QuickSortProcedureName: string index taQuickSortProcedureName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ListInterfaceName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property Left: string read GetLeft write FLeft stored IsLeftStored;
    property Right: string read GetRight write FRight stored IsRightStored;
    property CompareFunctionTypeName: string index taCompareFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* QUICKSORTIMP(QUICKSORTPROCEDURENAME, LISTINTERFACENAME, LEFT, RIGHT, COMPAREFUNCTIONTYPENAME, TYPENAME, GETTERFUNCTIONNAME, SETTERPROCEDURENAME) *)
  TJclQuickSortImpParams = class(TJclAlgorithmsImpProcParams)
  private
    function GetLeft: string;
    function GetRight: string;
    procedure SetLeft(const Value: string);
    procedure SetRight(const Value: string);
  published
    property QuickSortProcedureName: string index taQuickSortProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
    property ListInterfaceName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property Left: string read GetLeft write SetLeft stored False;
    property Right: string read GetRight write SetRight stored False;
    property CompareFunctionTypeName: string index taCompareFunctionTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property GetterFunctionName: string index taGetterFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterProcedureName: string index taSetterProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
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

procedure RegisterJclContainers;
begin
  RegisterContainerParams('MOVEARRAYINT', TJclMoveArrayIntParams);
  RegisterContainerParams('ITERATEINT', TJclIterateIntParams);
  RegisterContainerParams('APPLYINT', TJclApplyIntParams);
  RegisterContainerParams('SIMPLECOMPAREINT', TJclSimpleCompareIntParams);
  RegisterContainerParams('SIMPLEEQUALITYCOMPAREINT', TJclSimpleEqualityCompareIntParams);
  RegisterContainerParams('SIMPLEHASHCONVERTINT', TJclSimpleHashConvertIntParams);
  RegisterContainerParams('FINDINT', TJclFindIntParams);
  RegisterContainerParams('FINDEQINT', TJclFindEqIntParams);
  RegisterContainerParams('COUNTOBJECTINT', TJclCountObjectIntParams);
  RegisterContainerParams('COUNTOBJECTEQINT', TJclCountObjectEqIntParams);
  RegisterContainerParams('COPYINT', TJclCopyIntParams);
  RegisterContainerParams('GENERATEINT', TJclGenerateIntParams);
  RegisterContainerParams('FILLINT', TJclFillIntParams);
  RegisterContainerParams('REVERSEINT', TJclReverseIntParams);
  RegisterContainerParams('SORTINT', TJclSortIntParams);
  RegisterContainerParams('QUICKSORTINT', TJclQuickSortIntParams);

  RegisterContainerParams('MOVEARRAYIMP', TJclMoveArrayImpParams, TJclMoveArrayIntParams);
  RegisterContainerParams('ITERATEIMP', TJclIterateImpParams, TJclIterateIntParams);
  RegisterContainerParams('APPLYIMP', TJclApplyImpParams, TJclApplyIntParams);
  RegisterContainerParams('FINDIMP', TJclFindImpParams, TJclFindIntParams);
  RegisterContainerParams('FINDEQIMP', TJclFindEqImpParams, TJclFindEqIntParams);
  RegisterContainerParams('COUNTOBJECTIMP', TJclCountObjectImpParams, TJclCountObjectIntParams);
  RegisterContainerParams('COUNTOBJECTEQIMP', TJclCountObjectEqImpParams, TJclCountObjectEqIntParams);
  RegisterContainerParams('COPYIMP', TJclCopyImpParams, TJclCopyIntParams);
  RegisterContainerParams('GENERATEIMP', TJclGenerateImpParams, TJclGenerateIntParams);
  RegisterContainerParams('FILLIMP', TJclFillImpParams, TJclFillIntParams);
  RegisterContainerParams('REVERSEIMP', TJclReverseImpParams, TJclReverseIntParams);
  RegisterContainerParams('QUICKSORTIMP', TJclQuickSortImpParams, TJclQuickSortIntParams);
end;

//=== { TJclSortIntParams } ==================================================

function TJclSortIntParams.GetLeft: string;
begin
  Result := FLeft;
  if Result = '' then
    Result := 'L';
end;

function TJclSortIntParams.GetRight: string;
begin
  Result := FRight;
  if Result = '' then
    Result := 'R';
end;

function TJclSortIntParams.IsLeftStored: Boolean;
begin
  Result := FLeft <> '';
end;

function TJclSortIntParams.IsRightStored: Boolean;
begin
  Result := FRight <> '';
end;

//=== { TJclQuickSortIntParams } =============================================

function TJclQuickSortIntParams.GetLeft: string;
begin
  Result := FLeft;
  if Result = '' then
    Result := 'L';
end;

function TJclQuickSortIntParams.GetRight: string;
begin
  Result := FRight;
  if Result = '' then
    Result := 'R';
end;

function TJclQuickSortIntParams.IsLeftStored: Boolean;
begin
  Result := FLeft <> '';
end;

function TJclQuickSortIntParams.IsRightStored: Boolean;
begin
  Result := FRight <> '';
end;

//=== { TJclQuickSortImpParams } =============================================

function TJclQuickSortImpParams.GetLeft: string;
begin
  Result := (InterfaceParams as TJclQuickSortIntParams).Left;
end;

function TJclQuickSortImpParams.GetRight: string;
begin
  Result := (InterfaceParams as TJclQuickSortIntParams).Right;
end;

procedure TJclQuickSortImpParams.SetLeft(const Value: string);
begin
  (InterfaceParams as TJclQuickSortIntParams).Left := Value;
end;

procedure TJclQuickSortImpParams.SetRight(const Value: string);
begin
  (InterfaceParams as TJclQuickSortIntParams).Right := Value;
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

