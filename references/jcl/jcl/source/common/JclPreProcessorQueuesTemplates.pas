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
{ The Original Code is JclQueuesTemplates.pas.                                                     }
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

unit JclPreProcessorQueuesTemplates;

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
  (* JCLQUEUEINT(SELFCLASSNAME, QUEUEINTERFACENAME, ANCESTORCLASSNAME, DYNARRAYTYPENAME,
                 EQUALITYCOMPARERINTERFACENAME, INTERFACEADDITIONAL, SECTIONADDITIONAL,
                 OWNERSHIPDECLARATION, CONSTKEYWORD, PARAMETERNAME, TYPENAME) *)
  TJclQueueIntParams = class(TJclClassInterfaceParams)
  protected
    // function CodeUnit: string; override;
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property SelfClassName: string index taQueueClassName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property BaseContainerInterfaceName: string index taContainerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property QueueInterfaceName: string index taQueueInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property AncestorClassName;
    property DynArrayTypeName: string index taDynArrayTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property EqualityComparerInterfaceName: string index taEqualityComparerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property InterfaceAdditional;
    property SectionAdditional;
    property OwnershipDeclaration;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* JCLQUEUEIMP(SELFCLASSNAME, OWNERSHIPDECLARATION, OWNERSHIPPARAMETERNAME, MOVEARRAYPROCEDURENAME,
                 CONSTKEYWORD, PARAMETERNAME, TYPENAME, DEFAULTVALUE, RELEASERFUNCTIONNAME) *)
  TJclQueueImpParams = class(TJclClassImplementationParams)
  protected
    // function CodeUnit: string; override;
  public
    function GetConstructorParameters: string; override;
    function GetSelfClassName: string; override;
  published
    property SelfClassName: string index taQueueClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property OwnershipDeclaration;
    property OwnershipParameterName: string index taOwnershipParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property MoveArrayProcedureName: string index taMoveArrayProcedureName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property DefaultValue: string index taDefaultValue read GetTypeAttribute write SetTypeAttribute stored False;
    property ReleaserFunctionName: string index taReleaserFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
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
  RegisterContainerParams('JCLQUEUEINT', TJclQueueIntParams);
  RegisterContainerParams('JCLQUEUEIMP', TJclQueueImpParams, TJclQueueIntParams);
end;

//=== { TJclQueueIntParams } =================================================

function TJclQueueIntParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taQueueClassName];
end;

//=== { TJclQueueImpParams } =================================================

function TJclQueueImpParams.GetConstructorParameters: string;
begin
  Result := 'Size + 1';
end;

function TJclQueueImpParams.GetSelfClassName: string;
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

