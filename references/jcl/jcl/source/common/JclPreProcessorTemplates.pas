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
{ The Original Code is JclOtaTemplates.pas.                                                        }
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

unit JclPreProcessorTemplates;

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
  JclPreProcessorParser;

type
  TJclTemplateParams = class(TPppState)
  public
    constructor Create;
  end;

const
  ModulePattern = '%MODULENAME%';
  FormPattern = '%FORMNAME%';
  AncestorPattern = '%ANCESTORNAME%';

function GetFinalFormContent(const Content, FormIdent,
  AncestorIdent: string): string;
function GetFinalHeaderContent(const Content, ModuleIdent, FormIdent,
  AncestorIdent: string): string;
function GetFinalSourceContent(const Content, ModuleIdent, FormIdent,
  AncestorIdent: string): string;

function ApplyTemplate(const Template: string; const Params: TJclTemplateParams): string;

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
  System.TypInfo,
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils,
  TypInfo,
  {$ENDIF ~HAS_UNITSCOPE}
  JclStrings, JclSysUtils;

//=== { TJclTemplateParams } =================================================

constructor TJclTemplateParams.Create;
begin
  inherited Create;
  Options := Options + [poProcessDefines, poProcessMacros, poProcessValues];
end;

function GetFinalFormContent(const Content, FormIdent,
  AncestorIdent: string): string;
begin
  Result := StringReplace(Content, FormPattern, FormIdent, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, AncestorPattern, AncestorIdent, [rfReplaceAll, rfIgnoreCase]);
end;

function GetFinalHeaderContent(const Content, ModuleIdent, FormIdent,
  AncestorIdent: string): string;
begin
  Result := StringReplace(Content, FormPattern, FormIdent, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, AncestorPattern, AncestorIdent, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, ModulePattern, ModuleIdent, [rfReplaceAll, rfIgnoreCase]);
end;

function GetFinalSourceContent(const Content, ModuleIdent, FormIdent, AncestorIdent: string): string;
begin
  Result := StringReplace(Content, FormPattern, FormIdent, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, AncestorPattern, AncestorIdent, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, ModulePattern, ModuleIdent, [rfReplaceAll, rfIgnoreCase]);
end;

function ApplyTemplate(const Template: string; const Params: TJclTemplateParams): string;
var
  JppParser: TJppParser;
begin
  Params.PushState;
  try
    JppParser := TJppParser.Create(Template, Params);
    try
      Result := JppParser.Parse;
    finally
      JppParser.Free;
    end;
  finally
    Params.PopState;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
