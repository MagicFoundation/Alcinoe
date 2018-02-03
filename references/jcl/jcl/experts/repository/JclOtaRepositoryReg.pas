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
{ The Original Code is JclOtaRepositoryReg.pas.                                                    }
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

unit JclOtaRepositoryReg;

interface

{$I jcl.inc}

uses
  SysUtils, Classes,
  ToolsAPI,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclIDEUtils,
  JclOtaUtils, JclOtaRepositoryUtils;

procedure RegisterJclOTARepositoryExpert(ExpertClass: TJclOTARepositoryExpertClass;
  const ExpertPersonality: string);

// procedure UnregisterJclOTARepositoryExpert(ExpertClass: TJclOTARepositoryExpertClass);

// design package entry point
procedure Register;

// expert DLL entry point
function JCLWizardInit(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var TerminateProc: TWizardTerminateProc): Boolean; stdcall;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\repository';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  Windows,
  JclStrings, JclFileUtils, JclRegistry,
  JclOtaResources, JclOtaConsts, JclPreProcessorTemplates;

type
  TExpertRecord = record
    ExpertClass: TJclOTARepositoryExpertClass;
    ExpertPersonality: string;
    ExpertIndex: Integer;
  end;
  
var
  ExpertClassList: array of TExpertRecord;

procedure RegisterJclOTARepositoryExpert(ExpertClass: TJclOTARepositoryExpertClass;
  const ExpertPersonality: string);
var
  I: Integer;
begin
  I := Length(ExpertClassList);
  SetLength(ExpertClassList, I + 1);
  ExpertClassList[I].ExpertClass := ExpertClass;
  ExpertClassList[I].ExpertPersonality := ExpertPersonality;
  ExpertClassList[I].ExpertIndex := -1;
end;

{procedure UnregisterJclOTARepositoryExpert(ExpertClass: TJclOTARepositoryExpertClass);
var
  I, J, K: Integer;
  OTAWizardServices: IOTAWizardServices;
begin
  for I := High(ExpertClassList) downto Low(ExpertClassList) do
    if ExpertClassList[I].ExpertClass = ExpertClass then
  begin
    if ExpertClassList[I].ExpertIndex <> -1 then
    begin
      if OTAWizardServices = nil then
        OTAWizardServices := TJclOTAExpertBase.GetOTAWizardServices;

      OTAWizardServices.RemoveWizard(ExpertClassList[I].ExpertIndex);
    end;
    J := High(ExpertClassList);
    for K := I to J do
      ExpertClassList[K] := ExpertClassList[K + 1];
    SetLength(ExpertClassList, J);
  end;
end;}

procedure Register;
var
  I: Integer;
begin
  try
    for I := Low(ExpertClassList) to High(ExpertClassList) do
      if (ExpertClassList[I].ExpertPersonality = '')
        or TJclOTAExpertBase.IsPersonalityLoaded(ExpertClassList[I].ExpertPersonality) then
        RegisterPackageWizard(ExpertClassList[I].ExpertClass.Create);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure JclWizardTerminate;
var
  OTAWizardServices: IOTAWizardServices;
begin
  try
    OTAWizardServices := TJclOTAExpertBase.GetOTAWizardServices;
    
    {$IFDEF DELPHIEXCDLG}
    if JCLDelphiWizardIndex <> -1 then
      OTAWizardServices.RemoveWizard(JCLDelphiWizardIndex);
    {$ENDIF DELPHIEXCDLG}

    {$IFDEF CBUILDEREXCDLG}
    if JclCBuilderWizardIndex <> -1 then
      OTAWizardServices.RemoveWizard(JclCBuilderWizardIndex);
    {$ENDIF CBUILDEREXCDLG}
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

function JCLWizardInit(const BorlandIDEServices: IBorlandIDEServices;
    RegisterProc: TWizardRegisterProc;
    var TerminateProc: TWizardTerminateProc): Boolean stdcall;
//var
//  I: Integer;
//  OTAWizardServices: IOTAWizardServices;
begin
  try
    TerminateProc := JclWizardTerminate;

    //OTAWizardServices := TJclOTAExpertBase.GetOTAWizardServices;

    //for I := Low(ExpertClassList) to High(ExpertClassList) do
    //  if (ExpertClassList[I].ExpertPersonality = '')
    //    or IsPersonalityLoaded(BorlandIDEServices, ExpertClassList[I].ExpertPersonality) then
    //    ExpertClassList[I].ExpertIndex := OTAWizardServices.AddWizard(ExpertClassList[I].ExpertClass.Create);

    Result := True;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      Result := False;
    end;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
