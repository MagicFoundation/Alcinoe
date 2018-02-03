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
{ The Original Code is JclOtaAddinOptions.pas.                                                     }
{                                                                                                  }
{ The Initial Developer of the Original Code is Uwe Schuster.                                      }
{ Portions created by Uwe Schuster are Copyright (C) 2010 Uwe Schuster. All rights reserved.       }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclOtaAddinOptions;

{$I jcl.inc}

interface

uses
  Forms, ToolsAPI,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclOtaUtils;

type
  TJclEmptyPageAddinOptions = class(TInterfacedObject, INTAAddinOptions)
  private
    FCaption: string;
    FTitle: string;
  public
    constructor Create(const ACaption, ATitle: string);
    { INTAAddinOptions }
    procedure DialogClosed(Accepted: Boolean);
    procedure FrameCreated(AFrame: TCustomFrame);
    function GetArea: string;
    function GetCaption: string;
    function GetFrameClass: TCustomFrameClass;
    function ValidateContents: Boolean;
    function GetHelpContext: Integer;
    function IncludeInIDEInsight: Boolean;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclOtaResources, JclOtaEmptyAddinOptionsFrame;

//=== { TJclEmptyPageAddinOptions } ==========================================

constructor TJclEmptyPageAddinOptions.Create(const ACaption, ATitle: string);
begin
  inherited Create;
  FCaption := ACaption;
  FTitle := ATitle;
end;

procedure TJclEmptyPageAddinOptions.DialogClosed(Accepted: Boolean);
begin
//
end;

procedure TJclEmptyPageAddinOptions.FrameCreated(AFrame: TCustomFrame);
begin
  TJclOtaEmptyAddinOptionsFrm(AFrame).lbTitle.Caption := FTitle;
end;

function TJclEmptyPageAddinOptions.GetArea: string;
begin
  Result := '';
end;

function TJclEmptyPageAddinOptions.GetCaption: string;
begin
  Result := FCaption;
end;

function TJclEmptyPageAddinOptions.GetFrameClass: TCustomFrameClass;
begin
  Result := TJclOtaEmptyAddinOptionsFrm;
end;

function TJclEmptyPageAddinOptions.GetHelpContext: Integer;
begin
  Result := 0;
end;

function TJclEmptyPageAddinOptions.IncludeInIDEInsight: Boolean;
begin
  Result := False;
end;

function TJclEmptyPageAddinOptions.ValidateContents: Boolean;
begin
  Result := True;
end;

var
  ProjectJEDIEmptyAddinOptions: INTAAddinOptions = nil;
  ProjectJEDIJclEmptyAddinOptions: INTAAddinOptions = nil;
  ProjectJEDIJclCommonEmptyAddinOptions: INTAAddinOptions = nil;

procedure JclRegisterCommonAddinOptions;
begin
  if not Assigned(ProjectJEDIEmptyAddinOptions) then
  begin
    ProjectJEDIEmptyAddinOptions := TJclEmptyPageAddinOptions.Create(RsProjectJEDIAddinOptionsCaption,
      RsProjectJEDIAddinOptionsTitle);
    (BorlandIDEServices as INTAEnvironmentOptionsServices).RegisterAddInOptions(ProjectJEDIEmptyAddinOptions);
  end;
  if not Assigned(ProjectJEDIJclEmptyAddinOptions) then
  begin
    ProjectJEDIJclEmptyAddinOptions := TJclEmptyPageAddinOptions.Create(RsProjectJEDIJclAddinOptionsCaption,
       RsProjectJEDIJclAddinOptionsTitle);
    (BorlandIDEServices as INTAEnvironmentOptionsServices).RegisterAddInOptions(ProjectJEDIJclEmptyAddinOptions);
  end;
  if not Assigned(ProjectJEDIJclCommonEmptyAddinOptions) then
  begin
    ProjectJEDIJclCommonEmptyAddinOptions := TJclEmptyPageAddinOptions.Create(RsProjectJEDIJclCommonAddinOptionsCaption,
      RsProjectJEDIJclCommonAddinOptionsTitle);
    (BorlandIDEServices as INTAEnvironmentOptionsServices).RegisterAddInOptions(ProjectJEDIJclCommonEmptyAddinOptions);
  end;
end;

procedure JclUnregisterCommonAddinOptions;
begin
  if Assigned(ProjectJEDIEmptyAddinOptions) then
  begin
    (BorlandIDEServices as INTAEnvironmentOptionsServices).UnregisterAddInOptions(ProjectJEDIEmptyAddinOptions);
    ProjectJEDIEmptyAddinOptions := nil;
  end;
  if Assigned(ProjectJEDIJclEmptyAddinOptions) then
  begin
    (BorlandIDEServices as INTAEnvironmentOptionsServices).UnregisterAddInOptions(ProjectJEDIJclEmptyAddinOptions);
    ProjectJEDIJclEmptyAddinOptions := nil;
  end;
  if Assigned(ProjectJEDIJclCommonEmptyAddinOptions) then
  begin
    (BorlandIDEServices as INTAEnvironmentOptionsServices).UnregisterAddInOptions(ProjectJEDIJclCommonEmptyAddinOptions);
    ProjectJEDIJclCommonEmptyAddinOptions := nil;
  end;
end;

initialization

try
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  JclRegisterCommonAddinOptions;
except
  on ExceptionObj: TObject do
  begin
    JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

finalization

try
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  JclUnregisterCommonAddinOptions;
except
  on ExceptionObj: TObject do
  begin
    JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;


end.
