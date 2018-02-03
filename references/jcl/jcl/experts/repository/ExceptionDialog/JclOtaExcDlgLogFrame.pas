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
{ The Original Code is JclOtaExcDlgLogFrame.pas.                                                   }
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

unit JclOtaExcDlgLogFrame;

interface

{$I jcl.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclPreProcessorExcDlgTemplates, JclOtaWizardFrame;

type
  TJclOtaExcDlgLogPage = class(TJclWizardFrame)
    CheckBoxLogFile: TCheckBox;
    LabelLogFileName: TLabel;
    EditLogFileName: TEdit;
    CheckBoxLogInWorkingDirectory: TCheckBox;
    CheckBoxLogInApplicationDirectory: TCheckBox;
    CheckBoxLogInDesktopDirectory: TCheckBox;
    CheckBoxSaveDialog: TCheckBox;
    procedure CheckBoxLogFileClick(Sender: TObject);
  private
    FParams: TJclExcDlgParams;
    procedure UpdateLogControls;
  protected
    function GetSupportsNext: Boolean; override;
  public
    constructor Create(AOwner: TComponent; AParams: TJclExcDlgParams); reintroduce;

    procedure PageActivated(Direction: TJclWizardDirection); override;
    procedure PageDesactivated(Direction: TJclWizardDirection); override;

    property Params: TJclExcDlgParams read FParams write FParams;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\repository\ExceptionDialog';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

{$R *.dfm}

uses
  JclOtaResources;

//=== { TJclOtaExcDlgLogPage } ===============================================

procedure TJclOtaExcDlgLogPage.CheckBoxLogFileClick(Sender: TObject);
begin
  UpdateLogControls;
end;

constructor TJclOtaExcDlgLogPage.Create(AOwner: TComponent;
  AParams: TJclExcDlgParams);
begin
  FParams := AParams;
  inherited Create(AOwner);

  Caption := LoadResString(@RsExcDlgLogOptions);
  CheckBoxLogFile.Caption := LoadResString(@RsLogTrace);
  LabelLogFileName.Caption := LoadResString(@RsLogFileName);
  CheckBoxLogInWorkingDirectory.Caption := LoadResString(@RsLogInWorkingDirectory);
  CheckBoxLogInApplicationDirectory.Caption := LoadResString(@RsLogInApplicationDirectory);
  CheckBoxLogInDesktopDirectory.Caption := LoadResString(@RsLogInDesktopDirectory);
  CheckBoxSaveDialog.Caption := LoadResString(@RsLogSaveDialog);
end;

function TJclOtaExcDlgLogPage.GetSupportsNext: Boolean;
begin
  Result := (not CheckBoxLogFile.Checked) or (EditLogFileName.Text <> '');
end;

procedure TJclOtaExcDlgLogPage.PageActivated(Direction: TJclWizardDirection);
begin
  inherited PageActivated(Direction);

  CheckBoxLogFile.Checked := Params.LogFile;
  EditLogFileName.Text := Params.LogFileName;
  CheckBoxLogInWorkingDirectory.Checked := Params.AutoSaveWorkingDirectory;
  CheckBoxLogInApplicationDirectory.Checked := Params.AutoSaveApplicationDirectory;
  CheckBoxLogInDesktopDirectory.Checked := Params.AutoSaveDesktopDirectory;
  CheckBoxSaveDialog.Checked := Params.LogSaveDialog;

  UpdateLogControls;
end;

procedure TJclOtaExcDlgLogPage.PageDesactivated(
  Direction: TJclWizardDirection);
begin
  inherited PageDesactivated(Direction);

  Params.LogFile := CheckBoxLogFile.Checked;
  Params.LogFileName := EditLogFileName.Text;
  Params.AutoSaveWorkingDirectory := CheckBoxLogInWorkingDirectory.Checked;
  Params.AutoSaveApplicationDirectory := CheckBoxLogInApplicationDirectory.Checked;
  Params.AutoSaveDesktopDirectory := CheckBoxLogInDesktopDirectory.Checked;
  Params.LogSaveDialog := CheckBoxSaveDialog.Checked;
end;

procedure TJclOtaExcDlgLogPage.UpdateLogControls;
var
  AEnabled: Boolean;
begin
  AEnabled := CheckBoxLogFile.Checked;
  EditLogFileName.Enabled := AEnabled;
  if AEnabled then
    EditLogFileName.Color := clWindow
  else
    EditLogFileName.ParentColor := True;
  CheckBoxLogInWorkingDirectory.Enabled := AEnabled;
  CheckBoxLogInApplicationDirectory.Enabled := AEnabled;
  CheckBoxLogInDesktopDirectory.Enabled := AEnabled;
  CheckBoxSaveDialog.Enabled := AEnabled;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
