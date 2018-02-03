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
{ The Original Code is JclOtaExcDlgFormFrame.pas.                                                  }
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

unit JclOtaExcDlgFormFrame;

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
  TJclOtaExcDlgFormPage = class(TJclWizardFrame)
    CheckBoxMail: TCheckBox;
    LabelEMailAddress: TLabel;
    EditEMail: TEdit;
    CheckBoxModalDialog: TCheckBox;
    CheckBoxSizeable: TCheckBox;
    EditSubject: TEdit;
    LabelSubject: TLabel;
    CheckBoxAutoScrollBars: TCheckBox;
    procedure CheckBoxMailClick(Sender: TObject);
  private
    FParams: TJclExcDlgParams;
    procedure UpdateMailEdits;
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

//=== { TJclOtaExcDlgFormPage } ==============================================

procedure TJclOtaExcDlgFormPage.CheckBoxMailClick(Sender: TObject);
begin
  UpdateMailEdits;
end;

constructor TJclOtaExcDlgFormPage.Create(AOwner: TComponent;
  AParams: TJclExcDlgParams);
begin
  FParams := AParams;
  inherited Create(AOwner);

  Caption := LoadResString(@RsExcDlgFormOptions);
  CheckBoxMail.Caption := LoadResString(@RsDialogWithMailButton);
  LabelEMailAddress.Caption := LoadResString(@RsEMail);
  LabelSubject.Caption := LoadResString(@RsSubject);
  CheckBoxModalDialog.Caption := LoadResString(@RsModalDialog);
  CheckBoxSizeable.Caption := LoadResString(@RsSizeableDialog);
  CheckBoxAutoScrollBars.Caption := LoadResString(@RsAutoScrollBars);
end;

function TJclOtaExcDlgFormPage.GetSupportsNext: Boolean;
begin
  Result := (not CheckBoxMail.Checked) or ((EditEMail.Text <> '') and (EditSubject.Text <> ''));
end;

procedure TJclOtaExcDlgFormPage.PageActivated(Direction: TJclWizardDirection);
begin
  inherited PageActivated(Direction);

  CheckBoxMail.Checked := Params.SendEMail;
  EditEMail.Text := Params.EMailAddress;
  EditSubject.Text := Params.EMailSubject;
  CheckBoxModalDialog.Checked := Params.ModalDialog;
  CheckBoxSizeable.Checked := Params.SizeableDialog;
  CheckBoxAutoScrollBars.Checked := Params.AutoScrollBars;

  UpdateMailEdits;
end;

procedure TJclOtaExcDlgFormPage.PageDesactivated(
  Direction: TJclWizardDirection);
begin
  inherited PageDesactivated(Direction);

  Params.SendEMail := CheckBoxMail.Checked;
  Params.EMailAddress := EditEMail.Text;
  Params.EMailSubject := EditSubject.Text;
  Params.ModalDialog := CheckBoxModalDialog.Checked;
  Params.SizeableDialog := CheckBoxSizeable.Checked;
  Params.AutoScrollBars := CheckBoxAutoScrollBars.Checked;
end;

procedure TJclOtaExcDlgFormPage.UpdateMailEdits;
begin
  if CheckBoxMail.Checked then
  begin
    EditEMail.Enabled := True;
    EditSubject.Enabled := True;
    EditEMail.Color := clWindow;
    EditSubject.Color := clWindow;
  end
  else
  begin
    EditEMail.Enabled := False;
    EditSubject.Enabled := False;
    EditEMail.ParentColor := True;
    EditSubject.ParentColor := True;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
