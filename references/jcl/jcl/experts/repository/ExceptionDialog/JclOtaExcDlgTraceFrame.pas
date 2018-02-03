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
{ The Original Code is JclOtaExcDlgTraceFrame.pas.                                                 }
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

unit JclOtaExcDlgTraceFrame;

interface

{$I jcl.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JclDebug,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclPreProcessorExcDlgTemplates, JclOtaWizardFrame;

type
  TJclOtaExcDlgTracePage = class(TJclWizardFrame)
    CheckBoxRawData: TCheckBox;
    CheckBoxModuleName: TCheckBox;
    CheckBoxCodeDetails: TCheckBox;
    CheckBoxVirtualAddress: TCheckBox;
    CheckBoxModuleOffset: TCheckBox;
    MemoStack: TMemo;
    LabelPreview: TLabel;
    CheckBoxStackList: TCheckBox;
    procedure CheckBoxClick(Sender: TObject);
    procedure CheckBoxStackListClick(Sender: TObject);
  private
    FParams: TJclExcDlgParams;
    procedure UpdatePreview;
    procedure UpdateCheckBoxes;
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

//=== { TJclOtaExcDlgTracePage } =============================================

procedure TJclOtaExcDlgTracePage.CheckBoxClick(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TJclOtaExcDlgTracePage.CheckBoxStackListClick(Sender: TObject);
begin
  UpdateCheckBoxes;
end;

constructor TJclOtaExcDlgTracePage.Create(AOwner: TComponent;
  AParams: TJclExcDlgParams);
begin
  FParams := AParams;
  inherited Create(AOwner);

  Caption := LoadResString(@RsExcDlgTraceOptions);
  CheckBoxStackList.Caption := LoadResString(@RsStackList);
  CheckBoxRawData.Caption := LoadResString(@RsRawData);
  CheckBoxModuleName.Caption := LoadResString(@RsModuleName);
//  CheckBoxAddressOffset.Caption := LoadResString(@RsAddressOffset);
  CheckBoxCodeDetails.Caption := LoadResString(@RsCodeDetails);
  CheckBoxVirtualAddress.Caption := LoadResString(@RsVirtualAddress);
  CheckBoxModuleOffset.Caption := LoadResString(@RsModuleOffset);
  LabelPreview.Caption := LoadResString(@RsPreview);
end;

procedure TJclOtaExcDlgTracePage.PageActivated(Direction: TJclWizardDirection);
begin
  inherited PageActivated(Direction);

  CheckBoxStackList.Checked := Params.StackList;
  CheckBoxRawData.Checked := Params.RawData;
  CheckBoxModuleName.Checked := Params.ModuleName;
//  CheckBoxAddressOffset.Checked := Params.AddressOffset;
  CheckBoxCodeDetails.Checked := Params.CodeDetails;
  CheckBoxVirtualAddress.Checked := Params.VirtualAddress;
  CheckBoxModuleOffset.Checked := Params.ModuleOffset;

  UpdateCheckBoxes;
end;

procedure TJclOtaExcDlgTracePage.PageDesactivated(
  Direction: TJclWizardDirection);
begin
  inherited PageDesactivated(Direction);

  Params.StackList := CheckBoxStackList.Checked;
  Params.RawData := CheckBoxRawData.Checked;
  Params.ModuleName := CheckBoxModuleName.Checked;
//  Params.AddressOffset := CheckBoxAddressOffset.Checked;
  Params.CodeDetails := CheckBoxCodeDetails.Checked;
  Params.VirtualAddress := CheckBoxVirtualAddress.Checked;
  Params.ModuleOffset := CheckBoxModuleOffset.Checked;
end;

procedure TJclOtaExcDlgTracePage.UpdateCheckBoxes;
var
  AEnabled: Boolean;
begin
  AEnabled := CheckBoxStackList.Enabled;

  CheckBoxRawData.Enabled := AEnabled;
  CheckBoxModuleName.Enabled := AEnabled;
  CheckBoxCodeDetails.Enabled := AEnabled;
  CheckBoxVirtualAddress.Enabled := AEnabled;
  CheckBoxModuleOffset.Enabled := AEnabled;
end;

procedure TJclOtaExcDlgTracePage.UpdatePreview;
var
  AStack: TJclStackInfoList;
begin
  MemoStack.Lines.Clear;
  
  AStack := TJclStackInfoList.Create(CheckBoxRawData.Checked, 0, nil, False);
  try
    AStack.AddToStrings(MemoStack.Lines, CheckBoxModuleName.Checked,
      CheckBoxModuleOffset.Checked, CheckBoxCodeDetails.Checked, CheckBoxVirtualAddress.Checked);
  finally
    AStack.Free;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
