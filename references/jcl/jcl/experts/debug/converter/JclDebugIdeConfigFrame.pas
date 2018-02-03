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
{ The Original Code is JclDebugIdeConfigFrame.pas.                                                 }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet.                                    }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet.                          }
{                                                                                                  }
{ Contributors:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                        $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclDebugIdeConfigFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Dialogs, StdCtrls, ExtCtrls;

type
  TDebugExpertState = (deAlwaysDisabled, deProjectDisabled, deProjectEnabled, deAlwaysEnabled);

  TJclDebugIdeConfigFrame = class(TFrame)
    RadioGroupGenerateJdbg: TRadioGroup;
    RadioGroupInsertJdbg: TRadioGroup;
    RadioGroupDeleteMapFile: TRadioGroup;
    CheckBoxQuiet: TCheckBox;
  private
    function GetGenerateJdbgState: TDebugExpertState;
    function GetInsertJdbgState: TDebugExpertState;
    function GetDeleteMapFileState: TDebugExpertState;
    procedure SetGenerateJdbgState(Value: TDebugExpertState);
    procedure SetInsertJdbgState(Value: TDebugExpertState);
    procedure SetDeleteMapFileState(Value: TDebugExpertState);
    function GetQuiet: Boolean;
    procedure SetQuiet(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;

    property GenerateJdbgState: TDebugExpertState read GetGenerateJdbgState write SetGenerateJdbgState;
    property InsertJdbgState: TDebugExpertState read GetInsertJdbgState write SetInsertJdbgState;
    property DeleteMapFileState: TDebugExpertState read GetDeleteMapFileState write SetDeleteMapFileState;
    property Quiet: Boolean read GetQuiet write SetQuiet;
  end;

function DebugExpertStateToInt(Value: TDebugExpertState): Integer;
function IntToDebugExpertState(Value: Integer): TDebugExpertState;
function ToggleDebugExpertState(Value: TDebugExpertState): TDebugExpertState;
function EnableDebugExpertState(Value: TDebugExpertState): TDebugExpertState;
function DisableDebugExpertState(Value: TDebugExpertState): TDebugExpertState;
function ApplyDebugExpertState(GlobalState: TDebugExpertState; LocalEnabled: Boolean): TDebugExpertState;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\debug\converter';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

{$R *.dfm}

uses
  JclOtaResources;

function DebugExpertStateToInt(Value: TDebugExpertState): Integer;
begin
  case Value of
    deAlwaysDisabled:
      Result := 0;
    deProjectDisabled:
      Result := 1;
    deProjectEnabled:
      Result := 2;
    deAlwaysEnabled:
      Result := 3;
  else
    raise EConvertError.CreateResFmt(@RsEInvalidDebugExpertState, [Integer(Value)]);
  end;
end;

function IntToDebugExpertState(Value: Integer): TDebugExpertState;
begin
  case Value of
    0:
      Result := deAlwaysDisabled;
    1:
      Result := deProjectDisabled;
    2:
      Result := deProjectEnabled;
    3:
      Result := deAlwaysEnabled;
  else
    raise EConvertError.CreateResFmt(@RsEInvalidDebugExpertState, [Value]);
  end;
end;

function ToggleDebugExpertState(Value: TDebugExpertState): TDebugExpertState;
begin
  case Value of
    deAlwaysDisabled:
      Result := deAlwaysEnabled;
    deProjectDisabled:
      Result := deProjectEnabled;
    deProjectEnabled:
      Result := deProjectDisabled;
    deAlwaysEnabled:
      Result := deAlwaysDisabled;
  else
    raise EConvertError.CreateResFmt(@RsEInvalidDebugExpertState, [Integer(Value)]);
  end;
end;

function EnableDebugExpertState(Value: TDebugExpertState): TDebugExpertState;
begin
  case Value of
    deAlwaysDisabled:
      Result := deAlwaysEnabled;
    deProjectDisabled:
      Result := deProjectEnabled;
    deProjectEnabled,
    deAlwaysEnabled:
      Result := Value;
  else
    raise EConvertError.CreateResFmt(@RsEInvalidDebugExpertState, [Integer(Value)]);
  end;
end;

function DisableDebugExpertState(Value: TDebugExpertState): TDebugExpertState;
begin
  case Value of
    deAlwaysDisabled,
    deProjectDisabled:
      Result := Value;
    deProjectEnabled:
      Result := deProjectDisabled;
    deAlwaysEnabled:
      Result := deAlwaysDisabled;
  else
    raise EConvertError.CreateResFmt(@RsEInvalidDebugExpertState, [Integer(Value)]);
  end;
end;

function ApplyDebugExpertState(GlobalState: TDebugExpertState; LocalEnabled: Boolean): TDebugExpertState;
begin
  case GlobalState of
    deAlwaysDisabled:
      Result := deAlwaysDisabled;
    deProjectDisabled,
    deProjectEnabled:
      if LocalEnabled then
        Result := deProjectEnabled
      else
        Result := deProjectDisabled;
    deAlwaysEnabled:
      Result := deAlwaysEnabled;
  else
    raise EConvertError.CreateResFmt(@RsEInvalidDebugExpertState, [Integer(GlobalState)]);
  end;
end;

//=== { TJclDebugIdeConfigFrame } ============================================

constructor TJclDebugIdeConfigFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  RadioGroupGenerateJdbg.Caption := LoadResString(@RsDebugGenerateJdbg);
  RadioGroupGenerateJdbg.Items.Strings[0] := LoadResString(@RsAlwaysDisabled);
  RadioGroupGenerateJdbg.Items.Strings[1] := LoadResString(@RsDefaultDisabled);
  RadioGroupGenerateJdbg.Items.Strings[2] := LoadResString(@RsDefaultEnabled);
  RadioGroupGenerateJdbg.Items.Strings[3] := LoadResString(@RsAlwaysEnabled);

  RadioGroupInsertJdbg.Caption := LoadResString(@RsDebugInsertJdbg);
  RadioGroupInsertJdbg.Items.Strings[0] := LoadResString(@RsAlwaysDisabled);
  RadioGroupInsertJdbg.Items.Strings[1] := LoadResString(@RsDefaultDisabled);
  RadioGroupInsertJdbg.Items.Strings[2] := LoadResString(@RsDefaultEnabled);
  RadioGroupInsertJdbg.Items.Strings[3] := LoadResString(@RsAlwaysEnabled);

  RadioGroupDeleteMapFile.Caption := LoadResString(@RsDeleteMapFile);
  RadioGroupDeleteMapFile.Items.Strings[0] := LoadResString(@RsAlwaysDisabled);
  RadioGroupDeleteMapFile.Items.Strings[1] := LoadResString(@RsDefaultDisabled);
  RadioGroupDeleteMapFile.Items.Strings[2] := LoadResString(@RsDefaultEnabled);
  RadioGroupDeleteMapFile.Items.Strings[3] := LoadResString(@RsAlwaysEnabled);

  CheckBoxQuiet.Caption := LoadResString(@RsQuiet);
end;

function TJclDebugIdeConfigFrame.GetGenerateJdbgState: TDebugExpertState;
begin
  Result := IntToDebugExpertState(RadioGroupGenerateJdbg.ItemIndex);
end;

function TJclDebugIdeConfigFrame.GetInsertJdbgState: TDebugExpertState;
begin
  Result := IntToDebugExpertState(RadioGroupInsertJdbg.ItemIndex);
end;

function TJclDebugIdeConfigFrame.GetDeleteMapFileState: TDebugExpertState;
begin
  Result := IntToDebugExpertState(RadioGroupDeleteMapFile.ItemIndex);
end;

function TJclDebugIdeConfigFrame.GetQuiet: Boolean;
begin
  Result := CheckBoxQuiet.Checked;
end;

procedure TJclDebugIdeConfigFrame.SetGenerateJdbgState(Value: TDebugExpertState);
begin
  RadioGroupGenerateJdbg.ItemIndex := DebugExpertStateToInt(Value);
end;

procedure TJclDebugIdeConfigFrame.SetInsertJdbgState(Value: TDebugExpertState);
begin
  RadioGroupInsertJdbg.ItemIndex := DebugExpertStateToInt(Value);
end;

procedure TJclDebugIdeConfigFrame.SetDeleteMapFileState(Value: TDebugExpertState);
begin
  RadioGroupDeleteMapFile.ItemIndex := DebugExpertStateToInt(Value);
end;

procedure TJclDebugIdeConfigFrame.SetQuiet(const Value: Boolean);
begin
  CheckBoxQuiet.Checked := Value;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
