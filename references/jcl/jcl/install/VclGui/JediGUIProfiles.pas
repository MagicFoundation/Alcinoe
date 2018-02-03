{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) extension                                                        }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JediGUIProfiles.pas.                                                        }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet. Portions created by Florent Ouchet }
{ are Copyright (C) of Florent Ouchet. All Rights Reserved.                                        }
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

unit JediGUIProfiles;

{$I jcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, 
  Dialogs, JediInstall, StdCtrls, ComCtrls;

type
  TProfilesFrame = class(TFrame, IJediProfilesPage, IJediPage)
    MemoComment: TMemo;
  public
    constructor Create(AOwner: TComponent); override;
    // IJediPage
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function GetHintAtPos(ScreenX, ScreenY: Integer): string;
    // IJediProfilesPage
    function GetProfileEnabled(Index: Integer): Boolean;
    procedure SetProfileEnabled(Index: Integer; Value: Boolean);
  end;

implementation

{$R *.dfm}

uses
  JediInstallResources;

//=== { TProfilesFrame } =====================================================

constructor TProfilesFrame.Create(AOwner: TComponent);
var
  Index: Integer;
  ACheckBox: TCheckBox;
  AProfilesManager: IJediProfilesManager;
begin
  inherited Create(AOwner);
  MemoComment.Lines.Text := LoadResString(@RsGUIProfiles);
  MemoComment.WordWrap := True;
  AProfilesManager := InstallCore.ProfilesManager;
  for Index := 0 to AProfilesManager.ProfileCount - 1 do
  begin
    ACheckBox := TCheckBox.Create(Self);
    ACheckBox.SetBounds(48, Index * 32 + 100, Width - 96, ACheckBox.Height);
    ACheckBox.Anchors := [akLeft, akTop, akRight];
    ACheckBox.Parent := Self;
    ACheckBox.Checked := True;
    ACheckBox.Caption := AProfilesManager.ProfileNames[Index];
  end;
end;

function TProfilesFrame.GetCaption: string;
begin
  Result := (Parent as TTabSheet).Caption;
end;

function TProfilesFrame.GetHintAtPos(ScreenX, ScreenY: Integer): string;
begin
  Result := '';
end;

function TProfilesFrame.GetProfileEnabled(Index: Integer): Boolean;
begin
  Result := (Controls[Index + 1] as TCheckBox).Checked;
end;

procedure TProfilesFrame.SetCaption(const Value: string);
begin
  (Parent as TTabSheet).Caption := Value;
end;

procedure TProfilesFrame.SetProfileEnabled(Index: Integer; Value: Boolean);
begin
  (Controls[Index + 1] as TCheckBox).Checked := Value;
end;

end.
