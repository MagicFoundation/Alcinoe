{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) - Delphi Tools                                                   }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is D6MdiMsgFix.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date$                                                      } 
{                                                                                                  }
{**************************************************************************************************}

unit D6MdiMsgFix;

interface

{$I jcl.inc}

implementation

{$IFDEF DELPHI6}

uses
  Windows, Classes, SysUtils, Forms, AppEvnts;

type
  TFixApplicationEvents = class(TCustomApplicationEvents)
  protected
    procedure ApplicationEventsMessage(var Msg: TMsg; var Handled: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TApplicationAccess = class(TApplication);

var
  FixApplicationEvents: TFixApplicationEvents;

{ TFixApplicationEvents }

procedure TFixApplicationEvents.ApplicationEventsMessage(var Msg: TMsg; var Handled: Boolean);
begin
  with Application do
    if Assigned(MainForm) and (MainForm.FormStyle = fsMDIForm) and
      Assigned(Screen.ActiveForm) and (Screen.ActiveForm.FormStyle <> fsMdiChild) then
      begin
        Handled := True;
        with TApplicationAccess(Application) do
          if not IsKeyMsg(Msg) and not IsDlgMsg(Msg) then
          begin
            // prevent to call buggy TApplication.IsMDIMsg method, handle message here
            TranslateMessage(Msg);
            DispatchMessage(Msg);
          end;
      end;
end;

constructor TFixApplicationEvents.Create(AOwner: TComponent);
begin
  inherited;
  OnMessage := ApplicationEventsMessage;
end;

initialization
  FixApplicationEvents := TFixApplicationEvents.Create(nil);

finalization
  FreeAndNil(FixApplicationEvents);

{$ENDIF DELPHI6}

end.
