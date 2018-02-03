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
{ The Original Code is PeViewerControl.pas.                                                        }
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

unit PeViewerControl;

{$I JCL.INC}

interface

uses
  ComObj, ActiveX, PeViewer_TLB, Forms, Windows, StdVcl;

type
  TPeViewerControl = class(TAutoObject, IPeViewerControl)
  private
    FROTHandle: Integer;
  protected
    procedure OpenFile(const FileName: WideString); safecall;
    procedure BringToFront; safecall;
    { Protected declarations }
  public
    destructor Destroy; override;
    procedure Initialize; override;
  end;

implementation

uses ComServ, PeViewerMain;

procedure TPeViewerControl.OpenFile(const FileName: WideString);
begin
  if Length(FileName) > 0 then MainForm.OpenFile(FileName, True);
end;

procedure TPeViewerControl.BringToFront;
begin
  Application.Restore;
  SetForegroundWindow(Application.Handle);
end;

procedure TPeViewerControl.Initialize;
begin
  inherited;
  OleCheck(RegisterActiveObject(Self as IUnknown, Class_PeViewerControl,
    ACTIVEOBJECT_WEAK, FROTHandle));
  {$IFDEF COMPILER5_UP}
  ComServer.UIInteractive := False;
  {$ENDIF}
end;

destructor TPeViewerControl.Destroy;
begin
  OleCheck(RevokeActiveObject(FROTHandle, nil));
  inherited;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TPeViewerControl, Class_PeViewerControl,
    ciMultiInstance, tmApartment);

end.
