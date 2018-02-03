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
{ The Original Code is JclStackTraceViewerConfigFrame.pas.                                         }
{                                                                                                  }
{ The Initial Developer of the Original Code is Uwe Schuster.                                      }
{ Portions created by Uwe Schuster are Copyright (C) 2009 Uwe Schuster. All rights reserved.       }
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

unit JclStackTraceViewerConfigFrame;

{$I jcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Dialogs, StdCtrls, ExtCtrls, JclStackTraceViewerOptions;

type
  TJclStackTraceViewerConfigFrame = class(TFrame)
    cbExpandTreeView: TCheckBox;
    cbModuleVersionAsRevision: TCheckBox;
  private
    FOptions: TExceptionViewerOption;
    function GetOptions: TExceptionViewerOption;
    procedure SetOptions(const Value: TExceptionViewerOption);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Options: TExceptionViewerOption read GetOptions write SetOptions;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\stacktraceviewer';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

{$R *.dfm}

uses
  JclOtaResources;

//=== { TJclStackTraceViewerConfigFrame } ====================================

constructor TJclStackTraceViewerConfigFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := TExceptionViewerOption.Create;
  cbExpandTreeView.Caption := LoadResString(@RsExpandTreeView);
  cbModuleVersionAsRevision.Caption := LoadResString(@RsModuleVersionAsRevision);
end;

destructor TJclStackTraceViewerConfigFrame.Destroy;
begin
  FOptions.Free;
  inherited Destroy;
end;

function TJclStackTraceViewerConfigFrame.GetOptions: TExceptionViewerOption;
begin
  Result := FOptions;
  FOptions.ExpandTreeView := cbExpandTreeView.Checked;
  FOptions.ModuleVersionAsRevision := cbModuleVersionAsRevision.Checked;
end;

procedure TJclStackTraceViewerConfigFrame.SetOptions(const Value: TExceptionViewerOption);
begin
  FOptions.Assign(Value);
  cbExpandTreeView.Checked := FOptions.ExpandTreeView;
  cbModuleVersionAsRevision.Checked := FOptions.ModuleVersionAsRevision;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
