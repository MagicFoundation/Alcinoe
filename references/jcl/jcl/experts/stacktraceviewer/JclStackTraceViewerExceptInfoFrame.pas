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
{ The Original Code is JclStackTraceViewerExceptInfoFrame.pas.                                     }
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

unit JclStackTraceViewerExceptInfoFrame;

{$I jcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclDebugSerialization;

type
  TfrmException = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    lbExceptionClassName: TLabel;
    lbExceptionMessage: TLabel;
  private
    FException: TJclSerializableException;
    procedure SetException(const Value: TJclSerializableException);
  public
    constructor Create(AOwner: TComponent); override;
    property Exception: TJclSerializableException read FException write SetException;
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

{ TfrmException }

constructor TfrmException.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Label1.Caption := LoadResString(@RsExceptionClassName);
  Label2.Caption := LoadResString(@RsExceptionMessage);
end;

procedure TfrmException.SetException(const Value: TJclSerializableException);
begin
  FException := Value;
  if Assigned(FException) then
  begin
    lbExceptionClassName.Caption := FException.ExceptionClassName;
    lbExceptionMessage.Caption := FException.ExceptionMessage;
  end
  else
  begin
    lbExceptionClassName.Caption := '';
    lbExceptionMessage.Caption := '';
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
