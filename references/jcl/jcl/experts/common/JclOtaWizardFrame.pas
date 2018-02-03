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
{ The Original Code is JclOtaWizardFrame.pas.                                                      }
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

unit JclOtaWizardFrame;

interface

{$I jcl.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Dialogs;

type
  TJclWizardDirection = (wdForward, wdBackward);
  TJclWizardFrame = class(TFrame)
  protected
    function GetSupportsFinish: Boolean; virtual;
    function GetSupportsNext: Boolean; virtual;
    function GetSupportsPrevious: Boolean; virtual;
  public
    procedure PageActivated(Direction: TJclWizardDirection); virtual;
    procedure PageDesactivated(Direction: TJclWizardDirection); virtual;
    property SupportsNext: Boolean read GetSupportsNext;
    property SupportsPrevious: Boolean read GetSupportsPrevious;
    property SupportsFinish: Boolean read GetSupportsFinish;
    property Caption;
  end;

  TJclWizardFrameClass = class of TJclWizardFrame;

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

{$R *.dfm}

//=== { TJclWizardFrame } ====================================================

function TJclWizardFrame.GetSupportsFinish: Boolean;
begin
  // override to customize
  Result := SupportsNext;
end;

function TJclWizardFrame.GetSupportsNext: Boolean;
begin
  // override to customize
  Result := True;
end;

function TJclWizardFrame.GetSupportsPrevious: Boolean;
begin
  // override to customize
  Result := True;
end;

procedure TJclWizardFrame.PageActivated(Direction: TJclWizardDirection);
begin
  // override to customize
end;

procedure TJclWizardFrame.PageDesactivated(Direction: TJclWizardDirection);
begin
  // override to customize
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
