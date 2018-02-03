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
{ The Original Code is JclFont.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Fabien Connault.                              }
{ Portions created by these individuals are Copyright (C) of these individuals.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains function to initialize TFont objects from standard font styles.               }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclFont;

{$I jcl.inc}

interface

type
  TFontType  = (ftAuto, ftCaption, ftContent);

procedure SetObjectFontToSystemFont(const AObject: TObject; const FontType: TFontType = ftAuto);

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  WinApi.Windows, System.SysUtils, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Graphics, System.TypInfo,
  {$ELSE ~HAS_UNITSCOPE}
  Windows, SysUtils, StdCtrls, ComCtrls, Graphics, TypInfo,
  {$ENDIF ~HAS_UNITSCOPE}
  JclSysUtils, JclSysInfo;

procedure SetCaptionFont(const AObjectFont: TFont);
begin
  if JclCheckWinVersion(6, 0) then // WinVista or newer
  begin
    AObjectFont.Name := 'Segoe UI';
    AObjectFont.Size := 9;
  end
  else if JclCheckWinVersion(5, 0) then // Win2k or newer
  begin
     AObjectFont.Name := 'Tahoma';
     AObjectFont.Size := 8;
  end
  else // Win95..WinME/NT4
  begin
    AObjectFont.Name := 'MS Sans Serif';
    AObjectFont.Size := 8;
  end;
end;

procedure SetContentFont(const AObjectFont: TFont);
begin
  if JclCheckWinVersion(6, 0) then // WinVista or newer
  begin
    AObjectFont.Name := 'Calibri';
    AObjectFont.Size := 9;
  end
  else if JclCheckWinVersion(5, 0) then // Win2k or newer
  begin
    AObjectFont.Name := 'Verdana';
    AObjectFont.Size := 8;
  end
  else // Win95..WinME/NT4
  begin
    AObjectFont.Name := 'MS Sans Serif';
    AObjectFont.Size := 8;
  end;
end;

procedure SetObjectFontToSystemFont(const AObject: TObject; const FontType: TFontType);
var
  AObjectFont: TFont;
  AFontType:   TFontType;
begin
  if AObject.ClassType = TFont then
    AObjectFont := TFont(AObject)
  else
    AObjectFont := TFont(GetObjectProp(AObject, 'Font', TFont));

  if FontType = ftAuto then
  begin
    if (AObject.ClassType = TMemo) {$IFDEF BORLAND}or (AObject.ClassType = TRichEdit){$ENDIF} then
      AFontType := ftContent
    else
      AFontType := ftCaption;
  end
  else
    AFontType := FontType;

  if AFontType = ftCaption then
    SetCaptionFont(AObjectFont)
  else if AFontType = ftContent then
    SetContentFont(AObjectFont);
end;

end.
