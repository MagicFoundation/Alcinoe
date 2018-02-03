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
{ The Original Code is JclDebugXMLSerializer.pas.                                                  }
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

unit JclDebugXMLSerializer;

{$I jcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF HAS_UNITSCOPE}
  System.SysUtils, System.Classes,
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils, Classes,
  {$ENDIF ~HAS_UNITSCOPE}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclDebugSerialization;

type
  TJclXMLSerializer = class(TJclCustomSimpleSerializer)
  public
    function SaveToString: string;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\windows';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

//=== { TJclXMLSerializer } ==================================================

function TJclXMLSerializer.SaveToString: string;

  procedure AddToStrings(ASerializer: TJclCustomSimpleSerializer; AXMLStrings: TStringList; AIdent: Integer);
  var
    I, P: Integer;
    S, S1, S2, V: string;
  begin
    if AIdent = 0 then
      S := ''
    else
      S := StringOfChar(' ', AIdent);
    V := '';
    for I := 0 to ASerializer.Values.Count - 1 do
    begin
      S1 := ASerializer.Values[I];
      P := Pos('=', S1);
      if P > 0 then
      begin
        S2 := S1;
        Delete(S1, P, Length(S1));
        Delete(S2, 1, P);
        V := V + ' ';
        V := V + Format('%s="%s"', [S1, S2]);
      end;
    end;
    if ASerializer.Count > 0 then
    begin
      AXMLStrings.Add(S + '<' + ASerializer.Name + V + '>');
      for I := 0 to ASerializer.Count - 1 do
        AddToStrings(ASerializer[I], AXMLStrings, AIdent + 2);
      AXMLStrings.Add(S + '</' + ASerializer.Name + '>');
    end
    else
      AXMLStrings.Add(S + '<' + ASerializer.Name + V + '/>');
  end;


var
  XMLStrings: TStringList;
begin
  XMLStrings := TStringList.Create;
  try
    AddToStrings(Self, XMLStrings, 0);
    Result := XMLStrings.Text;
  finally
    XMLStrings.Free;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
