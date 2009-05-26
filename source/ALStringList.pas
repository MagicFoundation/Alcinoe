{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALStringList
Version:      3.50

Description:  TALStringList in inherited from Delphi TstringList.
              It's allow to search a name=value using a quicksort
              algorithm when the list is sorted.

Legal issues: Copyright (C) 1999-2009 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :

History :     27/10/2007 add ForceValues and ForceValueFromIndex
                         that do not delete an entry when we do
                         ForceValue[name] := ''   

Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALStringList;

interface

Uses Classes,
     sysutils;

Type

  {--------------------------------}
  TALStringList = class(TStringList)
  private
    function GetForceValue(const Name: string): string;
    function GetForceValueFromIndex(Index: Integer): string;
    procedure SetForceValue(const Name, Value: string);
    procedure SetForceValueFromIndex(Index: Integer; const Value: string);
  protected
    procedure Put(Index: Integer; const S: string); override;
    function CompareStrings(const S1, S2: string): Integer; override;
  public
    function FindName(const S: string; var Index: Integer): Boolean; virtual;
    function IndexOfName(const Name: string): Integer; override;
    property ForceValues[const Name: string]: string read GetForceValue write SetForceValue;
    property ForceValueFromIndex[Index: Integer]: string read GetForceValueFromIndex write SetForceValueFromIndex;
  end;

implementation

Uses AlFcnString;

{*******************************************************************}
function TALStringList.CompareStrings(const S1, S2: string): Integer;
begin
  if CaseSensitive then Result := AnsiCompareStr(
                                                 AlStringReplace(S1,'=',' ',[]),
                                                 AlStringReplace(S2,'=',' ',[])
                                                )
  else Result := AnsiCompareText(
                                 AlStringReplace(S1,'=',' ',[]),
                                 AlStringReplace(S2,'=',' ',[])
                                );
end;

{****************************************************************************}
function TALStringList.FindName(const S: string; var Index: Integer): Boolean;

  {----------------------------------------------------}
  function InternalExtractName(const S: string): string;
  var P: Integer;
  begin
    Result := S;
    P := AlCharPos(NameValueSeparator, Result);
    if P <> 0 then SetLength(Result, P-1);
  end;


var L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := CompareStrings(InternalExtractName(Get(I)), S);
    if C < 0 then L := I + 1
    else begin
      H := I - 1;
      if C = 0 then Result := True;
    end;
  end;
  Index := L;
end;

{**************************************************************}
function TALStringList.IndexOfName(const Name: string): Integer;
begin
  if not Sorted then Result := inherited IndexOfName(Name)
  else if not FindName(Name, Result) then Result := -1;
end;

{***********************************************************}
procedure TALStringList.Put(Index: Integer; const S: string);
begin
  If not sorted then inherited Put(Index, S)
  else begin
    delete(index);
    add(s);
  end;
end;

{***************************************************************}
function TALStringList.GetForceValue(const Name: string): string;
begin
  Result := Values[Name];
end;

{********************************************************************}
function TALStringList.GetForceValueFromIndex(Index: Integer): string;
begin
  Result := ValueFromIndex[Index];
end;

{***************************************************************}
procedure TALStringList.SetForceValue(const Name, Value: string);
var I: Integer;
begin
  I := IndexOfName(Name);
  if I < 0 then I := Add('');
  Put(I, Name + NameValueSeparator + Value);
end;

{**********************************************************************************}
procedure TALStringList.SetForceValueFromIndex(Index: Integer; const Value: string);
begin
  if Index < 0 then Index := Add('');
  Put(Index, Names[Index] + NameValueSeparator + Value);
end;

end.
