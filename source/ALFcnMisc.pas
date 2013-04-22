{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          https://alcinoe.svn.sourceforge.net/svnroot/alcinoe              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      Alcinoe Misc functions
Version:      4.00

Description:  Alcinoe Misc Functions

Legal issues: Copyright (C) 1999-2013 by Arkadia Software Engineering

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

History :     09/01/2005: correct then AlEmptyDirectory function
              25/05/2006: Move some function to AlFcnFile
              25/02/2008: Update AlIsValidEmail
              06/10/3008: Update AlIsValidEmail
              03/03/2010: add ALIsInt64
              26/06/2012: Add xe2 support
Link :

* Please send all your feedback to svanderclock@arkadia.com
* If you have downloaded this source from a website different from 
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALFcnMisc;

interface

Function AlBoolToInt(Value:Boolean):Integer;
Function ALMediumPos(LTotal, LBorder, LObject : integer):Integer;
function AlIsValidEmail(const Value: AnsiString): boolean;
function AlLocalDateTimeToGMTDateTime(Const aLocalDateTime: TDateTime): TdateTime;
Function ALInc(var x: integer; Count: integer): Integer;

implementation

uses Windows,
     sysutils,
     ALFcnString;

{******************************************}
Function AlBoolToInt(Value:Boolean):Integer;
Begin
  If Value then result := 1
  else result := 0;
end;

{***************************************************************}
Function ALMediumPos(LTotal, LBorder, LObject : integer):Integer;
Begin
  result := (LTotal - (LBorder*2) - LObject) div 2 + LBorder;
End;

{********************************************************}
function AlIsValidEmail(const Value: AnsiString): boolean;

 {------------------------------------------------------}
 function CheckAllowedName(const s: AnsiString): boolean;
 var i: integer;
 begin
   Result:= false;
   for i:= 1 to Length(s) do begin
     // illegal char in s -> no valid address
     if not (s[i] in ['a'..'z','A'..'Z','0'..'9','_','-','.','+']) then Exit;
   end;
   Result:= true;
 end;

 {----------------------------------------------------------}
 function CheckAllowedHostname(const s: AnsiString): boolean;
 var i: integer;
 begin
   Result:= false;
   for i:= 1 to Length(s) do begin
     // illegal char in s -> no valid address
     if not (s[i] in ['a'..'z','A'..'Z','0'..'9','-','.']) then Exit;
   end;
   Result:= true;
 end;

 {-----------------------------------------------------}
 function CheckAllowedExt(const s: AnsiString): boolean;
 var i: integer;
 begin
   Result:= false;
   for i:= 1 to Length(s) do begin
     // illegal char in s -> no valid address
     if not (s[i] in ['a'..'z','A'..'Z']) then Exit;
   end;
   Result:= true;
 end;

var i, j: integer;
    namePart, serverPart, extPart: AnsiString;
begin
  Result := false;

  // Value can not be < 6 char (ex: a@b.fr)
  if length(Value) < 6 then exit;

  // must have the '@' char inside
  i := AlPos('@', Value);
  if (i <= 1) or (i > length(Value)-4) then exit;

  //can not have @. or .@
  if (value[i-1] = '.') or (value[i+1] = '.') then exit;

  //can not have 2 ..
  If (ALpos('..', Value) > 0) then Exit;

  //extract namePart and serverPart
  namePart:= AlCopyStr(Value, 1, i - 1);
  serverPart:= AlCopyStr(Value, i + 1, Length(Value));

  // Extension (.fr, .com, etc..) must be betwen 2 to 6 char
  i:= AlPos('.', serverPart);
  j := 0;
  While I > 0 do begin
    j := i;
    I := AlPosEx('.', serverPart, i + 1);
  end;
  if (j <= 1) then Exit; // no dot at all so exit !
  extPart    := AlCopyStr(ServerPart,J+1,Maxint);
  serverPart := ALCopyStr(ServerPart, 1, J - 1);
  If not (Length(ExtPart) in [2..6]) then exit;

  Result:= CheckAllowedname(namePart) and
           CheckAllowedHostname(serverPart) and
           CheckAllowedExt(ExtPart);
end;

{********************************************************************************}
function AlLocalDateTimeToGMTDateTime(Const aLocalDateTime: TDateTime): TdateTime;

  {--------------------------------------------}
  function InternalCalcTimeZoneBias : TDateTime;
  const Time_Zone_ID_DayLight = 2;
  var TZI: TTimeZoneInformation;
      TZIResult: Integer;
      aBias : Integer;
  begin
    TZIResult := GetTimeZoneInformation(TZI);
    if TZIResult = -1 then Result := 0
    else begin
      if TZIResult = Time_Zone_ID_DayLight then aBias := TZI.Bias + TZI.DayLightBias
      else aBias := TZI.Bias + TZI.StandardBias;
      Result := EncodeTime(Abs(aBias) div 60, Abs(aBias) mod 60, 0, 0);
      if aBias < 0 then Result := -Result;
    end;
  end;

begin
  Result := aLocalDateTime + InternalCalcTimeZoneBias;
end;

{******************************************************}
Function ALInc(var x: integer; Count: integer): Integer;
begin
  inc(X, count);
  result := X;
end;

end.
