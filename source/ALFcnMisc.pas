{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      Alcinoe Misc functions
Version:      3.51

Description:  Alcinoe Misc Functions

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

History :     09/01/2005: correct then AlEmptyDirectory function
              25/05/2006: Move some function to AlFcnFile
              25/02/2008: Update AlIsValidEmail
              06/10/3008: Update AlIsValidEmail
Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALFcnMisc;

interface

uses Windows,
     sysutils;

Function AlBoolToInt(Value:Boolean):Integer;
Function ALMediumPos(LTotal, LBorder, LObject : integer):Integer;
Function ALIsInteger (const S : String) : Boolean;
Function ALIsSmallInt (const S : String) : Boolean;
Function AlStrToBool(Value:String):Boolean;
Function ALMakeKeyStrByGUID: String;
function AlIsValidEmail(const Value: string): boolean;
function AlLocalDateTimeToGMTDateTime(Const aLocalDateTime: TDateTime): TdateTime;

implementation

uses SysConst,
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

{************************************************}
Function ALIsInteger (const S : String) : Boolean;
var i : Integer;
Begin
 Result := TryStrToInt(S, I);
End;

{*************************************************}
Function ALIsSmallInt (const S : String) : Boolean;
var i : Integer;
Begin
 Result := TryStrToInt(S, I) and (i <= 32767) and (I >= -32768);
End;

{*****************************************}
Function AlStrToBool(Value:String):Boolean;
Begin
  Result := False;
  TryStrtoBool(Value,Result);
end;

{***********************************}
Function  ALMakeKeyStrByGUID: String;
Var aGUID: TGUID;
Begin
  CreateGUID(aGUID);
  Result := GUIDToString(aGUID);
  Delete(Result,1,1);
  Delete(Result,Length(result),1);
End;

{****************************************************}
function AlIsValidEmail(const Value: string): boolean;

 {--------------------------------------------------}
 function CheckAllowedname(const s: string): boolean;
 var i: integer;
 begin
   Result:= false;
   for i:= 1 to Length(s) do begin
     // illegal char in s -> no valid address
     if not (s[i] in ['a'..'z','A'..'Z','0'..'9','_','-','.']) then Exit;
   end;
   Result:= true;
 end;

 {-------------------------------------------------}
 function CheckAllowedExt(const s: string): boolean;
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
    namePart, serverPart, extPart: string;
begin
  Result:= false;

  // Value can not be < 6 char (ex: a@b.fr)
  if length(Value) < 6 then exit;

  // must have the '@' char inside
  i:= AlCharPos('@', Value);
  if (i <= 1) or (i > length(Value)-4) then exit;

  //can not have @. or .@
  if (value[i-1] = '.') or (value[i+1] = '.') then exit;

  //can not have 2 ..
  If (ALpos('..', Value) > 0) then Exit;

  //extract namePart and serverPart
  namePart:= AlCopyStr(Value, 1, i - 1);
  serverPart:= AlCopyStr(Value, i + 1, Length(Value));

  // Extension (.fr, .com, etc..) must be betwen 2 to 6 char
  i:= AlCharPos('.', serverPart);
  j := 0;
  While I > 0 do begin
    j := i;
    I := AlCharPosEx('.', serverPart, i + 1);
  end;
  if (j <= 1) then Exit; // no dot at all so exit !
  extPart := AlCopyStr(ServerPart,J+1,Maxint);
  If not (Length(ExtPart) in [2..6]) then exit;

  Result:= CheckAllowedname(namePart) and CheckAllowedname(serverPart) and CheckAllowedExt(ExtPart);
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

end.
