{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code              
Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
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

* Please send all your feedback to alcinoe@arkadia.com
* If you have downloaded this source from a website different from 
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALMisc;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

Function AlBoolToInt(Value:Boolean):Integer;
Function ALMediumPos(LTotal, LBorder, LObject : integer):Integer;
function AlLocalDateTimeToGMTDateTime(Const aLocalDateTime: TDateTime): TdateTime;
function ALGMTNow: TDateTime;
function ALUnixMsToDateTime(const aValue: Int64): TDateTime;
function ALDateTimeToUnixMs(const aValue: TDateTime): Int64;
Function ALInc(var x: integer; Count: integer): Integer;

const ALMAXUInt64: UInt64 = 18446744073709551615;
      ALMAXInt64: Int64 = 9223372036854775807;
      ALMAXINT: integer = 2147483647; // this is unecessarily because MAXINT system const exists but just for consistency

implementation

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     Winapi.Windows,
     system.sysutils,
     system.DateUtils,
     {$ELSE}
     Windows,
     sysutils,
     DateUtils,
     {$IFEND}
     ALString;

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

{**************************}
{The same like Now but used
 GMT-time not local time.}
function ALGMTNow: TDateTime;
begin
  result := AlLocalDateTimeToGMTDateTime(NOW);
end;

{******************************************************}
Function ALInc(var x: integer; Count: integer): Integer;
begin
  inc(X, count);
  result := X;
end;

{*******************************************************}
{Accepts number of milliseconds in the parameter aValue,
 provides 1000 times more precise value of TDateTime}
function ALUnixMsToDateTime(const aValue: Int64): TDateTime;
begin
  Result := IncMilliSecond(UnixDateDelta, aValue);
end;

{********************************************************}
{Returns UNIX-time as the count of milliseconds since the
 UNIX epoch. Can be very useful for the purposes of
 special precision.}
function ALDateTimeToUnixMs(const aValue: TDateTime): Int64;
begin
  result := MilliSecondsBetween(UnixDateDelta, aValue);
  if aValue < UnixDateDelta then result := -result;
end;

end.
