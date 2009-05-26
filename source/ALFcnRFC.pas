{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      Alcinoe RFC Functions
Version:      3.50

Description:  Common functions to work with RFC standards. especialy
              to convert RFC date time string to TDateTime.

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

History :

Link :        http://www.rfc.net/

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALFcnRFC;

interface

uses windows,
     SysConst,
     Classes;

function ALGmtDateTimeToRfc822Str(const aValue: TDateTime): String;
function ALDateTimeToRfc822Str(const aValue: TDateTime): String;
Function ALTryRfc822StrToGMTDateTime(const S: string; out Value: TDateTime): Boolean;
function ALRfc822StrToGMTDateTime(const s: String): TDateTime;

const
  CAlRfc822DaysOfWeek: array[1..7] of string = (
                                                'Sun',
                                                'Mon',
                                                'Tue',
                                                'Wed',
                                                'Thu',
                                                'Fri',
                                                'Sat'
                                               );

  CALRfc822MonthNames: array[1..12] of string = (
                                                 'Jan',
                                                 'Feb',
                                                 'Mar',
                                                 'Apr',
                                                 'May',
                                                 'Jun',
                                                 'Jul',
                                                 'Aug',
                                                 'Sep',
                                                 'Oct',
                                                 'Nov',
                                                 'Dec'
                                                );

implementation

Uses SYsUtils,
     AlFcnMisc,
     AlFcnString;

{*********************************************************************}
{aValue is a GMT TDateTime - result is "Sun, 06 Nov 1994 08:49:37 GMT"}
function  ALGMTDateTimeToRfc822Str(const aValue: TDateTime): String;
var aDay, aMonth, aYear: Word;
begin
  DecodeDate(
             aValue,
             aYear,
             aMonth,
             aDay
            );

  Result := Format(
                   '%s, %.2d %s %.4d %s %s',
                   [
                    CAlRfc822DaysOfWeek[DayOfWeek(aValue)],
                    aDay,
                    CAlRfc822MonthNames[aMonth],
                    aYear,
                    FormatDateTime('hh":"nn":"ss', aValue),
                    'GMT'
                   ]
                  );
end;

{***********************************************************************}
{aValue is a Local TDateTime - result is "Sun, 06 Nov 1994 08:49:37 GMT"}
function ALDateTimeToRfc822Str(const aValue: TDateTime): String;
begin
  Result := ALGMTDateTimeToRfc822Str(AlLocalDateTimeToGMTDateTime(aValue));
end;

{************************************************************}
{Sun, 06 Nov 1994 08:49:37 GMT  ; RFC 822, updated by RFC 1123
 the function allow also date like "Sun, 06-Nov-1994 08:49:37 GMT"
 to be compatible with cookies field (http://wp.netscape.com/newsref/std/cookie_spec.html)

 The "Date" line (formerly "Posted") is the date that the message was
 originally posted to the network.  Its format must be acceptable
 both in RFC-822 and to the getdate(3) routine that is provided with
 the Usenet software.  This date remains unchanged as the message is
 propagated throughout the network.  One format that is acceptable to
 both is:

                      Wdy, DD Mon YY HH:MM:SS TIMEZONE

 Several examples of valid dates appear in the sample message above.
 Note in particular that ctime(3) format:

                      Wdy Mon DD HH:MM:SS YYYY

  is not acceptable because it is not a valid RFC-822 date.  However,
  since older software still generates this format, news
  implementations are encouraged to accept this format and translate
  it into an acceptable format.

  There is no hope of having a complete list of timezones.  Universal
  Time (GMT), the North American timezones (PST, PDT, MST, MDT, CST,
  CDT, EST, EDT) and the +/-hhmm offset specifed in RFC-822 should be
  supported.  It is recommended that times in message headers be
  transmitted in GMT and displayed in the local time zone.}
Function  ALTryRfc822StrToGMTDateTime(const S: string; out Value: TDateTime): Boolean;
Var P1,P2: Integer;
    ADateStr : String;
    aLst: TstringList;
    aMonthLabel: String;
    aFormatSettings: TformatSettings;
    aTimeZoneStr: String;
    aTimeZoneDelta: TDateTime;

    {----------------------------------------------------------}
    Function MonthWithLeadingChar(const AMonth: String): String;
    Begin
      If Length(AMonth) = 1 then result := '0' + AMonth
      else result := aMonth;
    end;

Begin
  ADateStr := S; //'Wdy, DD-Mon-YYYY HH:MM:SS GMT' or 'Wdy, DD-Mon-YYYY HH:MM:SS +0200' or '23 Aug 2004 06:48:46 -0700'
  P1 := AlPos(',',ADateStr);
  If P1 > 0 then delete(ADateStr,1,P1); //' DD-Mon-YYYY HH:MM:SS GMT' or ' DD-Mon-YYYY HH:MM:SS +0200' or '23 Aug 2004 06:48:46 -0700'
  ADateStr := trim(ADateStr); //'DD-Mon-YYYY HH:MM:SS GMT' or 'DD-Mon-YYYY HH:MM:SS +0200' or '23 Aug 2004 06:48:46 -0700'

  P1 := AlcharPos(':',ADateStr);
  P2 := AlcharPos('-',ADateStr);
  While (P2 > 0) and (P2 < P1) do begin
    Delete(aDateStr,P2,1);
    dec(P1);
    P2 := AlcharPosEx('-',ADateStr,P2);
  end; //'DD Mon YYYY HH:MM:SS GMT' or 'DD Mon YYYY HH:MM:SS +0200' or '23 Aug 2004 06:48:46 -0700'
  While Alpos('  ',ADateStr) > 0 do ADateStr := AlStringReplace(ADateStr,'  ',' ',[RfReplaceAll]); //'DD Mon YYYY HH:MM:SS GMT' or 'DD Mon YYYY HH:MM:SS +0200'
  Alst := TstringList.create;
  Try

    Alst.Text :=  AlStringReplace(ADateStr,' ',#13#10,[RfReplaceall]);
    If Alst.Count < 5 then begin
      Result := False;
      Exit;
    end;

    aMonthLabel := trim(Alst[1]);
    P1 := 1;
    While (p1 <= 12) and (not sameText(CAlRfc822MonthNames[P1],aMonthLabel)) do inc(P1);
    If P1 > 12 then begin
      Result := False;
      Exit;
    end;

    GetLocaleFormatSettings(GetSystemDefaultLCID,aFormatSettings);
    aFormatSettings.DateSeparator := '/';
    aFormatSettings.TimeSeparator := ':';
    aFormatSettings.ShortDateFormat := 'dd/mm/yyyy';
    aFormatSettings.ShortTimeFormat := 'hh:nn:zz';

    aTimeZoneStr := trim(Alst[4]);
    aTimeZoneStr := AlStringReplace(aTimeZoneStr,'(','',[]);
    aTimeZoneStr := AlStringReplace(aTimeZoneStr,')','',[]);
    aTimeZoneStr := Trim(aTimeZoneStr);
    If aTimeZoneStr = '' then Begin
      Result := False;
      Exit;
    end
    else If (Length(aTimeZoneStr) >= 5) and
            (aTimeZoneStr[1] in ['+','-']) and
            (aTimeZoneStr[2] in ['0','1','2','3','4','5','6','7','8','9']) and
            (aTimeZoneStr[3] in ['0','1','2','3','4','5','6','7','8','9']) and
            (aTimeZoneStr[4] in ['0','1','2','3','4','5','6','7','8','9']) and
            (aTimeZoneStr[5] in ['0','1','2','3','4','5','6','7','8','9']) then begin
      aTimeZoneDelta := StrToDateTime(AlCopyStr(aTimeZoneStr,2,2) + ':' + AlCopyStr(aTimeZoneStr,4,2) + ':00', aFormatSettings);
      if aTimeZoneStr[1] = '+' then aTimeZoneDelta := -1*aTimeZoneDelta;
    end
    else If SameText(aTimeZoneStr,'GMT') then  aTimeZoneDelta := 0
    else If SameText(aTimeZoneStr,'UTC') then  aTimeZoneDelta := 0
    else If SameText(aTimeZoneStr,'UT')  then  aTimeZoneDelta := 0
    else If SameText(aTimeZoneStr,'EST') then aTimeZoneDelta := StrToDateTime('05:00:00', aFormatSettings)
    else If SameText(aTimeZoneStr,'EDT') then aTimeZoneDelta := StrToDateTime('04:00:00', aFormatSettings)
    else If SameText(aTimeZoneStr,'CST') then aTimeZoneDelta := StrToDateTime('06:00:00', aFormatSettings)
    else If SameText(aTimeZoneStr,'CDT') then aTimeZoneDelta := StrToDateTime('05:00:00', aFormatSettings)
    else If SameText(aTimeZoneStr,'MST') then aTimeZoneDelta := StrToDateTime('07:00:00', aFormatSettings)
    else If SameText(aTimeZoneStr,'MDT') then aTimeZoneDelta := StrToDateTime('06:00:00', aFormatSettings)
    else If SameText(aTimeZoneStr,'PST') then aTimeZoneDelta := StrToDateTime('08:00:00', aFormatSettings)
    else If SameText(aTimeZoneStr,'PDT') then aTimeZoneDelta := StrToDateTime('07:00:00', aFormatSettings)
    else begin
      Result := False;
      Exit;
    end;

    ADateStr := trim(Alst[0]) + '/' + MonthWithLeadingChar(inttostr(P1))  + '/' + trim(Alst[2]) + ' ' + trim(Alst[3]); //'DD/MM/YYYY HH:MM:SS'
    Result := TryStrToDateTime(ADateStr,Value,AformatSettings);
    If Result then Value := Value + aTimeZoneDelta;
  finally
    aLst.free;
  end;
end;

{*************************************************************}
{Sun, 06 Nov 1994 08:49:37 GMT  ; RFC 822, updated by RFC 1123
 the function allow also date like "Sun, 06-Nov-1994 08:49:37 GMT"
 to be compatible with cookies field (http://wp.netscape.com/newsref/std/cookie_spec.html)}
function  ALRfc822StrToGMTDateTime(const s: String): TDateTime;
Begin
  if not ALTryRfc822StrToGMTDateTime(S, Result) then
    raise EConvertError.CreateResFmt(@SInvalidDateTime, [S]);
end;

end.
