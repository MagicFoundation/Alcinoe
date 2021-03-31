(*******************************************************************************
Contributors:
Igor Ivkin (igor@arkadia.com)

ALLibPhoneNumber is a wrapper for a several functions that use
Google's C++ library libphonenumber to parse and format
phone numbers written in a free form.
This wrapper requires few DLLs to be working correctly.
These DLLs are distributed in the folder "lib/dll".

This wrapper is based on a custom DLL-modification that provides three
main functions:
1. To convert a given string phone written in a free form to Int64.
2. To convert phone given as Int64 to international format defined for its country.
3. To define a type of the phone (landing line, mobile, toll-free etc).
*******************************************************************************)

unit ALLibPhoneNumber;

interface

uses
  system.Classes,
  system.sysutils;

function ALStrPhoneNumberToInt64(const PhoneNumber, CountryCode: AnsiString): Int64; overload;
function ALStrPhoneNumberToInt64(PhoneNumber: AnsiString): Int64; overload;
function ALInt64PhoneNumberToStr(PhoneNumber: Int64): AnsiString;
function ALGetPhoneNumberType(PhoneNumber: Int64): integer;

const
  cALFixedLine = 0;
  cALMobile = 1;
  cALFixedLineOrMobil = 2; // mostly for US
  cALTollFree = 3;
  cALPremiumRate = 4;
  cALSharedCost = 5; // see http://en.wikipedia.org/wiki/Shared_Cost_Service
  cALVoIP = 6;
  cALPersonalNumber = 7;
  cALPager = 8;
  cALUAN = 9; // see "Universal Access Numbers"
  cALVoiceMail = 10;
  cALUnknown = 11;

implementation

uses
  alString;

function _StrPhoneNumberToInt64(phoneNumber, countryCode: PAnsiChar): Int64; cdecl; external 'libphonenumber.dll';
function _Int64PhoneNumberToStr(phoneNumber: Int64; buffer: PAnsiChar): Cardinal; cdecl; external 'libphonenumber.dll';
function _GetPhoneNumberType(phoneNumber: Int64): integer; cdecl; external 'libphonenumber.dll';

{**********************************************************************************}
function ALStrPhoneNumberToInt64(const PhoneNumber, CountryCode: AnsiString): Int64;
begin
  result := _StrPhoneNumberToInt64(PAnsiChar(PhoneNumber), PAnsiChar(AlUppercase(CountryCode)));
end;

{***************************************************}
{This function analyzes the phone number looking like
 [FR] 06.34.54.12.22, extract country code if it is possible and
 converts this number to an Int64 representation looking like 33634541222 }
function ALStrPhoneNumberToInt64(PhoneNumber: AnsiString): Int64;

  function _IsDecimal(const S: AnsiString): boolean;
  var i: integer;
  begin
    result := s <> '';
    if not result then exit;
    for i := low(s) to high(S) do begin
      if not (S[i] in ['0'..'9']) then begin
        result := false;
        break;
      end;
    end;
  end;

var LCountryCode: AnsiString;
    P1, P2: integer;
begin

  PhoneNumber := ALTrim(PhoneNumber);  //1rt trim the aPhoneNumber
  if _IsDecimal(PhoneNumber) and alTryStrToInt64(PhoneNumber, result) then exit; // if their is not the '+' sign we can do nothing because ALStrPhoneNumberToInt64 will return 0
                                                                                 // if alTryStrToInt64 not success it's mean it's a tooo big number, so better to return 0

  LCountryCode := '';
  P1 := AlPos('[',PhoneNumber);  // look if their is some prefix or suffix like [FR] to give an hint about the country
  while P1 > 0 do begin
    P2 := ALPosEx(']', PhoneNumber, P1+1);
    if P2 = P1 + 3 then begin
      LCountryCode := ALUpperCase(ALCopyStr(PhoneNumber, P1+1, 2)); // [FR] 06.34.54.12.22 => FR
      if (length(LCountryCode) = 2) and
         (LCountryCode[1] in ['A'..'Z']) and
         (LCountryCode[2] in ['A'..'Z']) then begin
        delete(PhoneNumber,P1,4); // "[FR] 06.34.54.12.22" => " 06.34.54.12.22"
        PhoneNumber := ALtrim(PhoneNumber); // " 06.34.54.12.22" => "06.34.54.12.22"
        break; // break the loop, we found the country code hint
      end
      else LCountryCode := '';
    end;
    P1 := AlPosEx('[',PhoneNumber, P1+1);
  end;
  result := ALStrPhoneNumberToInt64(PhoneNumber, LCountryCode); //even if the aPhoneNumber is already an integer we need to format it
                                                                //because user can gave us +330625142445 but it's must be stored as
                                                                //                         +33625142445

end;

{***************************************************************}
function ALInt64PhoneNumberToStr(PhoneNumber: Int64): AnsiString;
var ln: Cardinal;
begin
  SetLength(Result, 255);
  ln := _Int64PhoneNumberToStr(PhoneNumber, @Result[1]);
  SetLength(Result, ln);
end;

{*********************************************************}
function ALGetPhoneNumberType(PhoneNumber: Int64): integer;
begin
  result := _GetPhoneNumberType(PhoneNumber);
end;

end.

