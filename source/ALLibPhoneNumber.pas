(**************************************************************
www:          http://sourceforge.net/projects/alcinoe/
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
Contributors: Igor Ivkin (igor@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALLibPhoneNumber
Version:      1.0

Description:  ALLibPhoneNumber is a wrapper for a several functions that use
              Google's C++ library libphonenumber to parse and format
              phone numbers written in a free form.
              This wrapper requires few DLLs to be working correctly.
              These DLLs are distributed in the folder "lib/dll".

              This wrapper is based on a custom DLL-modification that provides three
              main functions:
              1. To convert a given string phone written in a free form to Int64.
              2. To convert phone given as Int64 to international format defined for its country.
              3. To define a type of the phone (landing line, mobile, toll-free etc).


Legal issues: Copyright (C) 1999-2015 by Arkadia Software Engineering

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

Known bug :

History :

Link :

* Please send all your feedback to alcinoe@arkadia.com
* If you have downloaded this source from a website different from
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
**************************************************************)
unit ALLibPhoneNumber;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     system.Classes,
     system.sysutils;
     {$ELSE}
     Classes,
     sysutils;
     {$IFEND}

function ALStrPhoneNumberToInt64(const PhoneNumber, CountryCode: AnsiString): Int64;
function ALInt64PhoneNumberToStr(PhoneNumber: Int64): AnsiString;
function ALGetPhoneNumberType(PhoneNumber: Int64): integer;

const cALFixedLine = 0;
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

uses alString;

function _StrPhoneNumberToInt64(phoneNumber, countryCode: PAnsiChar): Int64; cdecl; external 'libphonenumber.dll';
function _Int64PhoneNumberToStr(phoneNumber: Int64; buffer: PAnsiChar): Cardinal; cdecl; external 'libphonenumber.dll';
function _GetPhoneNumberType(phoneNumber: Int64): integer; cdecl; external 'libphonenumber.dll';

{**********************************************************************************}
function ALStrPhoneNumberToInt64(const PhoneNumber, CountryCode: AnsiString): Int64;
begin
  result := _StrPhoneNumberToInt64(PAnsiChar(PhoneNumber), PAnsiChar(AlUppercase(CountryCode)));
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

