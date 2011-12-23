{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      Alcinoe Unicode Functions
Version:      3.52

Description:  This unit contains a Unicode support library along
              with some additional files to use WideStrings/Unicode
              strings within your application.

Legal issues: Copyright (C) 1999-2008 by Arkadia Software Engineering

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

History :     14/11/2005: Add Function ALStringToWideString,
                          AlWideStringToString, ALUTF8Encode,
                          ALUTF8decode, ALGetCodePageFromName;
              29/10/2007: Add function ALGetCodePageFromLCID
              10/11/2007: update ALWideRemoveDiacritic to handle
                          char like U+1E69 (html code &#7785;)
              20/11/2007: add function ALUTF8ISO91995TransliterationCyrillicToLatin
              12/01/2009: add InternalfoldNonDiacriticChar to ALWideNormalize
                          to fold letter like l with stroke to l without stroke

Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALFcnUnicode;

interface

uses windows;

Function ALWideNormalize(S: Widestring): Widestring;
Function ALWideRemoveDiacritic(S: Widestring): Widestring;
Function ALWideExpandLigatures(S: Widestring): Widestring;
Function ALWideUpperCaseNoDiacritic(S: Widestring): Widestring;
Function ALWideLowerCaseNoDiacritic(S: Widestring): Widestring;
Function ALUTF8RemoveDiacritic(S: UTF8String): UTF8String;
Function ALUTF8ExpandLigatures(S: UTF8String): UTF8String;
Function ALUTF8UpperCaseNoDiacritic(S: UTF8String): UTF8String;
Function ALUTF8LowerCaseNoDiacritic(S: UTF8String): UTF8String;
Function ALUTF8Normalize(S: UTF8String): UTF8String;
function ALUTF8UpperCase(const s: UTF8String): UTF8String;
function ALUTF8LowerCase(const s: UTF8String): UTF8String;
function AlUTF8DetectBOM(const P: PChar; const Size: Integer): Boolean;
function ALUTF8CharSize(const P: PChar; const Size: Integer): Integer;
function ALUTF8CharCount(const P: PChar; const Size: Integer): Integer; overload;
function ALUTF8CharCount(const S: Utf8String): Integer; overload;
Function ALUTF8Trunc(s:UTF8string; Count: Integer): UTF8String;
Function ALUTF8UpperFirstChar(s:UTF8string): UTF8String;
Function ALUTF8LowerCaseFirstCharUpper(s:UTF8string): UTF8String;
Function ALStringToWideString(const S: string; aCodePage: Word): WideString;
function AlWideStringToString(const WS: WideString; aCodePage: Word): string;
Function ALUTF8Encode(const S: string; aCodePage: Word): UTF8String;
Function ALUTF8decode(const S: UTF8String; aCodePage: Word): String;
Function ALGetCodePageFromName(Acharset:String): Word;
Function ALGetCodePageFromLCID(aLCID:Integer): Word;
Function ALUTF8ISO91995CyrillicToLatin(aCyrillicText: UTF8String): UTF8String;

implementation

uses SysUtils;

{*******************}
{Normalize an string
 ie: l''été sur l''europe => l-ete-sur-l-europe}
Function ALWideNormalize(S: Widestring): Widestring;

  {--------------------------------------------------------}
  {source: http://issues.apache.org/jira/browse/LUCENE-1343}
  Procedure InternalfoldNonDiacriticChar(Var aStr: WideString);
  Var i, j : integer;
  Begin
    for I := 1 to length(aStr) do begin
      j := ord(aStr[i]);
      case j of
        $0181: aStr[i] := widechar($0042);    //  LATIN CAPITAL LETTER B WITH HOOK -> LATIN CAPITAL LETTER B
        $0182: aStr[i] := widechar($0042);    //  LATIN CAPITAL LETTER B WITH TOPBAR -> LATIN CAPITAL LETTER B
        $0187: aStr[i] := widechar($0043);    //  LATIN CAPITAL LETTER C WITH HOOK -> LATIN CAPITAL LETTER C
        $0110: aStr[i] := widechar($0044);    //  LATIN CAPITAL LETTER D WITH STROKE -> LATIN CAPITAL LETTER D
        $018A: aStr[i] := widechar($0044);    //  LATIN CAPITAL LETTER D WITH HOOK -> LATIN CAPITAL LETTER D
        $018B: aStr[i] := widechar($0044);    //  LATIN CAPITAL LETTER D WITH TOPBAR -> LATIN CAPITAL LETTER D
        $0191: aStr[i] := widechar($0046);    //  LATIN CAPITAL LETTER F WITH HOOK -> LATIN CAPITAL LETTER F
        $0193: aStr[i] := widechar($0047);    //  LATIN CAPITAL LETTER G WITH HOOK -> LATIN CAPITAL LETTER G
        $01E4: aStr[i] := widechar($0047);    //  LATIN CAPITAL LETTER G WITH STROKE -> LATIN CAPITAL LETTER G
        $0126: aStr[i] := widechar($0048);    //  LATIN CAPITAL LETTER H WITH STROKE -> LATIN CAPITAL LETTER H
        $0197: aStr[i] := widechar($0049);    //  LATIN CAPITAL LETTER I WITH STROKE -> LATIN CAPITAL LETTER I
        $0198: aStr[i] := widechar($004B);    //  LATIN CAPITAL LETTER K WITH HOOK -> LATIN CAPITAL LETTER K
        $0141: aStr[i] := widechar($004C);    //  LATIN CAPITAL LETTER L WITH STROKE -> LATIN CAPITAL LETTER L
        $019D: aStr[i] := widechar($004E);    //  LATIN CAPITAL LETTER N WITH LEFT HOOK -> LATIN CAPITAL LETTER N
        $0220: aStr[i] := widechar($004E);    //  LATIN CAPITAL LETTER N WITH LONG RIGHT LEG -> LATIN CAPITAL LETTER N
        $00D8: aStr[i] := widechar($004F);    //  LATIN CAPITAL LETTER O WITH STROKE -> LATIN CAPITAL LETTER O
        $019F: aStr[i] := widechar($004F);    //  LATIN CAPITAL LETTER O WITH MIDDLE TILDE -> LATIN CAPITAL LETTER O
        $01FE: aStr[i] := widechar($004F);    //  LATIN CAPITAL LETTER O WITH STROKE AND ACUTE -> LATIN CAPITAL LETTER O
        $01A4: aStr[i] := widechar($0050);    //  LATIN CAPITAL LETTER P WITH HOOK -> LATIN CAPITAL LETTER P
        $0166: aStr[i] := widechar($0054);    //  LATIN CAPITAL LETTER T WITH STROKE -> LATIN CAPITAL LETTER T
        $01AC: aStr[i] := widechar($0054);    //  LATIN CAPITAL LETTER T WITH HOOK -> LATIN CAPITAL LETTER T
        $01AE: aStr[i] := widechar($0054);    //  LATIN CAPITAL LETTER T WITH RETROFLEX HOOK -> LATIN CAPITAL LETTER T
        $01B2: aStr[i] := widechar($0056);    //  LATIN CAPITAL LETTER V WITH HOOK -> LATIN CAPITAL LETTER V
        $01B3: aStr[i] := widechar($0059);    //  LATIN CAPITAL LETTER Y WITH HOOK -> LATIN CAPITAL LETTER Y
        $01B5: aStr[i] := widechar($005A);    //  LATIN CAPITAL LETTER Z WITH STROKE -> LATIN CAPITAL LETTER Z
        $0224: aStr[i] := widechar($005A);    //  LATIN CAPITAL LETTER Z WITH HOOK -> LATIN CAPITAL LETTER Z
        $0180: aStr[i] := widechar($0062);    //  LATIN SMALL LETTER B WITH STROKE -> LATIN SMALL LETTER B
        $0183: aStr[i] := widechar($0062);    //  LATIN SMALL LETTER B WITH TOPBAR -> LATIN SMALL LETTER B
        $0253: aStr[i] := widechar($0062);    //  LATIN SMALL LETTER B WITH HOOK -> LATIN SMALL LETTER B
        $0188: aStr[i] := widechar($0063);    //  LATIN SMALL LETTER C WITH HOOK -> LATIN SMALL LETTER C
        $0255: aStr[i] := widechar($0063);    //  LATIN SMALL LETTER C WITH CURL -> LATIN SMALL LETTER C
        $0111: aStr[i] := widechar($0064);    //  LATIN SMALL LETTER D WITH STROKE -> LATIN SMALL LETTER D
        $018C: aStr[i] := widechar($0064);    //  LATIN SMALL LETTER D WITH TOPBAR -> LATIN SMALL LETTER D
        $0221: aStr[i] := widechar($0064);    //  LATIN SMALL LETTER D WITH CURL -> LATIN SMALL LETTER D
        $0256: aStr[i] := widechar($0064);    //  LATIN SMALL LETTER D WITH TAIL -> LATIN SMALL LETTER D
        $0257: aStr[i] := widechar($0064);    //  LATIN SMALL LETTER D WITH HOOK -> LATIN SMALL LETTER D
        $0192: aStr[i] := widechar($0066);    //  LATIN SMALL LETTER F WITH HOOK -> LATIN SMALL LETTER F
        $01E5: aStr[i] := widechar($0067);    //  LATIN SMALL LETTER G WITH STROKE -> LATIN SMALL LETTER G
        $0260: aStr[i] := widechar($0067);    //  LATIN SMALL LETTER G WITH HOOK -> LATIN SMALL LETTER G
        $0127: aStr[i] := widechar($0068);    //  LATIN SMALL LETTER H WITH STROKE -> LATIN SMALL LETTER H
        $0266: aStr[i] := widechar($0068);    //  LATIN SMALL LETTER H WITH HOOK -> LATIN SMALL LETTER H
        $0268: aStr[i] := widechar($0069);    //  LATIN SMALL LETTER I WITH STROKE -> LATIN SMALL LETTER I
        $029D: aStr[i] := widechar($006A);    //  LATIN SMALL LETTER J WITH CROSSED-TAIL -> LATIN SMALL LETTER J
        $0199: aStr[i] := widechar($006B);    //  LATIN SMALL LETTER K WITH HOOK -> LATIN SMALL LETTER K
        $0142: aStr[i] := widechar($006C);    //  LATIN SMALL LETTER L WITH STROKE -> LATIN SMALL LETTER L
        $019A: aStr[i] := widechar($006C);    //  LATIN SMALL LETTER L WITH BAR -> LATIN SMALL LETTER L
        $0234: aStr[i] := widechar($006C);    //  LATIN SMALL LETTER L WITH CURL -> LATIN SMALL LETTER L
        $026B: aStr[i] := widechar($006C);    //  LATIN SMALL LETTER L WITH MIDDLE TILDE -> LATIN SMALL LETTER L
        $026C: aStr[i] := widechar($006C);    //  LATIN SMALL LETTER L WITH BELT -> LATIN SMALL LETTER L
        $026D: aStr[i] := widechar($006C);    //  LATIN SMALL LETTER L WITH RETROFLEX HOOK -> LATIN SMALL LETTER L
        $0271: aStr[i] := widechar($006D);    //  LATIN SMALL LETTER M WITH HOOK -> LATIN SMALL LETTER M
        $019E: aStr[i] := widechar($006E);    //  LATIN SMALL LETTER N WITH LONG RIGHT LEG -> LATIN SMALL LETTER N
        $0235: aStr[i] := widechar($006E);    //  LATIN SMALL LETTER N WITH CURL -> LATIN SMALL LETTER N
        $0272: aStr[i] := widechar($006E);    //  LATIN SMALL LETTER N WITH LEFT HOOK -> LATIN SMALL LETTER N
        $0273: aStr[i] := widechar($006E);    //  LATIN SMALL LETTER N WITH RETROFLEX HOOK -> LATIN SMALL LETTER N
        $00F8: aStr[i] := widechar($006F);    //  LATIN SMALL LETTER O WITH STROKE -> LATIN SMALL LETTER O
        $01FF: aStr[i] := widechar($006F);    //  LATIN SMALL LETTER O WITH STROKE AND ACUTE -> LATIN SMALL LETTER O
        $01A5: aStr[i] := widechar($0070);    //  LATIN SMALL LETTER P WITH HOOK -> LATIN SMALL LETTER P
        $02A0: aStr[i] := widechar($0071);    //  LATIN SMALL LETTER Q WITH HOOK -> LATIN SMALL LETTER Q
        $027C: aStr[i] := widechar($0072);    //  LATIN SMALL LETTER R WITH LONG LEG -> LATIN SMALL LETTER R
        $027D: aStr[i] := widechar($0072);    //  LATIN SMALL LETTER R WITH TAIL -> LATIN SMALL LETTER R
        $0282: aStr[i] := widechar($0073);    //  LATIN SMALL LETTER S WITH HOOK -> LATIN SMALL LETTER S
        $0167: aStr[i] := widechar($0074);    //  LATIN SMALL LETTER T WITH STROKE -> LATIN SMALL LETTER T
        $01AB: aStr[i] := widechar($0074);    //  LATIN SMALL LETTER T WITH PALATAL HOOK -> LATIN SMALL LETTER T
        $01AD: aStr[i] := widechar($0074);    //  LATIN SMALL LETTER T WITH HOOK -> LATIN SMALL LETTER T
        $0236: aStr[i] := widechar($0074);    //  LATIN SMALL LETTER T WITH CURL -> LATIN SMALL LETTER T
        $0288: aStr[i] := widechar($0074);    //  LATIN SMALL LETTER T WITH RETROFLEX HOOK -> LATIN SMALL LETTER T
        $028B: aStr[i] := widechar($0076);    //  LATIN SMALL LETTER V WITH HOOK -> LATIN SMALL LETTER V
        $01B4: aStr[i] := widechar($0079);    //  LATIN SMALL LETTER Y WITH HOOK -> LATIN SMALL LETTER Y
        $01B6: aStr[i] := widechar($007A);    //  LATIN SMALL LETTER Z WITH STROKE -> LATIN SMALL LETTER Z
        $0225: aStr[i] := widechar($007A);    //  LATIN SMALL LETTER Z WITH HOOK -> LATIN SMALL LETTER Z
        $0290: aStr[i] := widechar($007A);    //  LATIN SMALL LETTER Z WITH RETROFLEX HOOK -> LATIN SMALL LETTER Z
        $0291: aStr[i] := widechar($007A);    //  LATIN SMALL LETTER Z WITH CURL -> LATIN SMALL LETTER Z
        $025A: aStr[i] := widechar($0259);    //  LATIN SMALL LETTER SCHWA WITH HOOK -> LATIN SMALL LETTER SCHWA
        $0286: aStr[i] := widechar($0283);    //  LATIN SMALL LETTER ESH WITH CURL -> LATIN SMALL LETTER ESH
        $01BA: aStr[i] := widechar($0292);    //  LATIN SMALL LETTER EZH WITH TAIL -> LATIN SMALL LETTER EZH
        $0293: aStr[i] := widechar($0292);    //  LATIN SMALL LETTER EZH WITH CURL -> LATIN SMALL LETTER EZH
        $0490: aStr[i] := widechar($0413);    //  CYRILLIC CAPITAL LETTER GHE WITH UPTURN -> CYRILLIC CAPITAL LETTER GHE
        $0492: aStr[i] := widechar($0413);    //  CYRILLIC CAPITAL LETTER GHE WITH STROKE -> CYRILLIC CAPITAL LETTER GHE
        $0494: aStr[i] := widechar($0413);    //  CYRILLIC CAPITAL LETTER GHE WITH MIDDLE HOOK -> CYRILLIC CAPITAL LETTER GHE
        $0496: aStr[i] := widechar($0416);    //  CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER -> CYRILLIC CAPITAL LETTER ZHE
        $0498: aStr[i] := widechar($0417);    //  CYRILLIC CAPITAL LETTER ZE WITH DESCENDER -> CYRILLIC CAPITAL LETTER ZE
        $048A: aStr[i] := widechar($0419);    //  CYRILLIC CAPITAL LETTER SHORT I WITH TAIL -> CYRILLIC CAPITAL LETTER SHORT I
        $049A: aStr[i] := widechar($041A);    //  CYRILLIC CAPITAL LETTER KA WITH DESCENDER -> CYRILLIC CAPITAL LETTER KA
        $049C: aStr[i] := widechar($041A);    //  CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE -> CYRILLIC CAPITAL LETTER KA
        $049E: aStr[i] := widechar($041A);    //  CYRILLIC CAPITAL LETTER KA WITH STROKE -> CYRILLIC CAPITAL LETTER KA
        $04C3: aStr[i] := widechar($041A);    //  CYRILLIC CAPITAL LETTER KA WITH HOOK -> CYRILLIC CAPITAL LETTER KA
        $04C5: aStr[i] := widechar($041B);    //  CYRILLIC CAPITAL LETTER EL WITH TAIL -> CYRILLIC CAPITAL LETTER EL
        $04CD: aStr[i] := widechar($041C);    //  CYRILLIC CAPITAL LETTER EM WITH TAIL -> CYRILLIC CAPITAL LETTER EM
        $04A2: aStr[i] := widechar($041D);    //  CYRILLIC CAPITAL LETTER EN WITH DESCENDER -> CYRILLIC CAPITAL LETTER EN
        $04C7: aStr[i] := widechar($041D);    //  CYRILLIC CAPITAL LETTER EN WITH HOOK -> CYRILLIC CAPITAL LETTER EN
        $04C9: aStr[i] := widechar($041D);    //  CYRILLIC CAPITAL LETTER EN WITH TAIL -> CYRILLIC CAPITAL LETTER EN
        $04A6: aStr[i] := widechar($041F);    //  CYRILLIC CAPITAL LETTER PE WITH MIDDLE HOOK -> CYRILLIC CAPITAL LETTER PE
        $048E: aStr[i] := widechar($0420);    //  CYRILLIC CAPITAL LETTER ER WITH TICK -> CYRILLIC CAPITAL LETTER ER
        $04AA: aStr[i] := widechar($0421);    //  CYRILLIC CAPITAL LETTER ES WITH DESCENDER -> CYRILLIC CAPITAL LETTER ES
        $04AC: aStr[i] := widechar($0422);    //  CYRILLIC CAPITAL LETTER TE WITH DESCENDER -> CYRILLIC CAPITAL LETTER TE
        $04B2: aStr[i] := widechar($0425);    //  CYRILLIC CAPITAL LETTER HA WITH DESCENDER -> CYRILLIC CAPITAL LETTER HA
        $04B3: aStr[i] := widechar($0425);    //  CYRILLIC SMALL LETTER HA WITH DESCENDER -> CYRILLIC CAPITAL LETTER HA
        $0491: aStr[i] := widechar($0433);    //  CYRILLIC SMALL LETTER GHE WITH UPTURN -> CYRILLIC SMALL LETTER GHE
        $0493: aStr[i] := widechar($0433);    //  CYRILLIC SMALL LETTER GHE WITH STROKE -> CYRILLIC SMALL LETTER GHE
        $0495: aStr[i] := widechar($0433);    //  CYRILLIC SMALL LETTER GHE WITH MIDDLE HOOK -> CYRILLIC SMALL LETTER GHE
        $0497: aStr[i] := widechar($0436);    //  CYRILLIC SMALL LETTER ZHE WITH DESCENDER -> CYRILLIC SMALL LETTER ZHE
        $0499: aStr[i] := widechar($0437);    //  CYRILLIC SMALL LETTER ZE WITH DESCENDER -> CYRILLIC SMALL LETTER ZE
        $048B: aStr[i] := widechar($0439);    //  CYRILLIC SMALL LETTER SHORT I WITH TAIL -> CYRILLIC SMALL LETTER SHORT I
        $049B: aStr[i] := widechar($043A);    //  CYRILLIC SMALL LETTER KA WITH DESCENDER -> CYRILLIC SMALL LETTER KA
        $049D: aStr[i] := widechar($043A);    //  CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE -> CYRILLIC SMALL LETTER KA
        $049F: aStr[i] := widechar($043A);    //  CYRILLIC SMALL LETTER KA WITH STROKE -> CYRILLIC SMALL LETTER KA
        $04C4: aStr[i] := widechar($043A);    //  CYRILLIC SMALL LETTER KA WITH HOOK -> CYRILLIC SMALL LETTER KA
        $04C6: aStr[i] := widechar($043B);    //  CYRILLIC SMALL LETTER EL WITH TAIL -> CYRILLIC SMALL LETTER EL
        $04CE: aStr[i] := widechar($043C);    //  CYRILLIC SMALL LETTER EM WITH TAIL -> CYRILLIC SMALL LETTER EM
        $04A3: aStr[i] := widechar($043D);    //  CYRILLIC SMALL LETTER EN WITH DESCENDER -> CYRILLIC SMALL LETTER EN
        $04C8: aStr[i] := widechar($043D);    //  CYRILLIC SMALL LETTER EN WITH HOOK -> CYRILLIC SMALL LETTER EN
        $04CA: aStr[i] := widechar($043D);    //  CYRILLIC SMALL LETTER EN WITH TAIL -> CYRILLIC SMALL LETTER EN
        $04A7: aStr[i] := widechar($043F);    //  CYRILLIC SMALL LETTER PE WITH MIDDLE HOOK -> CYRILLIC SMALL LETTER PE
        $048F: aStr[i] := widechar($0440);    //  CYRILLIC SMALL LETTER ER WITH TICK -> CYRILLIC SMALL LETTER ER
        $04AB: aStr[i] := widechar($0441);    //  CYRILLIC SMALL LETTER ES WITH DESCENDER -> CYRILLIC SMALL LETTER ES
        $04AD: aStr[i] := widechar($0442);    //  CYRILLIC SMALL LETTER TE WITH DESCENDER -> CYRILLIC SMALL LETTER TE
        $04B9: aStr[i] := widechar($0447);    //  CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE -> CYRILLIC SMALL LETTER CHE
        $047C: aStr[i] := widechar($0460);    //  CYRILLIC CAPITAL LETTER OMEGA WITH TITLO -> CYRILLIC CAPITAL LETTER OMEGA
        $047D: aStr[i] := widechar($0461);    //  CYRILLIC SMALL LETTER OMEGA WITH TITLO -> CYRILLIC SMALL LETTER OMEGA
        $04B0: aStr[i] := widechar($04AE);    //  CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE -> CYRILLIC CAPITAL LETTER STRAIGHT U
        $04B1: aStr[i] := widechar($04AF);    //  CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE -> CYRILLIC SMALL LETTER STRAIGHT U
        $04B6: aStr[i] := widechar($04BC);    //  CYRILLIC CAPITAL LETTER CHE WITH DESCENDER -> CYRILLIC CAPITAL LETTER ABKHASIAN CHE
        $04B7: aStr[i] := widechar($04BC);    //  CYRILLIC SMALL LETTER CHE WITH DESCENDER -> CYRILLIC CAPITAL LETTER ABKHASIAN CHE
        $04B8: aStr[i] := widechar($04BC);    //  CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE -> CYRILLIC CAPITAL LETTER ABKHASIAN CHE
        $04BE: aStr[i] := widechar($04BC);    //  CYRILLIC CAPITAL LETTER ABKHASIAN CHE WITH DESCENDER -> CYRILLIC CAPITAL LETTER ABKHASIANCHE
        $04BF: aStr[i] := widechar($04BC);    //  CYRILLIC SMALL LETTER ABKHASIAN CHE WITH DESCENDER -> CYRILLIC CAPITAL LETTER ABKHASIAN CHE
        $04CB: aStr[i] := widechar($04BC);    //  CYRILLIC CAPITAL LETTER KHAKASSIAN CHE -> CYRILLIC CAPITAL LETTER ABKHASIAN CHE
        $04CC: aStr[i] := widechar($04BC);    //  CYRILLIC SMALL LETTER KHAKASSIAN CHE -> CYRILLIC CAPITAL LETTER ABKHASIAN CHE
      end;
    end;
  End;

Var i,j: integer;
    TmpWideStr: WideString;
Begin
  TmpWideStr := ALWideExpandLigatures(
                                     ALWideRemoveDiacritic(
                                                           Widelowercase(s)
                                                          )
                                     );
  SetLength(Result,length(TmpWideStr));
  j := 0;
  For i := 1 to length(TmpWideStr) do begin
    if IsCharAlphaNumericW(TmpWideStr[i]) then begin
      inc(j);
      result[j] := TmpWideStr[i];
    end
    else if (
             (j >= 1) and
             (result[j] <> '-')
            )
    then begin
      inc(j);
      result[j] := '-';
    end;

  end;
  While (J > 0) and (result[j] = '-') do dec(j);
  setlength(result,j);
  InternalfoldNonDiacriticChar(result);
end;

{*************************}
{remove accented character}
Function ALWideRemoveDiacritic(S: Widestring): Widestring;
var LenS, LenTmpWideStr: Integer;
    i,J: integer;
    TmpWideStr: WideString;

    {----------------------------------------------------------------}
    Function internalGetCompositeCharSize(aChar: WideString): integer;
    Begin
      Result := FoldStringW(MAP_COMPOSITE, PwideChar(aChar), length(aChar), nil, 0);
    end;

begin
  result := '';
  If s = '' then exit;

  {upper the result}
  LenS := length(S);

  {remove diacritic}
  LenTmpWideStr := FoldStringW(MAP_COMPOSITE, PwideChar(S), LenS, nil, 0);
  setlength(TmpWideStr,LenTmpWideStr);
  FoldStringW(MAP_COMPOSITE, PwideChar(S), LenS, PwideChar(TmpWideStr), LenTmpWideStr);
  i := 1;
  J := 1;
  SetLength(result,lenS);
  while J <= lenS do begin
    Result[j] := TmpWideStr[i];
    if S[j] <> TmpWideStr[i] then inc(i,internalGetCompositeCharSize(S[j])) //some Diacritic can have a length of 3 in composite (ie: U+1E69)
    else inc(i);
    inc(j);
  end;
end;

{********************************************************
 Expand all ligature characters so that they are represented by their two-character equivalent.
 For example, the ligature 'æ' expands to the two characters 'a' and 'e'.}
Function ALWideExpandLigatures(S: Widestring): Widestring;
Const aMAP_EXPAND_LIGATURES = $2000;
var LenS, LenResult: Integer;
begin
  result := '';
  If s = '' then exit;

  {upper the result}
  LenS := length(S);

  {EXPAND LIGATURES}
  LenResult := FoldStringW(aMAP_EXPAND_LIGATURES, PwideChar(S), LenS, nil, 0);
  setlength(Result,LenResult);
  FoldStringW(aMAP_EXPAND_LIGATURES, PwideChar(S), LenS, PwideChar(Result), LenResult);
end;

{*************************************************************}
Function ALWideUpperCaseNoDiacritic(S: Widestring): Widestring;
begin
  Result := ALWideRemoveDiacritic(WideUppercase(s));
end;

{*************************************************************}
Function ALWideLowerCaseNoDiacritic(S: Widestring): Widestring;
begin
  Result := ALWideRemoveDiacritic(Widelowercase(s));
end;

{********************************************************}
Function ALUTF8RemoveDiacritic(S: UTF8String): UTF8String;
begin
  Result := utf8Encode(
                       ALWideRemoveDiacritic(
                                             UTF8Decode(S)
                                            )
                      );
end;

{********************************************************}
Function ALUTF8ExpandLigatures(S: UTF8String): UTF8String;
begin
  Result := utf8Encode(
                       ALWideExpandLigatures(
                                             UTF8Decode(S)
                                            )
                      );
end;

{*************************************************************}
Function ALUTF8UpperCaseNoDiacritic(S: UTF8String): UTF8String;
begin
  Result := utf8Encode(
                       ALWideUpperCaseNoDiacritic(
                                                  UTF8Decode(S)
                                                 )
                      );
end;

{*************************************************************}
Function ALUTF8LowerCaseNoDiacritic(S: UTF8String): UTF8String;
begin
  Result := utf8Encode(
                       ALWideLowerCaseNoDiacritic(
                                                  UTF8Decode(S)
                                                 )
                      );
end;

{**************************************************}
Function ALUTF8Normalize(S: UTF8String): UTF8String;
begin
  Result := utf8Encode(
                       ALWideNormalize(
                                       UTF8Decode(S)
                                      )
                      );
end;

{********************************************************}
function ALUTF8UpperCase(const s: UTF8String): UTF8String;
begin
  result := utf8encode(WideUppercase(utf8Decode(s)));
end;

{********************************************************}
function ALUTF8LowerCase(const s: UTF8String): UTF8String;
begin
  result := utf8encode(WideLowerCase(utf8Decode(s)));
end;

{*********************************************************************}
function AlUTF8DetectBOM(const P: PChar; const Size: Integer): Boolean;
var Q: PChar;
begin
  Result := False;
  if Assigned(P) and (Size >= 3) and (P^ = #$EF) then begin
    Q := P;
    Inc(Q);
    if Q^ = #$BB then begin
      Inc(Q);
      if Q^ = #$BF then Result := True;
    end;
  end;
end;

{****************************************************}
{give on how many bits is encoded the first char in P}
function ALUTF8CharSize(const P: PChar; const Size: Integer): Integer;
var C: Byte;
    I: Integer;
    Q: PChar;
begin
  if not Assigned(P) or (Size <= 0) then begin
    Result := 0;
    exit;
  end;
  C := Ord(P^);
  if C < $80 then Result := 1 // 1-byte (US-ASCII value)
  else if C and $C0 = $80 then Result := 1 // invalid encoding
  else begin
    // multi-byte character
    if C and $20 = 0 then Result := 2
    else if C and $10 = 0 then Result := 3
    else if C and $08 = 0 then Result := 4
    else begin
      Result := 1; // invalid encoding
      exit;
    end;
    if Size < Result then exit; // incomplete encoding
    Q := P;
    Inc(Q);
    For I := 1 to Result - 1 do
      if Ord(Q^) and $C0 <> $80 then begin
          Result := 1; // invalid encoding
          exit;
      end
      else Inc(Q);
  end;
end;

{******************************}
{give on how many char are in P}
function ALUTF8CharCount(const P: PChar; const Size: Integer): Integer;
var Q    : PChar;
    L, C : Integer;
begin
  Q := P;
  L := Size;
  Result := 0;
  While L > 0 do begin
    C := ALUTF8CharSize(Q, L);
    Dec(L, C);
    Inc(Q, C);
    Inc(Result);
  end;
end;

{******************************}
{give on how many char are in P}
function ALUTF8CharCount(const S: UTF8String): Integer;
begin
  Result := ALUTF8CharCount(Pointer(S), Length(S));
end;

{******************************}
{give on how many char are in P}
Function ALUTF8Trunc(s:UTF8string; Count: Integer): UTF8String;
var Q    : PChar;
    L, C, M : Integer;
begin
  L := Length(S);
  If (L = 0) or (Count >= L) then Begin
    Result := S;
    Exit;
  end;

  Q := Pchar(S);
  M := 0;

  While L > 0 do begin
    C := ALUTF8CharSize(Q, L);
    If M + C > Count then break;
    inc(M, C);
    Dec(L, C);
    Inc(Q, C);
  end;

  Result := Copy(S,1,M);
end;

{*****************************}
{Uppercase only the First char}
Function ALUTF8UpperFirstChar(s:UTF8string): UTF8String;
var tmpWideStr: WideString;
begin
  TmpWideStr := UTF8Decode(S);
  result := utf8encode(WideUpperCase(copy(TmpWideStr,1,1)) + copy(TmpWideStr,2,MaxInt));
end;

{***************************************************************}
Function ALUTF8LowerCaseFirstCharUpper(s:UTF8string): UTF8String;
var tmpWideStr: WideString;
begin
  TmpWideStr := UTF8Decode(S);
  result := utf8encode(WideUpperCase(copy(TmpWideStr,1,1)) + WidelowerCase(copy(TmpWideStr,2,MaxInt)));
end;

{****************************************************}
Function ALGetCodePageFromName(Acharset:String): Word;
begin
  Acharset := Trim(LowerCase(ACharset));

  if acharset='utf-8' then result := 65001 // unicode (utf-8)
  else if acharset='iso-8859-1' then result := 28591 // western european (iso)
  else if acharset='iso-8859-2' then result := 28592 // central european (iso)
  else if acharset='iso-8859-3' then result := 28593 // latin 3 (iso)
  else if acharset='iso-8859-4' then result := 28594 // baltic (iso)
  else if acharset='iso-8859-5' then result := 28595 // cyrillic (iso)
  else if acharset='iso-8859-6' then result := 28596 // arabic (iso)
  else if acharset='iso-8859-7' then result := 28597 // greek (iso)
  else if acharset='iso-8859-8' then result := 28598 // hebrew (iso-visual)
  else if acharset='iso-8859-9' then result := 28599 // turkish (iso)
  else if acharset='iso-8859-13' then result := 28603 // estonian (iso)
  else if acharset='iso-8859-15' then result := 28605 // latin 9 (iso)
  else if acharset='ibm037' then result := 37 // ibm ebcdic (us-canada)
  else if acharset='ibm437' then result := 437 // oem united states
  else if acharset='ibm500' then result := 500 // ibm ebcdic (international)
  else if acharset='asmo-708' then result := 708 // arabic (asmo 708)
  else if acharset='dos-720' then result := 720 // arabic (dos)
  else if acharset='ibm737' then result := 737 // greek (dos)
  else if acharset='ibm775' then result := 775 // baltic (dos)
  else if acharset='ibm850' then result := 850 // western european (dos)
  else if acharset='ibm852' then result := 852 // central european (dos)
  else if acharset='ibm855' then result := 855 // oem cyrillic
  else if acharset='ibm857' then result := 857 // turkish (dos)
  else if acharset='ibm00858' then result := 858 // oem multilingual latin i
  else if acharset='ibm860' then result := 860 // portuguese (dos)
  else if acharset='ibm861' then result := 861 // icelandic (dos)
  else if acharset='dos-862' then result := 862 // hebrew (dos)
  else if acharset='ibm863' then result := 863 // french canadian (dos)
  else if acharset='ibm864' then result := 864 // arabic (864)
  else if acharset='ibm865' then result := 865 // nordic (dos)
  else if acharset='cp866' then result := 866 // cyrillic (dos)
  else if acharset='ibm869' then result := 869 // greek, modern (dos)
  else if acharset='ibm870' then result := 870 // ibm ebcdic (multilingual latin-2)
  else if acharset='windows-874' then result := 874 // thai (windows)
  else if acharset='cp875' then result := 875 // ibm ebcdic (greek modern)
  else if acharset='shift_jis' then result := 932 // japanese (shift-jis)
  else if acharset='gb2312' then result := 936 // chinese simplified (gb2312)
  else if acharset='ks_c_5601-1987' then result := 949 // korean
  else if acharset='big5' then result := 950 // chinese traditional (big5)
  else if acharset='ibm1026' then result := 1026 // ibm ebcdic (turkish latin-5)
  else if acharset='ibm01047' then result := 1047 // ibm latin-1
  else if acharset='ibm01140' then result := 1140 // ibm ebcdic (us-canada-euro)
  else if acharset='ibm01141' then result := 1141 // ibm ebcdic (germany-euro)
  else if acharset='ibm01142' then result := 1142 // ibm ebcdic (denmark-norway-euro)
  else if acharset='ibm01143' then result := 1143 // ibm ebcdic (finland-sweden-euro)
  else if acharset='ibm01144' then result := 1144 // ibm ebcdic (italy-euro)
  else if acharset='ibm01145' then result := 1145 // ibm ebcdic (spain-euro)
  else if acharset='ibm01146' then result := 1146 // ibm ebcdic (uk-euro)
  else if acharset='ibm01147' then result := 1147 // ibm ebcdic (france-euro)
  else if acharset='ibm01148' then result := 1148 // ibm ebcdic (international-euro)
  else if acharset='ibm01149' then result := 1149 // ibm ebcdic (icelandic-euro)
  else if acharset='utf-16' then result := 1200 // unicode
  else if acharset='unicodefffe' then result := 1201 // unicode (big-endian)
  else if acharset='windows-1250' then result := 1250 // central european (windows)
  else if acharset='windows-1251' then result := 1251 // cyrillic (windows)
  else if acharset='windows-1252' then result := 1252 // western european (windows)
  else if acharset='windows-1253' then result := 1253 // greek (windows)
  else if acharset='windows-1254' then result := 1254 // turkish (windows)
  else if acharset='windows-1255' then result := 1255 // hebrew (windows)
  else if acharset='windows-1256' then result := 1256 // arabic (windows)
  else if acharset='windows-1257' then result := 1257 // baltic (windows)
  else if acharset='windows-1258' then result := 1258 // vietnamese (windows)
  else if acharset='johab' then result := 1361 // korean (johab)
  else if acharset='macintosh' then result := 10000 // western european (mac)
  else if acharset='x-mac-japanese' then result := 10001 // japanese (mac)
  else if acharset='x-mac-chinesetrad' then result := 10002 // chinese traditional (mac)
  else if acharset='x-mac-korean' then result := 10003 // korean (mac)
  else if acharset='x-mac-arabic' then result := 10004 // arabic (mac)
  else if acharset='x-mac-hebrew' then result := 10005 // hebrew (mac)
  else if acharset='x-mac-greek' then result := 10006 // greek (mac)
  else if acharset='x-mac-cyrillic' then result := 10007 // cyrillic (mac)
  else if acharset='x-mac-chinesesimp' then result := 10008 // chinese simplified (mac)
  else if acharset='x-mac-romanian' then result := 10010 // romanian (mac)
  else if acharset='x-mac-ukrainian' then result := 10017 // ukrainian (mac)
  else if acharset='x-mac-thai' then result := 10021 // thai (mac)
  else if acharset='x-mac-ce' then result := 10029 // central european (mac)
  else if acharset='x-mac-icelandic' then result := 10079 // icelandic (mac)
  else if acharset='x-mac-turkish' then result := 10081 // turkish (mac)
  else if acharset='x-mac-croatian' then result := 10082 // croatian (mac)
  else if acharset='x-chinese-cns' then result := 20000 // chinese traditional (cns)
  else if acharset='x-cp20001' then result := 20001 // tca taiwan
  else if acharset='x-chinese-eten' then result := 20002 // chinese traditional (eten)
  else if acharset='x-cp20003' then result := 20003 // ibm5550 taiwan
  else if acharset='x-cp20004' then result := 20004 // teletext taiwan
  else if acharset='x-cp20005' then result := 20005 // wang taiwan
  else if acharset='x-ia5' then result := 20105 // western european (ia5)
  else if acharset='x-ia5-german' then result := 20106 // german (ia5)
  else if acharset='x-ia5-swedish' then result := 20107 // swedish (ia5)
  else if acharset='x-ia5-norwegian' then result := 20108 // norwegian (ia5)
  else if acharset='us-ascii' then result := 20127 // us-ascii
  else if acharset='x-cp20261' then result := 20261 // t.61
  else if acharset='x-cp20269' then result := 20269 // iso-6937
  else if acharset='ibm273' then result := 20273 // ibm ebcdic (germany)
  else if acharset='ibm277' then result := 20277 // ibm ebcdic (denmark-norway)
  else if acharset='ibm278' then result := 20278 // ibm ebcdic (finland-sweden)
  else if acharset='ibm280' then result := 20280 // ibm ebcdic (italy)
  else if acharset='ibm284' then result := 20284 // ibm ebcdic (spain)
  else if acharset='ibm285' then result := 20285 // ibm ebcdic (uk)
  else if acharset='ibm290' then result := 20290 // ibm ebcdic (japanese katakana)
  else if acharset='ibm297' then result := 20297 // ibm ebcdic (france)
  else if acharset='ibm420' then result := 20420 // ibm ebcdic (arabic)
  else if acharset='ibm423' then result := 20423 // ibm ebcdic (greek)
  else if acharset='ibm424' then result := 20424 // ibm ebcdic (hebrew)
  else if acharset='x-ebcdic-koreanextended' then result := 20833 // ibm ebcdic (korean extended)
  else if acharset='ibm-thai' then result := 20838 // ibm ebcdic (thai)
  else if acharset='koi8-r' then result := 20866 // cyrillic (koi8-r)
  else if acharset='ibm871' then result := 20871 // ibm ebcdic (icelandic)
  else if acharset='ibm880' then result := 20880 // ibm ebcdic (cyrillic russian)
  else if acharset='ibm905' then result := 20905 // ibm ebcdic (turkish)
  else if acharset='ibm00924' then result := 20924 // ibm latin-1
  else if acharset='euc-jp' then result := 20932 // japanese (jis 0208-1990 and 0212-1990)
  else if acharset='x-cp20936' then result := 20936 // chinese simplified (gb2312-80)
  else if acharset='x-cp20949' then result := 20949 // korean wansung
  else if acharset='cp1025' then result := 21025 // ibm ebcdic (cyrillic serbian-bulgarian)
  else if acharset='koi8-u' then result := 21866 // cyrillic (koi8-u)
  else if acharset='x-europa' then result := 29001 // europa
  else if acharset='iso-8859-8-i' then result := 38598 // hebrew (iso-logical)
  else if acharset='iso-2022-jp' then result := 50220 // japanese (jis)
  else if acharset='csiso2022jp' then result := 50221 // japanese (jis-allow 1 byte kana)
  else if acharset='iso-2022-jp' then result := 50222 // japanese (jis-allow 1 byte kana - so/si)
  else if acharset='iso-2022-kr' then result := 50225 // korean (iso)
  else if acharset='x-cp50227' then result := 50227 // chinese simplified (iso-2022)
  else if acharset='euc-jp' then result := 51932 // japanese (euc)
  else if acharset='euc-cn' then result := 51936 // chinese simplified (euc)
  else if acharset='euc-kr' then result := 51949 // korean (euc)
  else if acharset='hz-gb-2312' then result := 52936 // chinese simplified (hz)
  else if acharset='gb18030' then result := 54936 // chinese simplified (gb18030)
  else if acharset='x-iscii-de' then result := 57002 // iscii devanagari
  else if acharset='x-iscii-be' then result := 57003 // iscii bengali
  else if acharset='x-iscii-ta' then result := 57004 // iscii tamil
  else if acharset='x-iscii-te' then result := 57005 // iscii telugu
  else if acharset='x-iscii-as' then result := 57006 // iscii assamese
  else if acharset='x-iscii-or' then result := 57007 // iscii oriya
  else if acharset='x-iscii-ka' then result := 57008 // iscii kannada
  else if acharset='x-iscii-ma' then result := 57009 // iscii malayalam
  else if acharset='x-iscii-gu' then result := 57010 // iscii gujarati
  else if acharset='x-iscii-pa' then result := 57011 // iscii punjabi
  else if acharset='utf-7' then result := 65000 // unicode (utf-7)
  else if acharset='utf-32' then result := 65005 // unicode (utf-32)
  else if acharset='utf-32be' then result := 65006 // unicode (utf-32 big-endian)
  else Result := 0; //Default ansi code page
end;

{**************************************************}
Function ALGetCodePageFromLCID(aLCID:Integer): Word;
Begin
  Case aLCID of
    1025: Result := 1256; //Arabic (Saudi Arabia)
    1026: Result := 1251; //Bulgarian
    1027: Result := 1252; //Catalan
    1028: Result := 950; //Chinese (Taiwan)
    1029: Result := 1250; //Czech
    1030: Result := 1252; //Danish
    1031: Result := 1252; //German (Germany)
    1032: Result := 1253; //Greek
    1033: Result := 1252; //English (United States)
    1034: Result := 1252; //Spanish (Traditional Sort)
    1035: Result := 1252; //Finnish
    1036: Result := 1252; //French (France)
    1037: Result := 1255; //Hebrew
    1038: Result := 1250; //Hungarian
    1039: Result := 1252; //Icelandic
    1040: Result := 1252; //Italian (Italy)
    1041: Result := 932; //Japanese
    1042: Result := 949; //Korean
    1043: Result := 1252; //Dutch (Netherlands)
    1044: Result := 1252; //Norwegian (Bokmal)
    1045: Result := 1250; //Polish
    1046: Result := 1252; //Portuguese (Brazil)
    1048: Result := 1250; //Romanian
    1049: Result := 1251; //Russian
    1050: Result := 1250; //Croatian
    1051: Result := 1250; //Slovak
    1052: Result := 1250; //Albanian
    1053: Result := 1252; //Swedish
    1054: Result := 874; //Thai
    1055: Result := 1254; //Turkish
    1056: Result := 1256; //Urdu
    1057: Result := 1252; //Indonesian
    1058: Result := 1251; //Ukrainian
    1059: Result := 1251; //Belarusian
    1060: Result := 1250; //Slovenian
    1061: Result := 1257; //Estonian
    1062: Result := 1257; //Latvian
    1063: Result := 1257; //Lithuanian
    1065: Result := 1256; //Farsi
    1066: Result := 1258; //Vietnamese
    1068: Result := 1254; //Azeri (Latin)
    1069: Result := 1252; //Basque
    1071: Result := 1251; //FYRO Macedonian
    1078: Result := 1252; //Afrikaans
    1080: Result := 1252; //Faroese
    1086: Result := 1252; //Malay (Malaysia)
    1087: Result := 1251; //Kazakh
    1088: Result := 1251; //Kyrgyz (Cyrillic)
    1089: Result := 1252; //Swahili
    1091: Result := 1254; //Uzbek (Latin)
    1092: Result := 1251; //Tatar
    1104: Result := 1251; //Mongolian (Cyrillic)
    1110: Result := 1252; //Galician
    2049: Result := 1256; //Arabic (Iraq)
    2052: Result := 936; //Chinese (PRC)
    2055: Result := 1252; //German (Switzerland)
    2057: Result := 1252; //English (United Kingdom)
    2058: Result := 1252; //Spanish (Mexico)
    2060: Result := 1252; //French (Belgium)
    2064: Result := 1252; //Italian (Switzerland)
    2067: Result := 1252; //Dutch (Belgium)
    2068: Result := 1252; //Norwegian (Nynorsk)
    2070: Result := 1252; //Portuguese (Portugal)
    2074: Result := 1250; //Serbian (Latin)
    2077: Result := 1252; //Swedish (Finland)
    2092: Result := 1251; //Azeri (Cyrillic)
    2110: Result := 1252; //Malay (Brunei Darussalam)
    2115: Result := 1251; //Uzbek (Cyrillic)
    3073: Result := 1256; //Arabic (Egypt)
    3076: Result := 950; //Chinese (Hong Kong S.A.R.)
    3079: Result := 1252; //German (Austria)
    3081: Result := 1252; //English (Australia)
    3082: Result := 1252; //Spanish (International Sort)
    3084: Result := 1252; //French (Canada)
    3098: Result := 1251; //Serbian (Cyrillic)
    4097: Result := 1256; //Arabic (Libya)
    4100: Result := 936; //Chinese (Singapore)
    4103: Result := 1252; //German (Luxembourg)
    4105: Result := 1252; //English (Canada)
    4106: Result := 1252; //Spanish (Guatemala)
    4108: Result := 1252; //French (Switzerland)
    5121: Result := 1256; //Arabic (Algeria)
    5124: Result := 950; //Chinese (Macau S.A.R.)
    5127: Result := 1252; //German (Liechtenstein)
    5129: Result := 1252; //English (New Zealand)
    5130: Result := 1252; //Spanish (Costa Rica)
    5132: Result := 1252; //French (Luxembourg)
    6145: Result := 1256; //Arabic (Morocco)
    6153: Result := 1252; //English (Ireland)
    6154: Result := 1252; //Spanish (Panama)
    6156: Result := 1252; //French (Monaco)
    7169: Result := 1256; //Arabic (Tunisia)
    7177: Result := 1252; //English (South Africa)
    7178: Result := 1252; //Spanish (Dominican Republic)
    8193: Result := 1256; //Arabic (Oman)
    8201: Result := 1252; //English (Jamaica)
    8202: Result := 1252; //Spanish (Venezuela)
    9217: Result := 1256; //Arabic (Yemen)
    9225: Result := 1252; //English (Caribbean)
    9226: Result := 1252; //Spanish (Colombia)
    10241: Result := 1256; //Arabic (Syria)
    10249: Result := 1252; //English (Belize)
    10250: Result := 1252; //Spanish (Peru)
    11265: Result := 1256; //Arabic (Jordan)
    11273: Result := 1252; //English (Trinidad)
    11274: Result := 1252; //Spanish (Argentina)
    12289: Result := 1256; //Arabic (Lebanon)
    12297: Result := 1252; //English (Zimbabwe)
    12298: Result := 1252; //Spanish (Ecuador)
    13313: Result := 1256; //Arabic (Kuwait)
    13321: Result := 1252; //English (Philippines)
    13322: Result := 1252; //Spanish (Chile)
    14337: Result := 1256; //Arabic (U.A.E.)
    14346: Result := 1252; //Spanish (Uruguay)
    15361: Result := 1256; //Arabic (Bahrain)
    15370: Result := 1252; //Spanish (Paraguay)
    16385: Result := 1256; //Arabic (Qatar)
    16394: Result := 1252; //Spanish (Bolivia)
    17418: Result := 1252; //Spanish (El Salvador)
    18442: Result := 1252; //Spanish (Honduras)
    19466: Result := 1252; //Spanish (Nicaragua)
    20490: Result := 1252; //Spanish (Puerto Rico)
    else Result := 0; //Default ansi code page
  end;
end;

{**************************************************************************}
Function ALStringToWideString(const S: string; aCodePage: Word): WideString;
var InputLength,
    OutputLength: Integer;
begin
  InputLength := Length(S);
  OutputLength := MultiByteToWideChar(aCodePage, 0, PChar(S), InputLength, nil, 0);
  SetLength(Result, OutputLength);
  MultiByteToWideChar(aCodePage, 0, PChar(S), InputLength, PWideChar(Result), OutputLength);
end;

{***************************************************************************}
function AlWideStringToString(const WS: WideString; aCodePage: Word): string;
var InputLength,
    OutputLength: Integer;
begin
  InputLength := Length(WS);
  OutputLength := WideCharToMultiByte(aCodePage, 0, PWideChar(WS), InputLength, nil, 0, nil, nil);
  SetLength(Result, OutputLength);
  WideCharToMultiByte(aCodePage, 0, PWideChar(WS), InputLength, PChar(Result), OutputLength, nil, nil);
end;

{******************************************************************}
Function ALUTF8Encode(const S: string; aCodePage: Word): UTF8String;
begin
  Result := UTF8Encode(ALStringToWideString(S, aCodePage));
end;

{******************************************************************}
Function ALUTF8decode(const S: UTF8String; aCodePage: Word): String;
begin
  Result := ALWideStringToString(UTF8Decode(S), aCodePage);
end;

{****************************************************************************}
Function ALUTF8ISO91995CyrillicToLatin(aCyrillicText: UTF8String): UTF8String;
Var aCyrillicWideStr: WideString;
    aLatinWideStr: WideString;
    aLatinWideStrCurrentPos: Integer;

  {-----------------------------------------------------------------}
  Procedure InternalAddCharsToResult(aArrayOfChar: Array of Integer);
  var i: integer;
  begin
    for i := low(aArrayOfChar) to high(aArrayOfChar) do begin
      inc(aLatinWideStrCurrentPos);
      aLatinWideStr[aLatinWideStrCurrentPos] := WideChar(aArrayOfChar[i]);
    end;
  end;

Var I, j: Integer;
Begin

{ set of transliteration rules }
{ Hexnotation of Unicode codepoints, letter names according to Unicode Standard 5.0 }
{ Abbreviations:
    CCL = CYRILLIC CAPITAL LETTER
    CSL = CYRILLIC SMALL LETTER
    LCL = LATIN CAPITAL LETTER
    LSL = LATIN SMALL LETTER }

  Result := '';
  aCyrillicWideStr := UTF8Decode(aCyrillicText);
  setlength(ALatinWideStr,length(aCyrillicWideStr) * 2); //to Be on the safe way
  aLatinWideStrCurrentPos := 0;
  for i := 1 to length(aCyrillicWideStr) do begin
    j := ord(aCyrillicWideStr[i]);
    case j of
      $0410 { CCL A } : InternalAddCharsToResult([$0041]); {LCL A }
      $0430 { CSL A } : InternalAddCharsToResult([$0061]); {LSL A }
      $04D0 { CCL A with BREVE } : InternalAddCharsToResult([$0102]); {LCL A with BREVE }
      $04D1 { CSL A WITH BREVE } : InternalAddCharsToResult([$0103]); {LSL A WITH BREVE }
      $04D2 { CCL A WITH DIAERESIS } : InternalAddCharsToResult([$00C4]); {LCL A WITH DIAERESIS }
      $04D3 { CSL A WITH DIAERESIS } : InternalAddCharsToResult([$00E4]); {LSL A WITH DIAERESIS }
      $04D8 { CCL SCHWA } : InternalAddCharsToResult([$0041,$030B]); {LCL A WITH COMBINING DOUBLE ACUTE }
      $04D9 { CSL SCHWA } : InternalAddCharsToResult([$0061,$030B]); {LSL A WITH COMBINING DOUBLE ACUTE }
      $0411 { CCL BE } : InternalAddCharsToResult([$0042]); {LCL B }
      $0431 { CSL BE } : InternalAddCharsToResult([$0062]); {LSL B }
      $0412 { CCL VE } : InternalAddCharsToResult([$0056]); {LCL V }
      $0432 { CSL VE } : InternalAddCharsToResult([$0076]); {LSL V }
      $0413 { CCL GHE } : InternalAddCharsToResult([$0047]); {LCL G }
      $0433 { CSL GHE } : InternalAddCharsToResult([$0067]); {LSL G }
      $0490 { CCL GHE WITH UPTURN } : InternalAddCharsToResult([$0047,$0300]); {LCL G WITH COMBINING GRAVE ACCENT }
      $0491 { CSL GHE WITH UPTURN } : InternalAddCharsToResult([$0067,$0300]); {LCL G WITH COMBINING GRAVE ACCENT }
      $0494 { CCL GHE WITH MIDDLE HOOK } : InternalAddCharsToResult([$011E]); {LCL G WITH BREVE }
      $0495 { CSL GHE WITH MIDDLE HOOK } : InternalAddCharsToResult([$011F]); {LSL G WITH BREVE }
      $0492 { CCL GHE WITH STROKE } : InternalAddCharsToResult([$0120]); {LCL G WITH DOT ABOVE }
      $0493 { CSL GHE WITH STROKE } : InternalAddCharsToResult([$0121]); {LSL G WITHDOT ABOVE }
      $0414 { CCL DE } : InternalAddCharsToResult([$0044]); {LCL D }
      $0434 { CSL DE } : InternalAddCharsToResult([$0064]); {LSL D }
      $0402 { CCL DJE } : InternalAddCharsToResult([$0110]); {LCL D WITH STROKE }
      $0452 { CSL DJE } : InternalAddCharsToResult([$0111]); {LSL D WITH STROKE }
      $0403 { CCL GJE } : InternalAddCharsToResult([$01F4]); {LCL G WITH ACUTE }
      $0453 { CSL GJE } : InternalAddCharsToResult([$01F5]); {LSL G WITH ACUTE }
      $0415 { CCL IE } : InternalAddCharsToResult([$0045]); {LCL E }
      $0435 { CSL IE } : InternalAddCharsToResult([$0065]); {LSL E }
      $0401 { CCL IO } : InternalAddCharsToResult([$00CB]); {LCL E WITH DIAERESIS }
      $0451 { CSL IO } : InternalAddCharsToResult([$00EB]); {LSL E WITH DIAERESIS }
      $04D6 { CCL IE WITH BREVE } : InternalAddCharsToResult([$0114]); {LCL E WITH BREVE }
      $04D7 { CSL IE WITH BREVE } : InternalAddCharsToResult([$0115]); {LSL E WITH BREVE }
      $0404 { CCL UKRAINIAN IE } : InternalAddCharsToResult([$00CA]); {LCL E WITH CIRCUMFLEX }
      $0454 { CSL UKRAINIAN IE } : InternalAddCharsToResult([$00EA]); {LSL E WITH CIRCUMFLEX }
      $04BC { CCL ABKHASIAN CHE } : InternalAddCharsToResult([$0043,$0306]); {LCL C WITH COMBINING BREVE }
      $04BD { CSL ABKHASIAN CHE } : InternalAddCharsToResult([$0063,$0306]); {LSL C WITH COMBINING BREVE }
      $04BE { CCL ABKHASIAN CHE WITH DESCENDER } : InternalAddCharsToResult([$00C7,$0306]); {LCL C WITH CEDILLA & COMBINING BREVE }
      $04BF { CSL ABKHASIAN CHE WITH DESCENDER } : InternalAddCharsToResult([$00E7,$0306]); {LSL C WITH CEDILLA & COMBINING BREVE }
      $0416 { CCL ZHE } : InternalAddCharsToResult([$017D]); {LCL Z WITH CARON }
      $0436 { CSL ZHE } : InternalAddCharsToResult([$017E]); {LSL Z WITH CARON }
      $04C1 { CCL ZHE WITH BREVE } : InternalAddCharsToResult([$005A,$0306]); {LCL Z WITH COMBINING BREVE }
      $04C2 { CSL ZHE WITH BREVE } : InternalAddCharsToResult([$007A,$0306]); {LSL Z WITH COMBINING BREVE }
      $04DC { CCL ZHE WITH DIAERESIS } : InternalAddCharsToResult([$005A,$0304]); {LCL Z WITH COMBINING MACRON }
      $04DD { CSL ZHE WITH DIAERESIS } : InternalAddCharsToResult([$007A,$0304]); {LSL Z WITH COMBINING MACRON }
      $0496 { CCL ZHE WITH DESCENDER } : InternalAddCharsToResult([$017D,$0326]); {LCL Z WITH CARON & COMBINDING COMMA BELOW }
      $0497 { CSL ZHE WITH DESCENDER } : InternalAddCharsToResult([$017E,$0327]); {LSL Z WITH CARON & COMBINDING CEDILLA }
      $0417 { CCL ZE } : InternalAddCharsToResult([$005A]); {LCL Z }
      $0437 { CSL ZE } : InternalAddCharsToResult([$007A]); {LSL Z }
      $04DE { CCL ZE WITH DIAERESIS } : InternalAddCharsToResult([$005A,$0308]); {LCL Z WITH COMBINING DIAERESIS }
      $04DF { CSL ZE WITH DIAERESIS } : InternalAddCharsToResult([$007A,$0308]); {LSL Z WITH COMBINING DIAERESIS }
      $0405 { CCL DZE } : InternalAddCharsToResult([$1E90]); {LCL Z WITH CIRCUMFLEX }
      $0455 { CSL DZE } : InternalAddCharsToResult([$1E91]); {LSL WITH CIRCUMFLEX }
      $04E0 { CCL ABKHASIAN DZE } : InternalAddCharsToResult([$0179]); {LCL Z WITH ACUTE }
      $04E1 { CSL ABKHASIAN DZE } : InternalAddCharsToResult([$017A]); {LSL Z WITH ACUTE }
      $0418 { CCL I } : InternalAddCharsToResult([$0049]); {LCL I }
      $0438 { CSL I } : InternalAddCharsToResult([$0069]); {LSL I }
      $04E4 { CCL I WITH DIAERESIS } : InternalAddCharsToResult([$00CE]); {LCL I WITH CIRCUMFLEX }
      $04E5 { CSL I WITH DIAERESIS } : InternalAddCharsToResult([$00EE]); {LSL I WITH CIRCUMFLEX }
      $0406 { CCL BYELORUSSIAN-UKRAINIAN I } : InternalAddCharsToResult([$00CC]); {LCL I WITH GRAVE }
      $0456 { CSL BYELORUSSIAN-UKRAINIAN I } : InternalAddCharsToResult([$00EC]); {LSL WITH GRAVE }
      $0407 { CCL YI } : InternalAddCharsToResult([$00CF]); {LCL I WITH DIAERESIS }
      $0457 { CSL YI } : InternalAddCharsToResult([$00EF]); {LSL I WITH DIAERESIS }
      $0419 { CCL SHORT I } : InternalAddCharsToResult([$004A]); {LCL J }
      $0439 { CSL SHORT I } : InternalAddCharsToResult([$006A]); {LSL J }
      $0408 { CCL JE } : InternalAddCharsToResult([$004A,$030C]); {LCL J WITH COMBINING CARON }
      $0458 { CSL JE } : InternalAddCharsToResult([$01F0]); {LSL J WITH CARON }
      $041A { CCL KA } : InternalAddCharsToResult([$004B]); {LCL K }
      $043A { CSL KA } : InternalAddCharsToResult([$006B]); {LSL K }
      $049A { CCL KA WITH DESCENDER } : InternalAddCharsToResult([$0136]); {LCL K WITH CEDILLA }
      $049B { CSL KA WITH DESCENDER } : InternalAddCharsToResult([$0137]); {LSL K WITH CEDILLA }
      $049E { CCL KA WITH STROKE } : InternalAddCharsToResult([$004B,$0304]); {LCL K WITH COMBINING MACRON }
      $049F { CSL KA WITH STROKE } : InternalAddCharsToResult([$006B,$0304]); {LSL K WITH COMBINING MACRON }
      $041B { CCL EL } : InternalAddCharsToResult([$004C]); {LCL L }
      $043B { CSL EL } : InternalAddCharsToResult([$006C]); {LSL L }
      $0409 { CCL LJE } : InternalAddCharsToResult([$004C,$0302]); {LCL L WITH COMBINING CIRCUMFLEX }
      $0459 { CSL LJE } : InternalAddCharsToResult([$006C,$0302]); {LSL L WITH COMBINING CIRCUMFLEX }
      $041C { CCL EM } : InternalAddCharsToResult([$004D]); {LCL M }
      $043C { CSL EM } : InternalAddCharsToResult([$006D]); {LSL M }
      $041D { CCL EN } : InternalAddCharsToResult([$004E]); {LCL N }
      $043D { CSL EN } : InternalAddCharsToResult([$006E]); {LSL N }
      $040A { CCL NJE } : InternalAddCharsToResult([$004E,$0302]); {LCL N WITH COMBINING CIRCUMFLEX }
      $045A { CSL NJE } : InternalAddCharsToResult([$006E,$0302]); {LSL N WITH COMBINING CIRCUMFLEX }
      $04A4 { CC LIGATURE EN GHE } : InternalAddCharsToResult([$1E44]); {LCL N WITH DOT ABOVE }
      $04A5 { CS LIGATURE EN GHE } : InternalAddCharsToResult([$1E45]); {LSL N WITH DOT ABOVE }
      $04A2 { CCL EN WITH DESCENDER } : InternalAddCharsToResult([$1E46]); {LCL N WITH DOT BELOW }
      $04A3 { CSL EN WITH DESCENDER } : InternalAddCharsToResult([$1E47]); {LSL N WITH DOT BELOW }
      $041E { CCL O } : InternalAddCharsToResult([$004F]); {LCL O }
      $043E { CSL O } : InternalAddCharsToResult([$006F]); {LSL O }
      $04E6 { CCL O WITH DIAERESIS } : InternalAddCharsToResult([$00D6]); {LCL O WITH DIAERESIS }
      $04E7 { CSL O WITH DIAERESIS } : InternalAddCharsToResult([$00F6]); {LSL O WITH DIAERESIS }
      $04E8 { CCL BARRED O } : InternalAddCharsToResult([$00D4]); {LCL WITH CIRCUMFLEX }
      $04E9 { CSL BARRED O } : InternalAddCharsToResult([$00F4]); {LSL WITH CIRCUMFLEX }
      $041F { CCL PE } : InternalAddCharsToResult([$0050]); {LCL P }
      $043F { CSL PE } : InternalAddCharsToResult([$0070]); {LSL P }
      $04A6 { CCL PE WITH MIDDLE HOOK } : InternalAddCharsToResult([$1E54]); {LCL P WITH ACUTE }
      $04A7 { CSL PE WITH MIDDLE HOOK } : InternalAddCharsToResult([$1E55]); {LSL P WITH ACUTE }
      $0420 { CCL ER } : InternalAddCharsToResult([$0052]); {LCL R }
      $0440 { CSL ER } : InternalAddCharsToResult([$0072]); {LCL R }
      $0421 { CCL ES } : InternalAddCharsToResult([$0053]); {LCL S }
      $0441 { CSL ES } : InternalAddCharsToResult([$0073]); {LSL S }
      $04AA { CCL ES WITH DESCENDER } : InternalAddCharsToResult([$00C7]); {LCL C WITH CEDILLA }
      $04AB { CSL ES WITH DESCENDER } : InternalAddCharsToResult([$00E7]); {LSL C WITH CEDILLA }
      $0422 { CCL TE } : InternalAddCharsToResult([$0054]); {LCL T }
      $0442 { CSL TE } : InternalAddCharsToResult([$0074]); {LSL T }
      $04AC { CCL TE WITH DESCENDER } : InternalAddCharsToResult([$0162]); {LCL T WITH CEDILLA }
      $04AD { CSL TE WITH DESCENDER } : InternalAddCharsToResult([$0163]); {LSL T WITH CEDILLA }
      $040B { CCL TSHE } : InternalAddCharsToResult([$0106]); {LCL C WITH ACUTE }
      $045B { CSL TSHE } : InternalAddCharsToResult([$0170]); {LSL C WITH ACUTE }
      $040C { CCL KJE } : InternalAddCharsToResult([$1E30]); {LCL K WITH ACUTE }
      $045C { CSL KJE } : InternalAddCharsToResult([$1E31]); {LSL K WITH ACUTE }
      $0423 { CCL U } : InternalAddCharsToResult([$0055]); {LCL U }
      $0443 { CSL U } : InternalAddCharsToResult([$0075]); {LSL U }
      $EE19 { CCL U WITH COMBINING ACUTE } : InternalAddCharsToResult([$00DA]); {LCL U WITH ACUTE }
      $EE99 { CSL U WITH COMBINING ACUTE } : InternalAddCharsToResult([$00FA]); {LSL U WITH ACUTE }
      $040E { CCL SHORT U } : InternalAddCharsToResult([$016C]); {LCL U WITH BREVE }
      $045E { CSL SHORT U } : InternalAddCharsToResult([$016D]); {LCL U WITH BREVE }
      $04F0 { CCL U WITH DIAERESIS } : InternalAddCharsToResult([$00DC]); {LCL U WITH DIAERESIS }
      $04F1 { CSL U WITH DIAERESIS } : InternalAddCharsToResult([$00FC]); {LSL U WITH DIAERESIS }
      $04F2 { CCL U WITH DOUBLE ACUTE } : InternalAddCharsToResult([$0170]); {LCL U WITH DOUBLE ACUTE }
      $04F3 { CSL U WITH DOUBLE ACUTE } : InternalAddCharsToResult([$0171]); {LSL U WITH DOUBLE ACUTE }
      $04AE { CCL STRAIGHT U } : InternalAddCharsToResult([$00D9]); {LCL U WITH GRAVE }
      $04AF { CSL STRAIGHT U } : InternalAddCharsToResult([$00F9]); {LSL U WITH GRAVE }
      $0424 { CCL EF } : InternalAddCharsToResult([$0046]); {LCL F }
      $0444 { CSL EF } : InternalAddCharsToResult([$0066]); {LSL F }
      $0425 { CCL HA } : InternalAddCharsToResult([$0048]); {LCL H }
      $0445 { CSL HA } : InternalAddCharsToResult([$0068]); {LSL H }
      $04B2 { CCL HA WITH DESCENDER } : InternalAddCharsToResult([$1E28]); {LCL H WITH CEDILLA }
      $04B3 { CSL HA WITH DESCENDER } : InternalAddCharsToResult([$1E29]); {LSL H WITH CEDILLA }
      $04BA { CCL SHHA } : InternalAddCharsToResult([$1E24]); {LCL H WITH DOT BELOW }
      $04BB { CSL SHHA } : InternalAddCharsToResult([$1E25]); {LSL H WITH DOT BELOW }
      $0426 { CCL TSE } : InternalAddCharsToResult([$0043]); {LCL C }
      $0446 { CSL TSE } : InternalAddCharsToResult([$0063]); {LSL C }
      $04B4 { CC LIGATURE TE TSE } : InternalAddCharsToResult([$0043,$0304]); {LCL C WITH COMBINING MACRON }
      $04B5 { CS LIGATURE TE TSE } : InternalAddCharsToResult([$0063,$0304]); {LSL C WITH COMBINING MACRON }
      $0427 { CCL CHE } : InternalAddCharsToResult([$010C]); {LCL C WITH CARON }
      $0447 { CSL CHE } : InternalAddCharsToResult([$010D]); {LSL C WITH CARON }
      $04F4 { CCL CHE WITH DIAERESIS } : InternalAddCharsToResult([$0043,$0308]); {LCL C WITH COMBINING DIAERESIS }
      $04F5 { CSL CHE WITH DIAERESIS } : InternalAddCharsToResult([$0063,$0308]); {LSL C WITH COMBINING DIAERESIS }
      $04CB { CCL KHAKASSIAN CHE } : InternalAddCharsToResult([$00C7]); {LCL C WITH CEDILLA }
      $04CC { CSL KHAKASSIAN CHE } : InternalAddCharsToResult([$00E7]); {LSL C WITH CEDILLA }
      $040F { CCL DZHE } : InternalAddCharsToResult([$0044,$0302]); {LCL D WITH COMBINING CIRCUMFLEX }
      $045F { CSL DZHE } : InternalAddCharsToResult([$0064,$0302]); {LSL D WITH COMBINING CIRCUMFLEX }
      $0428 { CCL SHA } : InternalAddCharsToResult([$0160]); {LCL S WITH CARON }
      $0448 { CSL SHA } : InternalAddCharsToResult([$0161]); {LSL S WITH CARON }
      $0429 { CCL SHCHA } : InternalAddCharsToResult([$015C]); {LCL S WITH CIRCUMFLEX }
      $0449 { CSL SHCHA } : InternalAddCharsToResult([$015D]); {LSL S WITH CIRCUMFLEX }
      $042A { CCL HARD SIGN } : InternalAddCharsToResult([$02BA]); {MODIFIER LETTER DOUBLE PRIME }
      {The capital hard sign is very seldom in use. It may be capital, if everything is written in upper case - therefore when transliteration is used reverse we can say that $02BA = $042A if everything is written in upper case}
      $044A { CSL HARD SIGN } : InternalAddCharsToResult([$02BA]); {MODIFIER LETTER DOUBLE PRIME }
      $02BC { MODIFIER LETTER APOSTROPHE } : InternalAddCharsToResult([$2019]); {RIGHT SINGLE QUOTATION MARK }
      $042B { CCL YERU } : InternalAddCharsToResult([$0059]); {LCL Y }
      $044B { CSL YERU } : InternalAddCharsToResult([$0079]); {LSL Y }
      $04F8 { CCL YERU WITH DIAERESIS } : InternalAddCharsToResult([$0178]); {LCL Y WITH DIAERESIS }
      $04F9 { CSL YERU WITH DIAERESIS } : InternalAddCharsToResult([$00FF]); {LSL Y WITH DIAERESIS }
      $042C { CCL SOFT SIGN } : InternalAddCharsToResult([$02B9]); {MODIFIER LETTER PRIME }
      {The capital hard sign is very seldom in use. It may be capital, if everything is written in upper case - therefore when transliteration is used reverse we can say that $042C = $02B9 if everything is written in upper case}
      $044C { CSL SOFT SIGN } : InternalAddCharsToResult([$02B9]); {MODIFIER LETTER PRIME }
      $042D { CCL E } : InternalAddCharsToResult([$00C8]); {LCL E WITH GRAVE }
      $044D { CSL E } : InternalAddCharsToResult([$00E8]); {LSL E WITH GRAVE }
      $042E { CCL YU } : InternalAddCharsToResult([$00DB]); {LCL U WITH CIRCUMFLEX }
      $044E { CSL YU } : InternalAddCharsToResult([$00FB]); {LSL U WITH CIRCUMFLEX }
      $042F { CCL YA } : InternalAddCharsToResult([$00C2]); {LCL A WITH CIRCUMFLEX }
      $044F { CSL YA } : InternalAddCharsToResult([$00E2]); {LSL A WITH CIRCUMFLEX }
      $048C { CCL SEMISOFT SIGN } : InternalAddCharsToResult([$011A]); {LCL E WITH CARON }
      $048D { CSL SEMISOFT SIGN } : InternalAddCharsToResult([$011B]); {LSL E WITH CARON }
      $046A { CCL BIG YUS (HISTORICAL) } : InternalAddCharsToResult([$01CD]); {LCL A  WITH CARON }
      $046B { CSL BID YUS (HISTORICAL) } : InternalAddCharsToResult([$01CE]); {LSL A WITH CARON }
      $0472 { CCL FITA (HISTORICAL) } : InternalAddCharsToResult([$0046,$0300]); {LCL F WITH COMBINING GRAVE }
      $0473 { CSL FITA (HISTORICAL) } : InternalAddCharsToResult([$0066,$0300]); {LSL F WITH COMBINING GRAVE }
      $0474 { CCL IZHITSA (HISTORICAL) } : InternalAddCharsToResult([$1EF2]); {LCL Y WITH GRAVE }
      $0475 { CSL IZHITSA (HISTORICAL) } : InternalAddCharsToResult([$1EF3]); {LSL Y WITH GRAVE }
      $04A8 { CCL ABKHASIAN HA } : InternalAddCharsToResult([$00D2]); {LCL  O WITH GRAVE }
      $04A9 { CSL ABKHASIAN HA } : InternalAddCharsToResult([$00F2]); {LSL O WITH GRAVE }
      $04C0 { CCL PALOCHKA } : InternalAddCharsToResult([$2021]); {DOUBLE DAGGER }
      else InternalAddCharsToResult([j]);
    end;
  end;
  SetLength(aLatinWideStr,aLatinWideStrCurrentPos);
  Result := UTF8Encode(aLatinWideStr);
End;

end.
