{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      Alcinoe Unicode Functions
Version:      3.55

Description:  This unit contains a Unicode support library along
              with some additional files to use WideStrings/Unicode
              strings within your application.

Legal issues: Copyright (C) 1999-2010 by Arkadia Software Engineering

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
              15/03/2010: update ALUTF8LowerCaseFirstCharUpper to return
                          jean-pierre => Jean-Pierre instead of Jean-pierre
                          mandelieu la napoule => Mandelieu La Napoule instead of Mandelieu la napoule
                          arkadia france => Arkadia France instead of Arkadia france
              05/04/2010: update ALUTF8Trunc to ALUTF8ASCIITrunc and ALUTF8UnicodeTrunc
                          Where the first just trunc the string to n ascii char and the
                          second trunc the string to n unicode char

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
function AlUTF8removeBOM(S: UTF8String): String;
function AlUTF8DetectBOM(const P: PChar; const Size: Integer): Boolean;
function ALUTF8CharSize(const P: PChar; const Size: Integer): Integer;
function ALUTF8CharCount(const P: PChar; const Size: Integer): Integer; overload;
function ALUTF8CharCount(const S: Utf8String): Integer; overload;
Function ALUTF8UnicodeTrunc(s:UTF8string; Count: Integer): UTF8String;
Function ALUTF8AsciiTrunc(s:UTF8string; Count: Integer): UTF8String;
Function ALUTF8UpperFirstChar(s:UTF8string): UTF8String;
Function ALUTF8LowerCaseFirstCharUpper(s:UTF8string): UTF8String;
Function ALStringToWideString(const S: string; aCodePage: Word): WideString;
function AlWideStringToString(const WS: WideString; aCodePage: Word): string;
Function ALUTF8Encode(const S: string; aCodePage: Word): UTF8String;
Function ALUTF8decode(const S: UTF8String; aCodePage: Word): String;
Function ALGetCodePageFromName(Acharset:String): Word;
Function ALGetCodePageFromLCID(aLCID:Integer): Word;
Function ALUTF8ISO91995CyrillicToLatin(aCyrillicText: UTF8String): UTF8String;
Function ALUTF8BGNPCGN1947CyrillicToLatin(aCyrillicText: UTF8String): UTF8String;

Const cAlUTF8Bom = #$EF#$BB#$BF;
      cAlUTF16LittleEndianBom = #$FF#$FE;
      cAlUTF16bigEndianBom = #$FE#$FF;
      cAlUTF32LittleEndianBom = #$FF#$FE#$00#$00;
      cAlUTF32BigEndianBom = #$00#$00#$FE#$FF;

implementation

uses SysUtils,
     StrUtils,
     AlFcnString;

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

{**********************************************}
function AlUTF8removeBOM(S: UTF8String): String;
begin
  if AlUTF8DetectBOM(Pchar(S), length(S)) then result := AlCopyStr(S,length(cAlUTF8BOM) + 1,Maxint)
  else Result := S;
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

{**************************************}
{Trunc a UTF8string to count ascii char}
Function ALUTF8AscIITrunc(s:UTF8string; Count: Integer): UTF8String;
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

  Result := ALCopyStr(S,1,M);
end;

{****************************************}
{Trunc a UTF8string to count unicode char}
Function ALUTF8UnicodeTrunc(s:UTF8string; Count: Integer): UTF8String;
var tmpWideStr: WideString;
begin
  TmpWideStr := UTF8Decode(S);
  result := utf8encode(copy(TmpWideStr,1,Count));
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
    i: integer;
begin
  TmpWideStr := UTF8Decode(S);
  tmpWideStr := Trim(TmpWideStr);
  if length(TmpWideStr) = 0 then begin
    result := '';
    exit;
  end;
  TmpWideStr := WideUpperCase(copy(TmpWideStr,1,1)) + WidelowerCase(copy(TmpWideStr,2,MaxInt));
  for i:= 2 to length(TmpWideStr) do
    if (TmpWideStr[i-1] in [WideChar(' '),WideChar('-'),WideChar('''')]) and
       (
        (i >= length(TmpWideStr)) or
        (not (TmpWideStr[i+1] in [WideChar(' '),WideChar('-'),WideChar('''')]))
       )
    then TmpWideStr[i] := WideUpperCase(TmpWideStr[i])[1];
  result := utf8encode(TmpWideStr);
end;

{****************************************************}
Function ALGetCodePageFromName(Acharset:String): Word;
begin
  Acharset := Trim(AlLowerCase(ACharset));

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
  Result := '';
  aCyrillicWideStr := UTF8Decode(aCyrillicText);
  setlength(ALatinWideStr,length(aCyrillicWideStr) * 2); //to Be on the safe way
  aLatinWideStrCurrentPos := 0;
  for i := 1 to length(aCyrillicWideStr) do begin
    j := ord(aCyrillicWideStr[i]);
    case j of
      $0410 {А} : InternalAddCharsToResult([$0041]); {A}
      $0430 {а} : InternalAddCharsToResult([$0061]); {a}
      $04D0 {Ӑ} : InternalAddCharsToResult([$0102]); {Ă}
      $04D1 {ӑ} : InternalAddCharsToResult([$0103]); {ă}
      $04D2 {Ӓ} : InternalAddCharsToResult([$00C4]); {Ä}
      $04D3 {ӓ} : InternalAddCharsToResult([$00E4]); {ä}
      $04D8 {Ә} : InternalAddCharsToResult([$0041,$030B]); {A̋}
      $04D9 {ә} : InternalAddCharsToResult([$0061,$030B]); {a̋}
      $0411 {Б} : InternalAddCharsToResult([$0042]); {B}
      $0431 {б} : InternalAddCharsToResult([$0062]); {b}
      $0412 {В} : InternalAddCharsToResult([$0056]); {V}
      $0432 {в} : InternalAddCharsToResult([$0076]); {v}
      $0413 {Г} : InternalAddCharsToResult([$0047]); {G}
      $0433 {г} : InternalAddCharsToResult([$0067]); {g}
      $0490 {Ґ} : InternalAddCharsToResult([$0047,$0300]); {G̀}
      $0491 {ґ} : InternalAddCharsToResult([$0067,$0300]); {g̀}
      $0494 {Ҕ} : InternalAddCharsToResult([$011E]); {Ğ}
      $0495 {ҕ} : InternalAddCharsToResult([$011F]); {ğ}
      $0492 {Ғ} : InternalAddCharsToResult([$0120]); {Ġ}
      $0493 {ғ} : InternalAddCharsToResult([$0121]); {ġ}
      $0414 {Д} : InternalAddCharsToResult([$0044]); {D}
      $0434 {д} : InternalAddCharsToResult([$0064]); {d}
      $0402 {Ђ} : InternalAddCharsToResult([$0110]); {Đ}
      $0452 {ђ} : InternalAddCharsToResult([$0111]); {đ}
      $0403 {Ѓ} : InternalAddCharsToResult([$01F4]); {Ǵ}
      $0453 {ѓ} : InternalAddCharsToResult([$01F5]); {ǵ}
      $0415 {Е} : InternalAddCharsToResult([$0045]); {E}
      $0435 {е} : InternalAddCharsToResult([$0065]); {e}
      $0401 {Ё} : InternalAddCharsToResult([$00CB]); {Ë}
      $0451 {ё} : InternalAddCharsToResult([$00EB]); {ë}
      $04D6 {Ӗ} : InternalAddCharsToResult([$0114]); {Ĕ}
      $04D7 {ӗ} : InternalAddCharsToResult([$0115]); {ĕ}
      $0404 {Є} : InternalAddCharsToResult([$00CA]); {Ê}
      $0454 {є} : InternalAddCharsToResult([$00EA]); {ê}
      $04BC {Ҽ} : InternalAddCharsToResult([$0043,$0306]); {C̆}
      $04BD {ҽ} : InternalAddCharsToResult([$0063,$0306]); {c̆}
      $04BE {Ҿ} : InternalAddCharsToResult([$00C7,$0306]); {Ç̆}
      $04BF {ҿ} : InternalAddCharsToResult([$00E7,$0306]); {ç̆}
      $0416 {Ж} : InternalAddCharsToResult([$017D]); {Ž}
      $0436 {ж} : InternalAddCharsToResult([$017E]); {ž}
      $04C1 {Ӂ} : InternalAddCharsToResult([$005A,$0306]); {Z̆}
      $04C2 {ӂ} : InternalAddCharsToResult([$007A,$0306]); {z̆}
      $04DC {Ӝ} : InternalAddCharsToResult([$005A,$0304]); {Z̄}
      $04DD {ӝ} : InternalAddCharsToResult([$007A,$0304]); {z̄}
      $0496 {Җ} : InternalAddCharsToResult([$017D,$0326]); {Ž̦}
      $0497 {җ} : InternalAddCharsToResult([$017E,$0327]); {ž̧}
      $0417 {З} : InternalAddCharsToResult([$005A]); {Z}
      $0437 {з} : InternalAddCharsToResult([$007A]); {z}
      $04DE {Ӟ} : InternalAddCharsToResult([$005A,$0308]); {Z̈}
      $04DF {ӟ} : InternalAddCharsToResult([$007A,$0308]); {z̈}
      $0405 {Ѕ} : InternalAddCharsToResult([$1E90]); {Ẑ}
      $0455 {ѕ} : InternalAddCharsToResult([$1E91]); {ẑ}
      $04E0 {Ӡ} : InternalAddCharsToResult([$0179]); {Ź}
      $04E1 {ӡ} : InternalAddCharsToResult([$017A]); {ź}
      $0418 {И} : InternalAddCharsToResult([$0049]); {I}
      $0438 {и} : InternalAddCharsToResult([$0069]); {i}
      $04E4 {Ӥ} : InternalAddCharsToResult([$00CE]); {Î}
      $04E5 {ӥ} : InternalAddCharsToResult([$00EE]); {î}
      $0406 {І} : InternalAddCharsToResult([$00CC]); {Ì}
      $0456 {і} : InternalAddCharsToResult([$00EC]); {ì}
      $0407 {Ї} : InternalAddCharsToResult([$00CF]); {Ï}
      $0457 {ї} : InternalAddCharsToResult([$00EF]); {ï}
      $0419 {Й} : InternalAddCharsToResult([$004A]); {J}
      $0439 {й} : InternalAddCharsToResult([$006A]); {j}
      $0408 {Ј} : InternalAddCharsToResult([$004A,$030C]); {J̌}
      $0458 {ј} : InternalAddCharsToResult([$01F0]); {ǰ}
      $041A {К} : InternalAddCharsToResult([$004B]); {K}
      $043A {к} : InternalAddCharsToResult([$006B]); {k}
      $049A {Қ} : InternalAddCharsToResult([$0136]); {Ķ}
      $049B {қ} : InternalAddCharsToResult([$0137]); {ķ}
      $049E {Ҟ} : InternalAddCharsToResult([$004B,$0304]); {K̄}
      $049F {ҟ} : InternalAddCharsToResult([$006B,$0304]); {k̄}
      $041B {Л} : InternalAddCharsToResult([$004C]); {L}
      $043B {л} : InternalAddCharsToResult([$006C]); {l}
      $0409 {Љ} : InternalAddCharsToResult([$004C,$0302]); {L̂}
      $0459 {љ} : InternalAddCharsToResult([$006C,$0302]); {l̂}
      $041C {М} : InternalAddCharsToResult([$004D]); {M}
      $043C {м} : InternalAddCharsToResult([$006D]); {m}
      $041D {Н} : InternalAddCharsToResult([$004E]); {N}
      $043D {н} : InternalAddCharsToResult([$006E]); {n}
      $040A {Њ} : InternalAddCharsToResult([$004E,$0302]); {N̂}
      $045A {њ} : InternalAddCharsToResult([$006E,$0302]); {n̂}
      $04A4 {Ҥ} : InternalAddCharsToResult([$1E44]); {Ṅ}
      $04A5 {ҥ} : InternalAddCharsToResult([$1E45]); {ṅ}
      $04A2 {Ң} : InternalAddCharsToResult([$1E46]); {Ṇ}
      $04A3 {ң} : InternalAddCharsToResult([$1E47]); {ṇ}
      $041E {О} : InternalAddCharsToResult([$004F]); {O}
      $043E {о} : InternalAddCharsToResult([$006F]); {o}
      $04E6 {Ӧ} : InternalAddCharsToResult([$00D6]); {Ö}
      $04E7 {ӧ} : InternalAddCharsToResult([$00F6]); {ö}
      $04E8 {Ө} : InternalAddCharsToResult([$00D4]); {Ô}
      $04E9 {ө} : InternalAddCharsToResult([$00F4]); {ô}
      $041F {П} : InternalAddCharsToResult([$0050]); {P}
      $043F {п} : InternalAddCharsToResult([$0070]); {p}
      $04A6 {Ҧ} : InternalAddCharsToResult([$1E54]); {Ṕ}
      $04A7 {ҧ} : InternalAddCharsToResult([$1E55]); {ṕ}
      $0420 {Р} : InternalAddCharsToResult([$0052]); {R}
      $0440 {р} : InternalAddCharsToResult([$0072]); {r}
      $0421 {С} : InternalAddCharsToResult([$0053]); {S}
      $0441 {с} : InternalAddCharsToResult([$0073]); {s}
      $04AA {Ҫ} : InternalAddCharsToResult([$00C7]); {Ç}
      $04AB {ҫ} : InternalAddCharsToResult([$00E7]); {ç}
      $0422 {Т} : InternalAddCharsToResult([$0054]); {T}
      $0442 {т} : InternalAddCharsToResult([$0074]); {t}
      $04AC {Ҭ} : InternalAddCharsToResult([$0162]); {Ţ}
      $04AD {ҭ} : InternalAddCharsToResult([$0163]); {ţ}
      $040B {Ћ} : InternalAddCharsToResult([$0106]); {Ć}
      $045B {ћ} : InternalAddCharsToResult([$0170]); {Ű}
      $040C {Ќ} : InternalAddCharsToResult([$1E30]); {Ḱ}
      $045C {ќ} : InternalAddCharsToResult([$1E31]); {ḱ}
      $0423 {У} : InternalAddCharsToResult([$0055]); {U}
      $0443 {у} : InternalAddCharsToResult([$0075]); {u}
      $EE19 {} : InternalAddCharsToResult([$00DA]); {Ú}
      $EE99 {} : InternalAddCharsToResult([$00FA]); {ú}
      $040E {Ў} : InternalAddCharsToResult([$016C]); {Ŭ}
      $045E {ў} : InternalAddCharsToResult([$016D]); {ŭ}
      $04F0 {Ӱ} : InternalAddCharsToResult([$00DC]); {Ü}
      $04F1 {ӱ} : InternalAddCharsToResult([$00FC]); {ü}
      $04F2 {Ӳ} : InternalAddCharsToResult([$0170]); {Ű}
      $04F3 {ӳ} : InternalAddCharsToResult([$0171]); {ű}
      $04AE {Ү} : InternalAddCharsToResult([$00D9]); {Ù}
      $04AF {ү} : InternalAddCharsToResult([$00F9]); {ù}
      $0424 {Ф} : InternalAddCharsToResult([$0046]); {F}
      $0444 {ф} : InternalAddCharsToResult([$0066]); {f}
      $0425 {Х} : InternalAddCharsToResult([$0048]); {H}
      $0445 {х} : InternalAddCharsToResult([$0068]); {h}
      $04B2 {Ҳ} : InternalAddCharsToResult([$1E28]); {Ḩ}
      $04B3 {ҳ} : InternalAddCharsToResult([$1E29]); {ḩ}
      $04BA {Һ} : InternalAddCharsToResult([$1E24]); {Ḥ}
      $04BB {һ} : InternalAddCharsToResult([$1E25]); {ḥ}
      $0426 {Ц} : InternalAddCharsToResult([$0043]); {C}
      $0446 {ц} : InternalAddCharsToResult([$0063]); {c}
      $04B4 {Ҵ} : InternalAddCharsToResult([$0043,$0304]); {C̄}
      $04B5 {ҵ} : InternalAddCharsToResult([$0063,$0304]); {c̄}
      $0427 {Ч} : InternalAddCharsToResult([$010C]); {Č}
      $0447 {ч} : InternalAddCharsToResult([$010D]); {č}
      $04F4 {Ӵ} : InternalAddCharsToResult([$0043,$0308]); {C̈}
      $04F5 {ӵ} : InternalAddCharsToResult([$0063,$0308]); {c̈}
      $04CB {Ӌ} : InternalAddCharsToResult([$00C7]); {Ç}
      $04CC {ӌ} : InternalAddCharsToResult([$00E7]); {ç}
      $040F {Џ} : InternalAddCharsToResult([$0044,$0302]); {D̂}
      $045F {џ} : InternalAddCharsToResult([$0064,$0302]); {d̂}
      $0428 {Ш} : InternalAddCharsToResult([$0160]); {Š}
      $0448 {ш} : InternalAddCharsToResult([$0161]); {š}
      $0429 {Щ} : InternalAddCharsToResult([$015C]); {Ŝ}
      $0449 {щ} : InternalAddCharsToResult([$015D]); {ŝ}
      $042A {Ъ} : InternalAddCharsToResult([$02BA]); {ʺ}
      {The capital hard sign is very seldom in use. It may be capital, if everything is written in upper case -
       therefore when transliteration is used reverse we can say that $02BA = $042A if everything is written in upper case}
      $044A {ъ} : InternalAddCharsToResult([$02BA]); {ʺ}
      $02BC {ʼ} : InternalAddCharsToResult([$2019]); {’}
      $042B {Ы} : InternalAddCharsToResult([$0059]); {Y}
      $044B {ы} : InternalAddCharsToResult([$0079]); {y}
      $04F8 {Ӹ} : InternalAddCharsToResult([$0178]); {Ÿ}
      $04F9 {ӹ} : InternalAddCharsToResult([$00FF]); {ÿ}
      $042C {Ь} : InternalAddCharsToResult([$02B9]); {ʹ}
      {The capital hard sign is very seldom in use. It may be capital, if everything is written in upper case -
       therefore when transliteration is used reverse we can say that $042C = $02B9 if everything is written in upper case}
      $044C {ь} : InternalAddCharsToResult([$02B9]); {ʹ}
      $042D {Э} : InternalAddCharsToResult([$00C8]); {È}
      $044D {э} : InternalAddCharsToResult([$00E8]); {è}
      $042E {Ю} : InternalAddCharsToResult([$00DB]); {Û}
      $044E {ю} : InternalAddCharsToResult([$00FB]); {û}
      $042F {Я} : InternalAddCharsToResult([$00C2]); {Â}
      $044F {я} : InternalAddCharsToResult([$00E2]); {â}
      $048C {Ҍ} : InternalAddCharsToResult([$011A]); {Ě}
      $048D {ҍ} : InternalAddCharsToResult([$011B]); {ě}
      $046A {Ѫ} : InternalAddCharsToResult([$01CD]); {Ǎ}
      $046B {ѫ} : InternalAddCharsToResult([$01CE]); {ǎ}
      $0472 {Ѳ} : InternalAddCharsToResult([$0046,$0300]); {F̀}
      $0473 {ѳ} : InternalAddCharsToResult([$0066,$0300]); {f̀}
      $0474 {Ѵ} : InternalAddCharsToResult([$1EF2]); {Ỳ}
      $0475 {ѵ} : InternalAddCharsToResult([$1EF3]); {ỳ}
      $04A8 {Ҩ} : InternalAddCharsToResult([$00D2]); {Ò}
      $04A9 {ҩ} : InternalAddCharsToResult([$00F2]); {ò}
      $04C0 {Ӏ} : InternalAddCharsToResult([$2021]); {‡}
      else InternalAddCharsToResult([j]);
    end;
  end;
  SetLength(aLatinWideStr,aLatinWideStrCurrentPos);
  Result := UTF8Encode(aLatinWideStr);
End;

{*******************************************************************************}
Function ALUTF8BGNPCGN1947CyrillicToLatin(aCyrillicText: UTF8String): UTF8String;
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

  {-------------------------------------------------------------------------------------}
  function InternalCheckInRange(aChar: integer; aArrayOfChar: Array of Integer): boolean;
  var i: integer;
  begin
    Result := False;
    for i := low(aArrayOfChar) to high(aArrayOfChar) do
      if aChar = aArrayOfChar[i] then begin
        result := true;
        exit;
      end;
  end;

Var I, j: Integer;
Begin
  Result := '';
  aCyrillicWideStr := UTF8Decode(aCyrillicText);
  setlength(ALatinWideStr,length(aCyrillicWideStr) * 2); //to Be on the safe way
  aLatinWideStrCurrentPos := 0;
  for i := 1 to length(aCyrillicWideStr) do begin
    j := ord(aCyrillicWideStr[i]);
    case j of
      $0410 {А} : InternalAddCharsToResult([$0041]); {A}
      $0430 {а} : InternalAddCharsToResult([$0061]); {a}
      $0411 {Б} : InternalAddCharsToResult([$0042]); {B}
      $0431 {б} : InternalAddCharsToResult([$0062]); {b}
      $0412 {В} : InternalAddCharsToResult([$0056]); {V}
      $0432 {в} : InternalAddCharsToResult([$0076]); {v}
      $0413 {Г} : InternalAddCharsToResult([$0047]); {G}
      $0433 {г} : InternalAddCharsToResult([$0067]); {g}
      $0414 {Д} : InternalAddCharsToResult([$0044]); {D}
      $0434 {д} : InternalAddCharsToResult([$0064]); {d}
      $0415 {Е} : begin
                    {The character e should be romanized ye initially, after the vowel characters a, e, ё, и, о, у, ы, э, ю, and я,
                     and after й, ъ, and ь. In all other instances, it should be romanized e.}
                    if (i > 1) and InternalCheckInRange(ord(aCyrillicWideStr[i-1]),[$0410 {А}, $0430 {а},
                                                                                    $0415 {Е}, $0435 {е},
                                                                                    $0401 {Ё}, $0451 {ё},
                                                                                    $0418 {И}, $0438 {и},
                                                                                    $041E {О}, $043E {о},
                                                                                    $0423 {У}, $0443 {у},
                                                                                    $042B {Ы}, $044B {ы},
                                                                                    $042D {Э}, $044D {э},
                                                                                    $042E {Ю}, $044E {ю},
                                                                                    $042F {Я}, $044F {я},
                                                                                    $0419 {Й}, $0439 {й},
                                                                                    $042A {Ъ}, $044A {ъ},
                                                                                    $042C {Ь}, $044C {ь}]) then InternalAddCharsToResult([$0059, $0065]) {Ye}
                    else InternalAddCharsToResult([$0045]); {E}
                  end;
      $0435 {е} : begin
                    {The character e should be romanized ye initially, after the vowel characters a, e, ё, и, о, у, ы, э, ю, and я,
                     and after й, ъ, and ь. In all other instances, it should be romanized e.}
                    if (i > 1) and InternalCheckInRange(ord(aCyrillicWideStr[i-1]),[$0410 {А}, $0430 {а},
                                                                                    $0415 {Е}, $0435 {е},
                                                                                    $0401 {Ё}, $0451 {ё},
                                                                                    $0418 {И}, $0438 {и},
                                                                                    $041E {О}, $043E {о},
                                                                                    $0423 {У}, $0443 {у},
                                                                                    $042B {Ы}, $044B {ы},
                                                                                    $042D {Э}, $044D {э},
                                                                                    $042E {Ю}, $044E {ю},
                                                                                    $042F {Я}, $044F {я},
                                                                                    $0419 {Й}, $0439 {й},
                                                                                    $042A {Ъ}, $044A {ъ},
                                                                                    $042C {Ь}, $044C {ь}]) then InternalAddCharsToResult([$0079, $0065]) {ye}
                    else InternalAddCharsToResult([$0065]); {e}
                  end;
      $0401 {Ё} : begin
                    {The character ё is not considered a separate character of the Russian alphabet and the dieresis is generally not shown.
                    When the dieresis is shown, the character should be romanized yë initially, after the vowel characters a, e, ё, и, о, у, ы, э, ю, and я,
                    and after й, ъ, and ь. In all other instances, it should be romanized ё. When the dieresis is not shown, the character may still be
                    romanized in the preceding manner or, alternatively, in accordance with note 1.}
                    if (i > 1) and InternalCheckInRange(ord(aCyrillicWideStr[i-1]),[$0410 {А}, $0430 {а},
                                                                                    $0415 {Е}, $0435 {е},
                                                                                    $0401 {Ё}, $0451 {ё},
                                                                                    $0418 {И}, $0438 {и},
                                                                                    $041E {О}, $043E {о},
                                                                                    $0423 {У}, $0443 {у},
                                                                                    $042B {Ы}, $044B {ы},
                                                                                    $042D {Э}, $044D {э},
                                                                                    $042E {Ю}, $044E {ю},
                                                                                    $042F {Я}, $044F {я},
                                                                                    $0419 {Й}, $0439 {й},
                                                                                    $042A {Ъ}, $044A {ъ},
                                                                                    $042C {Ь}, $044C {ь}]) then InternalAddCharsToResult([$0059, $00EB]) {Yë}
                    else InternalAddCharsToResult([$00CB]); {Ë}
                  end;
      $0451 {ё} : begin
                    {The character ё is not considered a separate character of the Russian alphabet and the dieresis is generally not shown.
                    When the dieresis is shown, the character should be romanized yë initially, after the vowel characters a, e, ё, и, о, у, ы, э, ю, and я,
                    and after й, ъ, and ь. In all other instances, it should be romanized ё. When the dieresis is not shown, the character may still be
                    romanized in the preceding manner or, alternatively, in accordance with note 1.}
                    if (i > 1) and InternalCheckInRange(ord(aCyrillicWideStr[i-1]),[$0410 {А}, $0430 {а},
                                                                                    $0415 {Е}, $0435 {е},
                                                                                    $0401 {Ё}, $0451 {ё},
                                                                                    $0418 {И}, $0438 {и},
                                                                                    $041E {О}, $043E {о},
                                                                                    $0423 {У}, $0443 {у},
                                                                                    $042B {Ы}, $044B {ы},
                                                                                    $042D {Э}, $044D {э},
                                                                                    $042E {Ю}, $044E {ю},
                                                                                    $042F {Я}, $044F {я},
                                                                                    $0419 {Й}, $0439 {й},
                                                                                    $042A {Ъ}, $044A {ъ},
                                                                                    $042C {Ь}, $044C {ь}]) then InternalAddCharsToResult([$0079, $00EB]) {yë}
                    else InternalAddCharsToResult([$00EB]); {ë}
                  end;
      $0416 {Ж} : InternalAddCharsToResult([$005A,$0068]); {Zh}
      $0436 {ж} : InternalAddCharsToResult([$007A,$0068]); {zh}
      $0417 {З} : InternalAddCharsToResult([$005A]); {Z}
      $0437 {з} : InternalAddCharsToResult([$007A]); {z}
      $0418 {И} : InternalAddCharsToResult([$0049]); {I}
      $0438 {и} : InternalAddCharsToResult([$0069]); {i}
      $0419 {Й} : InternalAddCharsToResult([$0059]); {Y}
      $0439 {й} : InternalAddCharsToResult([$0079]); {y}
      $041A {К} : InternalAddCharsToResult([$004B]); {K}
      $043A {к} : InternalAddCharsToResult([$006B]); {k}
      $041B {Л} : InternalAddCharsToResult([$004C]); {L}
      $043B {л} : InternalAddCharsToResult([$006C]); {l}
      $041C {М} : InternalAddCharsToResult([$004D]); {M}
      $043C {м} : InternalAddCharsToResult([$006D]); {m}
      $041D {Н} : InternalAddCharsToResult([$004E]); {N}
      $043D {н} : InternalAddCharsToResult([$006E]); {n}
      $041E {О} : InternalAddCharsToResult([$004F]); {O}
      $043E {о} : InternalAddCharsToResult([$006F]); {o}
      $041F {П} : InternalAddCharsToResult([$0050]); {P}
      $043F {п} : InternalAddCharsToResult([$0070]); {p}
      $0420 {Р} : InternalAddCharsToResult([$0052]); {R}
      $0440 {р} : InternalAddCharsToResult([$0072]); {r}
      $0421 {С} : InternalAddCharsToResult([$0053]); {S}
      $0441 {с} : InternalAddCharsToResult([$0073]); {s}
      $0422 {Т} : InternalAddCharsToResult([$0054]); {T}
      $0442 {т} : InternalAddCharsToResult([$0074]); {t}
      $0423 {У} : InternalAddCharsToResult([$0055]); {U}
      $0443 {у} : InternalAddCharsToResult([$0075]); {u}
      $0424 {Ф} : InternalAddCharsToResult([$0046]); {F}
      $0444 {ф} : InternalAddCharsToResult([$0066]); {f}
      $0425 {Х} : InternalAddCharsToResult([$004B, $0068]); {Kh}
      $0445 {х} : InternalAddCharsToResult([$006B, $0068]); {kh}
      $0426 {Ц} : InternalAddCharsToResult([$0054, $0073]); {Ts}
      $0446 {ц} : InternalAddCharsToResult([$0074, $0073]); {ts}
      $0427 {Ч} : InternalAddCharsToResult([$0043, $0068]); {Ch}
      $0447 {ч} : InternalAddCharsToResult([$0063, $0068]); {ch}
      $0428 {Ш} : InternalAddCharsToResult([$0053, $0068]); {Sh}
      $0448 {ш} : InternalAddCharsToResult([$0073, $0068]); {sh}
      $0429 {Щ} : InternalAddCharsToResult([$0053, $0068, $0063, $0068]); {Shch}
      $0449 {щ} : InternalAddCharsToResult([$0073, $0068, $0063, $0068]); {shch}
      $042A {Ъ} : InternalAddCharsToResult([$0022]); {"}
      $044A {ъ} : InternalAddCharsToResult([$0022]); {"}
      $042B {Ы} : InternalAddCharsToResult([$0059]); {Y}
      $044B {ы} : InternalAddCharsToResult([$0079]); {y}
      $042C {Ь} : InternalAddCharsToResult([$0027]); {'}
      $044C {ь} : InternalAddCharsToResult([$0027]); {'}
      $042D {Э} : InternalAddCharsToResult([$0045]); {E}
      $044D {э} : InternalAddCharsToResult([$0065]); {e}
      $042E {Ю} : InternalAddCharsToResult([$0059, $0075]); {Yu}
      $044E {ю} : InternalAddCharsToResult([$0079, $0075]); {yu}
      $042F {Я} : InternalAddCharsToResult([$0059, $0061]); {Ya}
      $044F {я} : InternalAddCharsToResult([$0079, $0061]); {ya}
      else InternalAddCharsToResult([j]);
    end;
  end;
  SetLength(aLatinWideStr,aLatinWideStrCurrentPos);
  Result := UTF8Encode(aLatinWideStr);
End;

end.
