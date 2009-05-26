{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
              John O'Harrow (john@elmcrest.demon.co.uk)
              Charalabos Michael (chmichael@creationpower.com)
              Aleksandr Sharahov
              Dennis Kjaer Christensen
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      Alcinoe String functions
Version:      3.53

Description:  Powerfull stringreplace, Pos, Move, comparetext,
              uppercase, lowercase function. Also a powerfull
              FastTagReplace function To replace in string tag
              like <#tagname params1="value1" params2="value2">
              by custom value

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

History:      11/05/2005: Remove the bug in alFastTagReplace that raise
                          an exception when char % is found in the params
                          of a tag
							20/10/2005: Move AlANSICodePage1252UppercaseNoDiacritic to
							            ALWideUpperCaseNoDiacritic in alFcnUnicode...
              16/11/2005: minor update in ALFastTagReplace to better
                          handle the "Handled" property of TALHandleTagfunct
              02/12/2005: 1/ Correct AlCopyStr;
                          2/ Move some copy call to AlCopyStr call;
                          3/ Update AlFastTagReplace to better performance and
                             low memory usage;
              08/12/2005: Update AlFastTagReplace to correct a bug that make
                          rfignorecase wrong in some case
              16/12/2005: remove ALStringMatches that seam to not work propertly
                          use MatchesMask insteed !
              01/04/2007: Update the FastCode Function
              22/02/2008: Use AlHttpEncode instead that HttpEncode
              26/12/2008: replace ALGetStringFromFileWithoutUT8BOM by
                          ALGetStringFromFileWithoutUTF8BOM
              01/03/2009: Now use the default internal delphi function
                          for alPos, AlPosEx, AlCompareText, AlLowerCase,
                          AlUpperCase, AlMove because they are all taken from
                          the fastcode project

Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALfcnString;

interface

uses Windows,
     SysUtils,
     Classes,
     StrUtils;

type

  {type declaration}
  TALHandleTagfunct = function(const TagString: string; TagParams: TStrings; ExtData: pointer; Var Handled: Boolean): string;

{Now than Delphi use the fastcode function, this is just a link to the delphi default function}
Var ALPosEx: function(const SubStr, S: string; Offset: Integer = 1): Integer;
var ALMove: procedure(const Source; var Dest; Count : Integer);
var ALPos: function(const SubStr: AnsiString; const Str: AnsiString): Integer;
Var ALCompareText: function(const S1, S2: string): Integer;
Var ALLowerCase: function(const s: string): string;
Var ALUpperCase: function(const s: string): string;
function ALCharPos(Ch: Char; const Str : AnsiString): Integer;
{from John O'Harrow (john@elmcrest.demon.co.uk)
 original name: StringReplace_JOH_IA32_12}
function ALStringReplace(const S, OldPattern, NewPattern: AnsiString; Flags: TReplaceFlags): AnsiString;
{from FastCode John O'Harrow (john@elmcrest.demon.co.uk)
 original name: CharPosEY_JOH_IA32_4}
function ALCharPosEX(const SearchCharacter: Char; const SourceString: AnsiString; Occurrence: Integer; StartPos: Integer): Integer; overload;
function ALCharPosEX(const SearchCharacter: Char; const SourceString: AnsiString; StartPos: Integer = 1): Integer; overload;

{Alcinoe}
function  ALFastTagReplace(Const SourceString, TagStart, TagEnd: string; FastTagReplaceProc: TALHandleTagFunct; ReplaceStrParamName, ReplaceWith: String; AStripParamQuotes: Boolean; Flags: TReplaceFlags; ExtData: Pointer): string; overload;
function  ALFastTagReplace(const SourceString, TagStart, TagEnd: string; ReplaceStrParamName: String; AStripParamQuotes: Boolean; const Flags: TReplaceFlags=[rfreplaceall]): string; overload;
function  ALFastTagReplace(const SourceString, TagStart, TagEnd: string; FastTagReplaceProc: TALHandleTagFunct; AStripParamQuotes: Boolean; ExtData: Pointer; Const flags: TReplaceFlags = [rfreplaceall]): string; overload;
function  ALFastTagReplace(const SourceString, TagStart, TagEnd: string; ReplaceWith: string; const Flags: TReplaceFlags=[rfreplaceall] ): string; overload;
function  ALExtractTagParams(const SourceString, TagStart, TagEnd: string; AStripParamQuotes: Boolean; TagParams: TStrings; IgnoreCase: Boolean): Boolean;
function  ALCopyStr(const aSourceString: string; aStart, aLength: Integer): string;
function  ALRandomStr(aLength: Longint): string;
function  ALNEVExtractName(const S: string): string;
function  ALNEVExtractValue(const s: string): string;
procedure ALExtractHeaderFields(Separators, WhiteSpace: TSysCharSet; Content: PChar; Strings: TStrings; Decode: Boolean; StripQuotes: Boolean = False);
procedure ALExtractHeaderFieldsWithQuoteEscaped(Separators, WhiteSpace: TSysCharSet; Content: PChar; Strings: TStrings; Decode: Boolean; StripQuotes: Boolean = False);
function  ALGetStringFromFile(filename: string): string;
function  ALGetStringFromFileWithoutUTF8BOM(filename: string): string;
procedure ALSaveStringtoFile(Str,filename: string);
Function  ALAnsiUpperCaseNoDiacritic(S: string): string;

implementation

uses AlHTTPCommon,
     ALFcnUnicode, 
     ALCPUID;

////////////////////////////
//////////ALPosEx //////////
////////////////////////////

{**********************************}
{Now all the fastcode function are use in delphi by default}
{we use the default delphi function}
procedure ALInitPosExFunct;
begin
  AlPosEx := PosEx;
end;



///////////////////////////////////////
//////////AlFastStringReplace//////////
///////////////////////////////////////

var
  vALStringReplaceAnsiUpcase: packed array[Char] of Char; {Upcase Lookup Table}
  vALStringReplacesrCodePage: UINT; {Active String Replace Windows CodePage}

{Setup Lookup Table for Ansi Uppercase}
procedure ALStringReplaceInitialiseAnsiUpcase;
var
  Ch: Char;
begin
  vALStringReplacesrCodePage := GetACP;
  for Ch := #0 to #255 do
    vALStringReplaceAnsiUpcase[Ch] := Ch;
  CharUpperBuffA(@vALStringReplaceAnsiUpcase, 256);
end;

{*********************************************************************************************}
function ALStringReplaceAnsiPosExIC(const SubStr, S: Ansistring; Offset: Integer = 1): Integer;
asm
  push    ebx
  push    esi
  push    edx              {@Str}
  test    eax, eax
  jz      @@NotFound       {Exit if SubStr = ''}
  test    edx, edx
  jz      @@NotFound       {Exit if Str = ''}
  mov     esi, ecx
  mov     ecx, [edx-4]     {Length(Str)}
  mov     ebx, [eax-4]     {Length(SubStr)}
  add     ecx, edx
  sub     ecx, ebx         {Max Start Pos for Full Match}
  lea     edx, [edx+esi-1] {Set Start Position}
  cmp     edx, ecx
  jg      @@NotFound       {StartPos > Max Start Pos}
  cmp     ebx, 1           {Length(SubStr)}
  jle     @@SingleChar     {Length(SubStr) <= 1}
  push    edi
  push    ebp
  lea     edi, [ebx-2]     {Length(SubStr) - 2}
  mov     esi, eax
  push    edi              {Save Remainder to Check = Length(SubStr) - 2}
  push    ecx              {Save Max Start Position}
  lea     edi, vALStringReplaceAnsiUpcase  {Uppercase Lookup Table}
  movzx   ebx, [eax]       {Search Character = 1st Char of SubStr}
  movzx   ebx, [edi+ebx]   {Convert to Uppercase}
@@Loop:                    {Loop Comparing 2 Characters per Loop}
  movzx   eax, [edx]       {Get Next Character}
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  cmp     eax, ebx
  jne     @@NotChar1
  mov     ebp, [esp+4]     {Remainder to Check}
@@Char1Loop:
  movzx   eax, [esi+ebp]
  movzx   ecx, [edx+ebp]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar1
  movzx   eax, [esi+ebp+1]
  movzx   ecx, [edx+ebp+1]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar1
  sub     ebp, 2
  jnc     @@Char1Loop
  pop     ecx
  pop     edi
  pop     ebp
  pop     edi
  jmp     @@SetResult
@@NotChar1:
  movzx   eax, [edx+1]     {Get Next Character}
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  cmp     bl, al
  jne     @@NotChar2
  mov     ebp, [esp+4]     {Remainder to Check}
@@Char2Loop:
  movzx   eax, [esi+ebp]
  movzx   ecx, [edx+ebp+1]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar2
  movzx   eax, [esi+ebp+1]
  movzx   ecx, [edx+ebp+2]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar2
  sub     ebp, 2
  jnc     @@Char2Loop
  pop     ecx
  pop     edi
  pop     ebp
  pop     edi
  jmp     @@CheckResult    {Check Match is within String Data}
@@NotChar2:
  add     edx, 2
  cmp     edx, [esp]       {Compate to Max Start Position}
  jle     @@Loop           {Loop until Start Position > Max Start Position}
  pop     ecx              {Dump Start Position}
  pop     edi              {Dump Remainder to Check}
  pop     ebp
  pop     edi
  jmp     @@NotFound
@@SingleChar:
  jl      @@NotFound       {Needed for Zero-Length Non-NIL Strings}
  lea     esi, vALStringReplaceAnsiUpcase
  movzx   ebx, [eax]       {Search Character = 1st Char of SubStr}
  movzx   ebx, [esi+ebx]   {Convert to Uppercase}
@@CharLoop:
  movzx   eax, [edx]
  movzx   eax, [esi+eax]   {Convert to Uppercase}
  cmp     eax, ebx
  je      @@SetResult
  movzx   eax, [edx+1]
  movzx   eax, [esi+eax]   {Convert to Uppercase}
  cmp     eax, ebx
  je      @@CheckResult
  add     edx, 2
  cmp     edx, ecx
  jle     @@CharLoop
@@NotFound:
  xor     eax, eax
  pop     edx
  pop     esi
  pop     ebx
  ret
@@CheckResult:             {Check Match is within String Data}
  cmp     edx, ecx
  jge     @@NotFound
  add     edx, 1           {OK - Adjust Result}
@@SetResult:               {Set Result Position}
  pop     ecx              {@Str}
  pop     esi
  pop     ebx
  neg     ecx
  lea     eax, [edx+ecx+1]
end; {AnsiPosExIC}

{******************************************************************************************************}
function ALStringReplace(const S, OldPattern, NewPattern: AnsiString; Flags: TReplaceFlags): AnsiString;
type
  TPosEx = function(const SubStr, S: Ansistring; Offset: Integer): Integer;
const
  StaticBufferSize = 16;
var
  SrcLen, OldLen, NewLen, Found, Count, Start, Match, Matches, BufSize,
  Remainder    : Integer;
  PosExFunction: TPosEx;
  StaticBuffer : array[0..StaticBufferSize-1] of Integer;
  Buffer       : PIntegerArray;
  P, PSrc, PRes: PChar;
  Ch           : Char;
begin
  SrcLen := Length(S);
  OldLen := Length(OldPattern);
  NewLen := Length(NewPattern);
  if (OldLen = 0) or (SrcLen < OldLen) then
    begin
      if SrcLen = 0 then
        Result := '' {Needed for Non-Nil Zero Length Strings}
      else
        Result := S
    end
  else
    begin
      if rfIgnoreCase in Flags then
        begin
          PosExFunction := ALStringReplaceAnsiPosExIC;
          if GetACP <> vALStringReplacesrCodePage then {Check CodePage}
            ALStringReplaceInitialiseAnsiUpcase; {CodePage Changed - Update Lookup Table}
        end
      else
        PosExFunction := ALPosEx;
      if rfReplaceAll in Flags then
        begin
          if (OldLen = 1) and (NewLen = 1) then
            begin {Single Character Replacement}
              Remainder := SrcLen;
              SetLength(Result, Remainder);
              P := Pointer(Result);
              Move(Pointer(S)^, P^, Remainder);
              if rfIgnoreCase in Flags then
                begin
                  Ch := vALStringReplaceAnsiUpcase[OldPattern[1]];
                  repeat
                    Dec(Remainder);
                    if vALStringReplaceAnsiUpcase[P[Remainder]] = Ch then
                      P[Remainder] := NewPattern[1];
                  until Remainder = 0;
                end
              else
                begin
                  repeat
                    Dec(Remainder);
                    if P[Remainder] = OldPattern[1] then
                      P[Remainder] := NewPattern[1];
                  until Remainder = 0;
                end;
              Exit;
            end;
          Found := PosExFunction(OldPattern, S, 1);
          if Found <> 0 then
            begin
              Buffer    := @StaticBuffer;
              BufSize   := StaticBufferSize;
              Matches   := 1;
              Buffer[0] := Found;
              repeat
                Inc(Found, OldLen);
                Found := PosExFunction(OldPattern, S, Found);
                if Found > 0 then
                  begin
                    if Matches = BufSize then
                      begin {Create or Expand Dynamic Buffer}
                        BufSize := BufSize + (BufSize shr 1); {Grow by 50%}
                        if Buffer = @StaticBuffer then
                          begin {Create Dynamic Buffer}
                            GetMem(Buffer, BufSize * SizeOf(Integer));
                            Move(StaticBuffer, Buffer^, SizeOf(StaticBuffer));
                          end
                        else {Expand Dynamic Buffer}
                          ReallocMem(Buffer, BufSize * SizeOf(Integer));
                      end;
                    Buffer[Matches] := Found;
                    Inc(Matches);
                  end
              until Found = 0;
              SetLength(Result, SrcLen + (Matches * (NewLen - OldLen)));
              PSrc := Pointer(S);
              PRes := Pointer(Result);
              Start := 1;
              Match := 0;
              repeat
                Found := Buffer[Match];
                Count := Found - Start;
                Start := Found + OldLen;
                if Count > 0 then
                  begin
                    Move(PSrc^, PRes^, Count);
                    Inc(PRes, Count);
                  end;
                Inc(PSrc, Count + OldLen);
                Move(Pointer(NewPattern)^, PRes^, NewLen);
                Inc(PRes, NewLen);
                Inc(Match);
              until Match = Matches;
              Remainder := SrcLen - Start;
              if Remainder >= 0 then
                Move(PSrc^, PRes^, Remainder + 1);
              if BufSize <> StaticBufferSize then
                FreeMem(Buffer); {Free Dynamic Buffer if Created}
            end
          else {No Matches Found}
            Result := S
        end {ReplaceAll}
      else
        begin {Replace First Occurance Only}
          Found := PosExFunction(OldPattern, S, 1);
          if Found <> 0 then
            begin {Match Found}
              SetLength(Result, SrcLen - OldLen + NewLen);
              Dec(Found);
              PSrc := Pointer(S);
              PRes := Pointer(Result);
              if NewLen = OldLen then
                begin
                  Move(PSrc^, PRes^, SrcLen);
                  Inc(PRes, Found);
                  Move(Pointer(NewPattern)^, PRes^, NewLen);
                end
              else
                begin
                  Move(PSrc^, PRes^, Found);
                  Inc(PRes, Found);
                  Inc(PSrc, Found + OldLen);
                  Move(Pointer(NewPattern)^, PRes^, NewLen);
                  Inc(PRes, NewLen);
                  Move(PSrc^, PRes^, SrcLen - Found - OldLen);
                end;
            end
          else {No Matches Found}
            Result := S
        end;
    end;
end;

{**********************************}
{Called Once by Unit Initialisation}
procedure ALInitStringReplaceFunct;
begin
  vALStringReplacesrCodePage := 0; {Invalidate AnsiUpcase Lookup Table}
  ALStringReplaceInitialiseAnsiUpcase;
end;




//////////////////////////
//////////AlMove//////////
//////////////////////////

{**********************************************************}
{Now all the fastcode function are use in delphi by default}
{we use the default delphi function}
procedure ALInitMovProc;
begin
  ALMove := Move;
end;



/////////////////////////
//////////ALPos//////////
/////////////////////////

{**********************************}
{Now all the fastcode function are use in delphi by default}
{we use the default delphi function}
procedure ALInitPosFunct;
begin
  Alpos := Pos;
end;





/////////////////////////////
//////////ALCharPos//////////
/////////////////////////////

{************************************************************}
function ALCharPos(Ch: Char; const Str : AnsiString): Integer;
Begin
  Result := ALCharPosEX(Ch,Str);
End;



///////////////////////////////
//////////ALCharPosEX//////////
///////////////////////////////

{****************************************}
{Can Read DWORD containing NULL Charatcer}
function ALCharPosEX(const SearchCharacter: Char;
                     const SourceString: AnsiString;
                     Occurrence: Integer;
                     StartPos: Integer): Integer;
asm
  test   edx, edx
  jz     @@NotFoundExit        {Exit if SourceString = ''}
  cmp    ecx, 1
  jl     @@NotFoundExit        {Exit if Occurence < 1}
  mov    ebp, StartPos         {Safe since EBP automatically saved}
  sub    ebp, 1
  jl     @@NotFoundExit        {Exit if StartPos < 1}
  push   ebx
  add    ebp, edx
  mov    ebx, [edx-4]
  add    ebx, edx
  sub    ebp, ebx
  jge    @@NotFound            {Traps Zero Length Non-Nil String}
@@Loop:
  cmp    al, [ebx+ebp]
  je     @@Check1
@@Next:
  cmp    al, [ebx+ebp+1]
  je     @@Check2
@@Next2:
  cmp    al, [ebx+ebp+2]
  je     @@Check3
@@Next3:
  cmp    al, [ebx+ebp+3]
  je     @@Check4
@@Next4:
  add    ebp, 4
  jl     @@Loop
@@NotFound:
  pop    ebx
@@NotFoundExit:
  xor    eax, eax
  jmp    @@Exit
@@Check4:
  sub    ecx, 1
  jnz    @@Next4
  add    ebp, 3
  jge    @@NotFound
  jmp    @@SetResult
@@Check3:
  sub    ecx, 1
  jnz    @@Next3
  add    ebp, 2
  jge    @@NotFound
  jmp    @@SetResult
@@Check2:
  sub    ecx, 1
  jnz    @@Next2
  add    ebp, 1
  jge    @@NotFound
  jmp    @@SetResult
@@Check1:
  sub    ecx, 1
  jnz    @@Next
@@SetResult:
  lea    eax, [ebx+ebp+1]
  sub    eax, edx
  pop    ebx
@@Exit:
end;

{****************************************}
{Can Read DWORD containing NULL Charatcer}
function ALCharPosEX(const SearchCharacter: Char;
                     const SourceString: AnsiString;
                     StartPos: Integer = 1): Integer;
begin
  result := ALCharPosEX(SearchCharacter, SourceString, 1, StartPos);
end;




/////////////////////////////////
//////////ALCompareText//////////
/////////////////////////////////

{**********************************}
{Now all the fastcode function are use in delphi by default}
{we use the default delphi function}
procedure ALInitCompareTextFunct;
begin
  ALCompareText := CompareText;
end;




////////////////////////////////////////////////////////////////
////////////////////////ALLowerCase/////////////////////////////
////////////////////////////////////////////////////////////////

{**********************************}
{Now all the fastcode function are use in delphi by default}
{we use the default delphi function}
procedure ALInitLowerCaseFunct;
begin
  AllowerCase := LowerCase;
end;




////////////////////////////////////////////////////////////////
//////////////////////////ALUpperCase///////////////////////////
////////////////////////////////////////////////////////////////

{**********************************}
{Now all the fastcode function are use in delphi by default}
{we use the default delphi function}
procedure ALInitUpperCaseFunct;
begin
  AlUpperCase := UpperCase;
end;



///////////////////////////
//////////Alcinoe//////////
///////////////////////////

{********************************************************************************}
function ALCopyStr(const aSourceString: string; aStart, aLength: Integer): string;
var SourceStringLength: Integer;
begin
  SourceStringLength := Length(aSourceString);
  If (aStart < 1) then aStart := 1;

  if (SourceStringLength=0) or
     (aLength < 1) or
     (aStart > SourceStringLength) then Begin
    Result := '';
    Exit;
  end;

  if aLength > SourceStringLength - (aStart - 1) then aLength := SourceStringLength - (aStart-1);

  SetLength(Result,aLength);
  ALMove(aSourceString[aStart], Result[1], aLength);
end;

{*********************************************}
function ALRandomStr(aLength: Longint): string;
var X: Longint;
begin
  if aLength <= 0 then exit;
  SetLength(Result, aLength);
  for X:=1 to aLength do Result[X] := Chr(Random(26) + 65);
end;

{*************************************************}
function ALNEVExtractName(const S: string): string;
var P: Integer;
begin
  Result := S;
  P := alCharPos('=', Result);
  if P <> 0 then SetLength(Result, P-1)
  else SetLength(Result, 0);
end;

{**************************************************}
function ALNEVExtractValue(const s: string): string;
begin
  Result := AlCopyStr(s, Length(ALNEVExtractName(s)) + 2, MaxInt)
end;

{*********************************************************************}
function ALFastTagReplace(Const SourceString, TagStart, TagEnd: string;
                          FastTagReplaceProc: TALHandleTagFunct;
                          ReplaceStrParamName,
                          ReplaceWith: String;
                          AStripParamQuotes: Boolean;
                          Flags: TReplaceFlags;
                          ExtData: Pointer): string;
var  i: integer;
     ReplaceString: String;
     Token, FirstTagEndChar: Char;
     TokenStr, ParamStr: string;
     ParamList: TStringList;
     TagStartLength: integer;
     TagEndLength: integer;
     SourceStringLength: Integer;
     T1,T2: Integer;
     InDoubleQuote: Boolean;
     InsingleQuote: Boolean;
     Work_SourceString: String;
     Work_TagStart: String;
     Work_TagEnd: String;
     TagHandled: Boolean;
     ResultCurrentPos: integer;
     ResultCurrentLength: integer;

Const ResultBuffSize: integer = 16384;

     {-------------------------------}
     Function ExtractTokenStr: String;
     var x: Integer;
     Begin
       x := AlCharPos(' ',ReplaceString);
       if x > 0 then Result := trim( AlcopyStr(ReplaceString,1,x) )
       else Result := trim(ReplaceString);
     end;

     {--------------------------------}
     Function ExtractParamsStr: String;
     Begin
       Result := trim( AlcopyStr(ReplaceString,length(TokenStr) + 1, MaxInt) );
     end;

     {-----------------------------------}
     Procedure MoveStr2Result(Src:String);
     Var l: integer;
     Begin
       If Src <> '' then begin
         L := Length(Src);
         If L+ResultCurrentPos-1>ResultCurrentLength Then begin
           ResultCurrentLength := ResultCurrentLength + L + ResultBuffSize;
           SetLength(Result,ResultCurrentLength);
         end;
         AlMove(Src[1],Result[ResultCurrentPos],L);
         ResultCurrentPos := ResultCurrentPos + L;
       end;
     end;


begin
  if (SourceString = '') or (TagStart = '') or (TagEnd = '') then begin
    Result := SourceString;
    Exit;
  end;

  If rfIgnoreCase in flags then begin
    Work_SourceString := ALUppercase(SourceString);
    Work_TagStart := ALuppercase(TagStart);
    Work_TagEnd := ALUppercase(TagEnd);
  end
  Else begin
    Work_SourceString := SourceString;
    Work_TagStart := TagStart;
    Work_TagEnd := TagEnd;
  end;

  SourceStringLength := length(Work_SourceString);
  ResultCurrentLength := SourceStringLength;
  SetLength(Result,ResultCurrentLength);
  ResultCurrentPos := 1;
  TagStartLength := Length(Work_TagStart);
  TagEndLength := Length(Work_TagEnd);
  FirstTagEndChar := Work_TagEnd[1];
  i := 1;

  T1 := ALPosEx(Work_TagStart,Work_SourceString,i);
  T2 := T1 + TagStartLength;
  If (T1 > 0) and (T2 <= SourceStringLength) then begin
    InDoubleQuote := False;
    InsingleQuote := False;
    Token := Work_SourceString[T2];
    if token = '"' then InDoubleQuote := True
    else if token = '''' then InSingleQuote := True;
    While (T2 < SourceStringLength) and (InDoubleQuote or InSingleQuote or (Token <> FirstTagEndChar) or (ALPosEx(Work_TagEnd,Work_SourceString,T2) <> T2)) do begin
      inc(T2);
      Token := Work_SourceString[T2];
      If Token = '"' then begin
        if (not InDoubleQuote) or (T2 = SourceStringLength) or (Work_SourceString[T2 + 1] <> Token) then InDoubleQuote := not InDoubleQuote and not InSingleQuote
        else inc(t2);
      end
      else If Token = '''' then begin
        if (not InSingleQuote) or (T2 = SourceStringLength) or (Work_SourceString[T2 + 1] <> Token) then InSingleQuote := not InSingleQuote and not InDoubleQuote
        else inc(t2);
      end;
    end;
  end;


  While (T1 > 0) and (T2 > T1) do begin
    ReplaceString := AlCopyStr(SourceString,T1 + TagStartLength,T2 - T1 - TagStartLength);

    TagHandled := True;
    If assigned(FastTagReplaceProc) or (ReplaceStrParamName <> '') then begin
      TokenStr := ExtractTokenStr;
      ParamStr := ExtractParamsStr;
      ParamList := TStringList.Create;
      try
        ALExtractHeaderFieldsWithQuoteEscaped([' ', #9], [' ', #9], PChar(ParamStr), ParamList, False, AStripParamQuotes);
        If assigned(FastTagReplaceProc) then ReplaceString := FastTagReplaceProc(TokenStr, ParamList, ExtData, TagHandled)
        else ReplaceString := ParamList.Values[ReplaceStrParamName];
      finally
        ParamList.Free;
      end;
    end
    else ReplaceString := ReplaceWith;


    If tagHandled then MoveStr2Result(AlcopyStr(SourceString,i,T1 - i) + ReplaceString)
    else MoveStr2Result(AlcopyStr(SourceString,i,T2 + TagEndLength - i));
    i := T2 + TagEndLength;

    If TagHandled and (not (rfreplaceAll in flags)) then Break;

    T1 := ALPosEx(Work_TagStart,Work_SourceString,i);
    T2 := T1 + TagStartLength;
    If (T1 > 0) and (T2 <= SourceStringLength) then begin
      InDoubleQuote := False;
      InsingleQuote := False;
      Token := Work_SourceString[T2];
      if token = '"' then InDoubleQuote := True
      else if token = '''' then InSingleQuote := True;
      While (T2 < SourceStringLength) and (InDoubleQuote or InSingleQuote or (Token <> FirstTagEndChar) or (ALPosEx(Work_TagEnd,Work_SourceString,T2) <> T2)) do begin
        inc(T2);
        Token := Work_SourceString[T2];
        If Token = '"' then begin
          if (not InDoubleQuote) or (T2 = SourceStringLength) or (Work_SourceString[T2 + 1] <> Token) then InDoubleQuote := not InDoubleQuote and not InSingleQuote
          else inc(t2);
        end
        else If Token = '''' then begin
          if (not InSingleQuote) or (T2 = SourceStringLength) or (Work_SourceString[T2 + 1] <> Token) then InSingleQuote := not InSingleQuote and not InDoubleQuote
          else inc(t2);
        end;
      end;
    end;
  end;

  MoveStr2Result(AlcopyStr(SourceString,i,maxint));
  SetLength(Result,ResultCurrentPos-1);
end;

{*********************************************************************}
function ALFastTagReplace(const SourceString, TagStart, TagEnd: string;
                          ReplaceWith: string;
                          const Flags: TReplaceFlags=[rfreplaceall] ): string;
Begin
  Result := ALFastTagReplace(SourceString, TagStart, TagEnd, nil, '', ReplaceWith, True, flags, nil);
end;

{*********************************************************************}
function ALFastTagReplace(const SourceString, TagStart, TagEnd: string;
                          ReplaceStrParamName: string;
                          AStripParamQuotes: Boolean;
                          const Flags: TReplaceFlags=[rfreplaceall] ): string;
Begin
  Result := ALFastTagReplace(SourceString, TagStart, TagEnd, nil, ReplaceStrParamName, '', AStripParamQuotes, flags, nil);
end;

{*********************************************************************}
function ALFastTagReplace(const SourceString, TagStart, TagEnd: string;
                          FastTagReplaceProc: TALHandleTagFunct;
                          AStripParamQuotes: Boolean;
                          ExtData: Pointer;
                          Const flags: TReplaceFlags=[rfreplaceall]): string;
Begin
  result := ALFastTagReplace(SourceString, TagStart, TagEnd, FastTagReplaceProc, '', '', AStripParamQuotes, flags, extdata);
end;

{***********************************************************************}
function ALExtractTagParams(Const SourceString, TagStart, TagEnd: string;
                            AStripParamQuotes: Boolean;
                            TagParams: TStrings;
                            IgnoreCase: Boolean): Boolean;
var  ReplaceString: String;
     Token, FirstTagEndChar: Char;
     TokenStr, ParamStr: string;
     TagStartLength: integer;
     SourceStringLength: Integer;
     T1,T2: Integer;
     InDoubleQuote: Boolean;
     InsingleQuote: Boolean;
     Work_SourceString: String;
     Work_TagStart: String;
     Work_TagEnd: String;

     {-------------------------------}
     Function ExtractTokenStr: String;
     var x: Integer;
     Begin
       x := AlCharPos(' ',ReplaceString);
       if x > 0 then Result := trim( AlcopyStr(ReplaceString,1,x) )
       else Result := trim(ReplaceString);
     end;

     {--------------------------------}
     Function ExtractParamsStr: String;
     Begin
       Result := trim( AlcopyStr(ReplaceString,length(TokenStr) + 1, MaxInt) );
     end;

begin
  Result := False;
  if (SourceString = '') or (TagStart = '') or (TagEnd = '') then Exit;

  If IgnoreCase then begin
    Work_SourceString := ALUppercase(SourceString);
    Work_TagStart := ALuppercase(TagStart);
    Work_TagEnd := ALUppercase(TagEnd);
  end
  Else begin
    Work_SourceString := SourceString;
    Work_TagStart := TagStart;
    Work_TagEnd := TagEnd;
  end;

  TagStartLength := Length(Work_TagStart);
  SourceStringLength := length(SourceString);
  FirstTagEndChar := tagEnd[1];

  T1 := ALPosEx(Work_TagStart,Work_SourceString,1);
  T2 := T1 + TagStartLength;
  If (T1 > 0) and (T2 <= SourceStringLength) then begin
    InDoubleQuote := False;
    InsingleQuote := False;
    Token := Work_SourceString[T2];
    if token = '"' then InDoubleQuote := True
    else if token = '''' then InSingleQuote := True;
    While (T2 < SourceStringLength) and (InDoubleQuote or InSingleQuote or (Token <> FirstTagEndChar) or (ALPosEx(Work_TagEnd,Work_SourceString,T2) <> T2)) do begin
      inc(T2);
      Token := Work_SourceString[T2];
      If Token = '"' then begin
        if (not InDoubleQuote) or (T2 = SourceStringLength) or (Work_SourceString[T2 + 1] <> Token) then InDoubleQuote := not InDoubleQuote and not InSingleQuote
        else inc(t2);
      end
      else If Token = '''' then begin
        if (not InSingleQuote) or (T2 = SourceStringLength) or (Work_SourceString[T2 + 1] <> Token) then InSingleQuote := not InSingleQuote and not InDoubleQuote
        else inc(t2);
      end;
    end;
  end;

  If (T1 > 0) and (T2 > T1) Then begin
    ReplaceString := AlCopyStr(SourceString,T1 + TagStartLength,T2 - T1 - TagStartLength);

    TokenStr := ExtractTokenStr;
    ParamStr := ExtractParamsStr;
    ALExtractHeaderFieldsWithQuoteEscaped([' ', #9], [' ', #9], PChar(ParamStr), TagParams, False, AStripParamQuotes);
    Result := True
  end;
end;

{********************************************************}
{Parses a multi-valued string into its constituent fields.
 ExtractHeaderFields is a general utility to parse multi-valued HTTP header strings into separate substrings.
 * Separators is a set of characters that are used to separate individual values within the multi-valued string.
 * WhiteSpace is a set of characters that are to be ignored when parsing the string.
 * Content is the multi-valued string to be parsed.
 * Strings is the TStrings object that receives the individual values that are parsed from Content.
 * StripQuotes determines whether the surrounding quotes are removed from the resulting items. When StripQuotes is true, surrounding quotes are removed
   before substrings are added to Strings.
 Note:	Characters contained in Separators or WhiteSpace are treated as part of a value substring if the substring is surrounded by single or double quote
 marks. HTTP escape characters are converted using the ALHTTPDecode function.}
procedure ALExtractHeaderFields(Separators, WhiteSpace: TSysCharSet; Content: PChar; Strings: TStrings; Decode: Boolean; StripQuotes: Boolean = False);
var
  Head, Tail: PChar;
  EOS, InQuote, LeadQuote: Boolean;
  QuoteChar: Char;
  ExtractedField: string;
  WhiteSpaceWithCRLF: TSysCharSet;
  SeparatorsWithCRLF: TSysCharSet;

  function DoStripQuotes(const S: string): string;
  var I: Integer;
      InStripQuote: Boolean;
      StripQuoteChar: Char;
  begin
    Result := S;
    InStripQuote := False;
    StripQuoteChar := #0;
    if StripQuotes then
      for I := Length(Result) downto 1 do
        if Result[I] in ['''', '"'] then
          if InStripQuote and (StripQuoteChar = Result[I]) then begin
            Delete(Result, I, 1);
            InStripQuote := False;
          end
          else if not InStripQuote then begin
            StripQuoteChar := Result[I];
            InStripQuote := True;
            Delete(Result, I, 1);
          end
  end;

Begin
  if (Content = nil) or (Content^ = #0) then Exit;
  WhiteSpaceWithCRLF := WhiteSpace + [#13, #10];
  SeparatorsWithCRLF := Separators + [#0, #13, #10, '"', ''''];
  Tail := Content;
  QuoteChar := #0;
  repeat
    while Tail^ in WhiteSpaceWithCRLF do Inc(Tail);
    Head := Tail;
    InQuote := False;
    LeadQuote := False;
    while True do begin
      while (InQuote and not (Tail^ in [#0, '"', ''''])) or not (Tail^ in SeparatorsWithCRLF) do Inc(Tail);
      if Tail^ in ['"',''''] then begin
        if (QuoteChar <> #0) and (QuoteChar = Tail^) then QuoteChar := #0
        else If QuoteChar = #0 then begin
          LeadQuote := Head = Tail;
          QuoteChar := Tail^;
          if LeadQuote then Inc(Head);
        end;
        InQuote := QuoteChar <> #0;
        if InQuote then Inc(Tail)
        else Break;
      end else Break;
    end;
    if not LeadQuote and (Tail^ <> #0) and (Tail^ in ['"','''']) then Inc(Tail);
    EOS := Tail^ = #0;
    if Head^ <> #0 then begin
      SetString(ExtractedField, Head, Tail-Head);
      if Decode then Strings.Add(ALHTTPDecode(DoStripQuotes(ExtractedField)))
      else Strings.Add(DoStripQuotes(ExtractedField));
    end;
    Inc(Tail);
  until EOS;
end;

{**************************************************************************************}
{same as ALExtractHeaderFields except the it take care or escaped quote (like '' or "")}
procedure ALExtractHeaderFieldsWithQuoteEscaped(Separators, WhiteSpace: TSysCharSet; Content: PChar; Strings: TStrings; Decode: Boolean; StripQuotes: Boolean = False);
var
  Head, Tail, NextTail: PChar;
  EOS, InQuote, LeadQuote: Boolean;
  QuoteChar: Char;
  ExtractedField: string;
  WhiteSpaceWithCRLF: TSysCharSet;
  SeparatorsWithCRLF: TSysCharSet;

  function DoStripQuotes(const S: string): string;
  var I: Integer;
      InStripQuote: Boolean;
      StripQuoteChar: Char;
  begin
    Result := S;
    InStripQuote := False;
    StripQuoteChar := #0;
    if StripQuotes then begin
      i := Length(Result);
      while i > 0 do begin
        if Result[I] in ['''', '"'] then begin
          if InStripQuote and (StripQuoteChar = Result[I]) then begin
            Delete(Result, I, 1);
            if (i > 1) and (Result[I-1] = StripQuoteChar) then dec(i)
            else InStripQuote := False;
          end
          else if not InStripQuote then begin
            StripQuoteChar := Result[I];
            InStripQuote := True;
            Delete(Result, I, 1);
          end
        end;
        dec(i);
      end;
    end;
  end;

Begin
  if (Content = nil) or (Content^ = #0) then Exit;
  WhiteSpaceWithCRLF := WhiteSpace + [#13, #10];
  SeparatorsWithCRLF := Separators + [#0, #13, #10, '"', ''''];
  Tail := Content;
  QuoteChar := #0;
  repeat
    while Tail^ in WhiteSpaceWithCRLF do Inc(Tail);
    Head := Tail;
    InQuote := False;
    LeadQuote := False;
    while True do begin
      while (InQuote and not (Tail^ in [#0, '"', ''''])) or not (Tail^ in SeparatorsWithCRLF) do Inc(Tail);
      if Tail^ in ['"',''''] then begin
        if (QuoteChar <> #0) and (QuoteChar = Tail^) then begin
          NextTail := Tail + 1;
          if NextTail^ = Tail^ then inc(tail)
          else QuoteChar := #0;
        end
        else If QuoteChar = #0 then begin
          LeadQuote := Head = Tail;
          QuoteChar := Tail^;
          if LeadQuote then Inc(Head);
        end;
        InQuote := QuoteChar <> #0;
        if InQuote then Inc(Tail)
        else Break;
      end else Break;
    end;
    if not LeadQuote and (Tail^ <> #0) and (Tail^ in ['"','''']) then Inc(Tail);
    EOS := Tail^ = #0;
    if Head^ <> #0 then begin
      SetString(ExtractedField, Head, Tail-Head);
      if Decode then Strings.Add(ALHTTPDecode(DoStripQuotes(ExtractedField)))
      else Strings.Add(DoStripQuotes(ExtractedField));
    end;
    Inc(Tail);
  until EOS;
end;

{*****************************************************}
Function ALAnsiUpperCaseNoDiacritic(S: string): string;
var Len1, Len2: Integer;
    i,J: integer;
    TmpStr1,
    TmpStr2: String;
begin
  result := '';
  If s = '' then exit;

  {upper the result}
  TmpStr1 := AnsiUppercase(s);
  Len1 := length(TmpStr1);

  {remove diacritic}
  Len2 := FoldString(MAP_COMPOSITE, PChar(TmpStr1), Len1, nil, 0);
  setlength(TmpStr2,len2);
  FoldString(MAP_COMPOSITE, PChar(TmpStr1), Len1, PChar(TmpStr2), len2);
  i := 1;
  J := 1;
  SetLength(result,len1);
  while J <= len1 do begin
    Result[j] := TmpStr2[i];
    if TmpStr1[j] <> TmpStr2[i] then inc(i,2)
    else inc(i);
    inc(j);
  end;
end;

{*****************************************************}
function ALGetStringFromFile(filename: string): string;
Var AFileStream: TfileStream;
begin
  AFileStream := TFileStream.Create(filename,fmOpenRead or fmShareDenyWrite);
  try

    If AFileStream.size > 0 then begin
      SetLength(Result, AFileStream.size);
      AfileStream.Read(Result[1],AfileStream.Size)
    end
    else Result := '';

  finally
    AfileStream.Free;
  end;
end;

{******************************************************************}
function ALGetStringFromFileWithoutUTF8BOM(filename: string): string;
Var AFileStream: TfileStream;
    aBOMStr: String;
    aSize: Integer;
begin
  AFileStream := TFileStream.Create(filename,fmOpenRead or fmShareDenyWrite);
  try

    aSize := AFileStream.size;
    If ASize > 0 then begin

      If Asize >= 3 then begin
        SetLength(aBOMStr,3);
        AfileStream.Read(aBOMStr[1],3);
        If AlUTF8DetectBOM(Pchar(aBOMStr), 3) then aSize := aSize - 3
        else AfileStream.Position := 0;
      end;

      If aSize > 0 then begin
        SetLength(Result, aSize);
        AfileStream.Read(Result[1],ASize)
      end
      else Result := '';

    end
    else Result := '';

  finally
    AfileStream.Free;
  end;
end;

{*************************************************}
procedure ALSaveStringtoFile(Str,filename: string);
Var AStringStream: TStringStream;
    AMemoryStream: TMemoryStream;
begin
  AMemoryStream := TMemoryStream.Create;
  try

    AStringStream := TStringStream.Create(str);
    try
      AmemoryStream.LoadFromStream(AstringStream);
      AmemoryStream.SaveToFile(filename);
    finally
      AStringStream.Free;
    end;

  finally
    AMemoryStream.Free;
  end;
end;

{************}
initialization
  ALInitStringReplaceFunct;
  ALInitPosExFunct;
  ALInitMovProc;
  ALInitPosFunct;
  ALInitCompareTextFunct;
  ALInitLowerCaseFunct;
  ALInitUpperCaseFunct;
end.
