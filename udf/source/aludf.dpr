{*************************************************************
Author:       Stéphane Vander Clock (SVanderClock@Arkadia.com)
www:          http://www.arkadia.com
EMail:        SVanderClock@Arkadia.com

product:      ALudf
Version:      1.52

Description:  Udf for Interbase

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

History :     20/11/2006: replace msvcrt.dll by msvcr71.dll do work with
                          FireBird 2.0
              21/11/2007: add aludf_httpencode function

Link :

Please send all your feedback to SVanderClock@Arkadia.com
**************************************************************}
library aludf;

uses
  windows,
  sysutils,
  HttpApp,
  alfcnUnicode,
  alfcnstring;

function ib_util_malloc(l: integer): pointer; cdecl; external 'ib_util.dll';

{***************************************************************}
function ALMakeResultString(Source: PChar): PChar; cdecl; export;
var Len: cardinal;
begin
  Len := StrLen(Source) + 1;
  result := ib_util_malloc(Len);

  if Len = 1 then result[0] := #0
  else alMove(Source^, result^, Len);
end;

{*********************************************************************}
function aludf_zerotoone(var anumber: integer): Integer; cdecl; export;
begin
  if anumber = 0 then result := 1
  else result := anumber;
end;

{****************************************************************}
function aludf_random(var range: integer): Integer; cdecl; export;
begin
  result := Random(range);
end;

{*************************************************************}
function aludf_pos(SubStr, Str: PChar): Integer; cdecl; export;
begin
  result := alPos(String(SubStr), String(Str));
end;

{********************************************************}
function aludf_length(Str: PChar): Integer; cdecl; export;
begin
  result := strlen(Str);
end;

{******************************************************************************}
function aludf_copy(Str: Pchar; var Index, Count: Integer):Pchar; cdecl; export;
begin
  result := ALMakeResultString(Pchar(alcopystr(String(Str), Index, Count)));
end;

{****************************************************}
function aludf_lower(Str: Pchar):Pchar; cdecl; export;
begin
  result := ALMakeResultString(StrLower(str));
end;

{*************************************************************}
function aludf_ansilowercase(Str: PChar): Pchar; cdecl; export;
begin
  result := ALMakeResultString(AnsiStrLower(str));
end;

{*************************************************************}
function aludf_ansiuppercase(Str: PChar): Pchar; cdecl; export;
begin
  result := ALMakeResultString(AnsiStrUpper(str));
end;

{*************************************************************}
function aludf_utf8lowercase(Str: PChar): Pchar; cdecl; export;
begin
  result := ALMakeResultString(pchar(alUTF8Lowercase(string(str))));
end;

{*************************************************************}
function aludf_utf8uppercase(Str: PChar): Pchar; cdecl; export;
begin
  result := ALMakeResultString(pchar(alUTF8uppercase(string(str))));
end;

{************************************************************}
function aludf_utf8normalize(Str: Pchar):Pchar; cdecl; export;
begin
  result := ALMakeResultString(pchar(ALUTF8Normalize(string(str))));
end;

{****************************************}
function aludf_crlf: Pchar; cdecl; export;
begin
  result := ALMakeResultString(Pchar(#13#10));
end;

{**********************************************************}
function aludf_ansitoutf8(Str: PChar): Pchar; cdecl; export;
begin
  result := ALMakeResultString(Pchar(AnsiToUTF8(String(Str))));
end;

{***********************************************************************}
function aludf_utf8lowercase1charupper(Str: PChar): Pchar; cdecl; export;
var tmpWideStr: WideString;
begin
  TmpWideStr := UTF8Decode(string(Str));
  result := ALMakeResultString(Pchar(utf8encode(WideUpperCase(copy(TmpWideStr,1,1)) + WideLowerCase(copy(TmpWideStr,2,MaxInt)))));
end;

{**********************************************************}
function aludf_httpencode(Str: PChar): Pchar; cdecl; export;
begin
  result := ALMakeResultString(pchar(httpencode(string(Str))));
end;

{************************************************************************************************************************}
function aludf_stringreplace(s, OldPattern, NewPattern: PChar; var replaceall, ignorecase: integer): Pchar; cdecl; export;
var aFlags: TReplaceFlags;
begin
  aFlags := [];
  if replaceall = 1 then aFlags := aFlags + [rfreplaceall];
  if ignorecase = 1 then aFlags := aFlags + [rfignorecase];
  result := ALMakeResultString(pchar(alstringreplace(String(s),String(OldPattern),String(NewPattern),aFlags)));
end;

{*******************************************************************************
DECLARE EXTERNAL function aludf_zerotoone
  INTEGER
  RETURNS INTEGER by VALUE
  ENTRY_POINT 'aludf_zerotoone' module_name 'aludf';

DECLARE EXTERNAL FUNCTION ALUDF_RANDOM
  INTEGER
  RETURNS INTEGER BY VALUE
  ENTRY_POINT 'aludf_random' MODULE_NAME 'aludf';

DECLARE EXTERNAL FUNCTION ALUDF_POS
  CSTRING(32767) CHARACTER SET ISO8859_1,
  CSTRING(32767) CHARACTER SET ISO8859_1
  RETURNS INTEGER BY VALUE
  ENTRY_POINT 'aludf_pos' MODULE_NAME 'aludf';

DECLARE EXTERNAL FUNCTION ALUDF_COPY
  CSTRING(32767) CHARACTER SET ISO8859_1,
  SMALLINT,
  SMALLINT
  RETURNS CSTRING(32767) CHARACTER SET ISO8859_1 FREE_IT
  ENTRY_POINT 'aludf_copy' MODULE_NAME 'aludf';

DECLARE EXTERNAL FUNCTION ALUDF_LENGTH
  CSTRING(32767) CHARACTER SET ISO8859_1
  RETURNS INTEGER BY VALUE
  ENTRY_POINT 'aludf_length' MODULE_NAME 'aludf';

DECLARE EXTERNAL FUNCTION ALUDF_LOWER
  CSTRING(32767) CHARACTER SET ISO8859_1
  RETURNS CSTRING(32767) CHARACTER SET ISO8859_1 FREE_IT
  ENTRY_POINT 'aludf_lower' MODULE_NAME 'aludf';

DECLARE EXTERNAL FUNCTION ALUDF_UTF8LOWERCASE
  CSTRING(32767) CHARACTER SET ISO8859_1
  RETURNS CSTRING(32767) CHARACTER SET ISO8859_1 FREE_IT
  ENTRY_POINT 'aludf_utf8lowercase' MODULE_NAME 'aludf';

DECLARE EXTERNAL FUNCTION ALUDF_UTF8UPPERCASE
  CSTRING(32767) CHARACTER SET ISO8859_1
  RETURNS CSTRING(32767) CHARACTER SET ISO8859_1 FREE_IT
  ENTRY_POINT 'aludf_utf8uppercase' MODULE_NAME 'aludf';

DECLARE EXTERNAL FUNCTION ALUDF_UTF8NORMALIZE
  CSTRING(32767) CHARACTER SET ISO8859_1
  RETURNS CSTRING(32767) CHARACTER SET ISO8859_1 FREE_IT
  ENTRY_POINT 'aludf_utf8normalize' MODULE_NAME 'aludf';

DECLARE EXTERNAL FUNCTION ALUDF_CRLF
  RETURNS CSTRING(32767) CHARACTER SET ISO8859_1 FREE_IT
  ENTRY_POINT 'aludf_crlf' MODULE_NAME 'aludf';

DECLARE EXTERNAL FUNCTION ALUDF_UTF8LOWERCASE1CHARUPPER
  CSTRING(32767) CHARACTER SET ISO8859_1
  RETURNS CSTRING(32767) CHARACTER SET ISO8859_1 FREE_IT
  ENTRY_POINT 'aludf_utf8lowercase1charupper' MODULE_NAME 'aludf';

DECLARE EXTERNAL FUNCTION ALUDF_HTTPENCODE
  CSTRING(32767) CHARACTER SET ISO8859_1
  RETURNS CSTRING(32767) CHARACTER SET ISO8859_1 FREE_IT
  ENTRY_POINT 'aludf_httpencode' MODULE_NAME 'aludf';

DECLARE EXTERNAL FUNCTION aludf_stringreplace
  CSTRING(32767),
  CSTRING(32767),
  CSTRING(32767),
  SMALLINT,
  SMALLINT
  RETURNS CSTRING(32767) FREE_IT
  ENTRY_POINT 'aludf_stringreplace' MODULE_NAME 'aludf';
*******************************************************************************}

exports
  aludf_zerotoone,
  aludf_random,
  aludf_pos,
  aludf_copy,
  aludf_length,
  aludf_lower,
  aludf_ansilowercase,
  aludf_ansiuppercase,
  aludf_utf8lowercase,
  aludf_utf8uppercase,
  aludf_utf8normalize,
  aludf_crlf,
  aludf_ansitoutf8,
  aludf_utf8lowercase1charupper,
  aludf_httpencode,
  aludf_stringreplace;

begin
  IsMultiThread := True;
  randomize;
end.
