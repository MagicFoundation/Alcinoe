/// REGEXP function for SQLite3 Database using PCRE library
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynSQLite3RegEx;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****



    REGEXP function for SQLite3 Database engine
   *********************************************

  This unit allows to enable a REGEXP operator to be used in SQlite3 statements.

  It will use the PCRE library as available since Delphi XE, or will rely
  on http://www.regular-expressions.info/download/TPerlRegEx.zip for older
  versions of Delphi.

  This unit will call directly the UTF-8 API of the PCRE library, and maintain
  a per-connection cache of compiled regular expressions to ensure the best
  performance possible.

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
{$ifdef ISDELPHIXE}
  // use direct PCRE library as available since Delphi XE
  RegularExpressionsAPI,
{$else}
  // download from http://www.regular-expressions.info/download/TPerlRegEx.zip
  PCRE,
{$endif}
  { TODO : use FLRE as pure pascal fast alernative }
  SysUtils,
  SynCommons,
  SynSQLite3;

/// register the REGEXP SQL function to a given SQLite3 engine instance
// - allow execution of statements as such:
// ! SELECT column FROM table WHERE column REGEXP '<here goes your expression>';
function CreateRegExpFunction(DB: TSQLite3DB): boolean;


implementation

type
  TPCREcache = record
    source: RawUTF8;
    compiled: PPCRE;
    extra: PPCREExtra;
  end;
  TPCREcaches = array of TPCREcache;
  PPCREcaches = ^TPCREcaches;

{$ifdef ISDELPHI103}
  {$define PCRE16}
  // System.RegularExpressionsAPI changed from a UTF-8 to UTF-16 calls :(
{$endif ISDELPHI103}


const
  // small regex compilation cache is enough in practice
  MAX_PCRECACHE = 16;

procedure InternalRegExp(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); cdecl;
var regexp, text: PUTF8Char;
    errMsg: PAnsiChar; // even for PCRE16 :)
    errPos: integer;
    found, result: boolean;
    i, n: integer;
    reg: PPCRE;
    cache: PPCREcaches;
    c: array[1..sizeof(TPCREcache)] of byte; // bulk mem block to avoid ref count
    {$ifdef PCRE16}
    temp: TSynTempBuffer;
    function ToUTF16(p: PUTF8Char; var temp: TSynTempBuffer): integer; inline;
    var len: integer;
    begin
      len := StrLen(p);
      temp.Init(len*2+2);
      result := UTF8ToWideChar(temp.buf,p,len);
    end;
    {$endif PCRE16}
  procedure CompileError;
  begin // sub procedure to avoid temp RawUTF8
    sqlite3.result_error(Context,pointer(FormatUTF8(
      'REGEXP "%": % at pos %',[regexp,errMsg,errPos])));
  end;
begin
  if argc<>2 then begin
    ErrorWrongNumberOfArgs(Context);
    Exit;
  end;
  cache := sqlite3.user_data(Context);
  regexp := sqlite3.value_text(argv[0]);
  if regexp=nil then begin
    sqlite3.result_error(Context,'REGEXP with no expression');
    exit;
  end;
  text := sqlite3.value_text(argv[1]);
  if text=nil then begin
    sqlite3.result_error(Context,'REGEXP with no text');
    exit;
  end;
  found := false;
  for i := 0 to high(cache^) do
    if StrComp(regexp,pointer(cache^[i].source))=0 then begin
      found := true;
      if i>0 then begin // always put last found item at first place
        move(cache^[i],c,sizeof(c));
        move(cache^[0],cache^[1],i*sizeof(c));
        move(c,cache^[0],sizeof(c));
      end;
      break;
    end;
  if not found then begin
    {$ifdef PCRE16}
    ToUTF16(regexp,temp);
    reg := pcre_compile(temp.buf,0,@errMsg,@errPos,nil);
    temp.Done;
    {$else}
    reg := pcre_compile(pointer(regexp),0,@errMsg,@errPos,nil);
    {$endif PCRE16}
    if reg=nil then begin
      CompileError;
      exit;
    end;
    n := length(cache^);
    if n=MAX_PCRECACHE then
      with cache^[MAX_PCRECACHE-1] do begin
        source := ''; // avoid memory leak
        pcre_dispose(compiled,extra,nil);
      end else
      SetLength(cache^,n+1);
    move(cache^[0],cache^[1],n*sizeof(c)); // new item is at first place
    with cache^[0] do begin
      pointer(source) := nil; // avoid GPF
      source := regexp;
      compiled := reg;
      extra := pcre_study(compiled,0,@errMsg);
    end;
  end;
  with cache^[0] do begin
    {$ifdef PCRE16}
    i := ToUTF16(text,temp);
    result := pcre_exec(compiled,extra,temp.buf,i,0,PCRE_NO_UTF16_CHECK,nil,0)>=0;
    temp.Done;
    {$else}
    result := pcre_exec(compiled,extra,pointer(text),StrLen(text),0,PCRE_NO_UTF8_CHECK,nil,0)>=0;
    {$endif PCRE16}
  end;
    // (faster with PCRE_NO_UTF8_CHECK option)
  sqlite3.result_int64(Context,ord(result));
end;

procedure InternalRegExpDestroy(cache: PPCREcaches); cdecl;
var i: integer;
begin
  if cache<>nil then begin
    for i := 0 to high(cache^) do
      pcre_dispose(cache^[i].compiled,cache^[i].extra,nil);
    Dispose(cache);
  end;
end;

function CreateRegExpFunction(DB: TSQLite3DB): boolean;
var cache: PPCREcaches;
begin
  if Assigned(sqlite3) and (DB<>0) then begin
    New(cache);
    result := sqlite3.create_function_v2(DB,'REGEXP',2,SQLITE_UTF8,cache,
      InternalRegExp,nil,nil,@InternalRegExpDestroy)=SQLITE_OK;
    if not result then
      Dispose(cache);
   end else
    result := false;
end;



end.
