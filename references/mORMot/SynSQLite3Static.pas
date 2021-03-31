/// SQLite3 3.34.1 Database engine - statically linked for Windows/Linux
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynSQLite3Static;

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
   - Alfred Glaenzer (alf)
   - Maciej Izak (hnb)

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


    Statically linked SQLite3 3.34.1 engine with optional AES encryption
   **********************************************************************

  To be declared in your project uses clause:  will fill SynSQlite3.sqlite3
  global variable with all statically linked .obj API entries.
  sqlite3 := TSQLite3LibraryStatic.Create; is called at unit initialization.

  Will work on Windows 32-bit or 64-bit (with Delphi or FPC, with expected
  .obj / .o) or Linux 32-bit 64-bit on Intel and ARM (with FPC, with the
  corresponding .o) under other platforms, this unit will just do nothing
  (but compile).

  To patch and compile the official SQlite3 amalgamation file, follow the
  instruction from SQLite3\amalgamation\ReadMe.md

  Uses TSQLite3LibraryDynamic to access external library (e.g. sqlite3.dll/.so)

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

{$ifdef NOSQLITE3STATIC} // conditional defined -> auto-load local .dll/.so
uses
  SysUtils,
  SynSQLite3;

implementation

uses
  SynCommons;

procedure DoInitialization;
begin
  FreeAndNil(sqlite3);
  try
    sqlite3 := TSQLite3LibraryDynamic.Create(SQLITE_LIBRARY_DEFAULT_NAME);
    sqlite3.ForceToUseSharedMemoryManager; // faster process
  except
    on E: Exception do
      {$ifdef LINUX}
      writeln(SQLITE_LIBRARY_DEFAULT_NAME+' initialization failed with ',
        E.ClassName,': ',E.Message);
      {$endif}
  end;
end;

initialization
  DoInitialization;

{$else NOSTATIC}

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$else}
  {$ifdef FPC}
  SynFPCLinux,
  BaseUnix,
  {$endif}
  {$ifdef KYLIX3}
  Types,
  LibC,
  SynKylix,
  {$endif}
  {$endif}
  Classes,
  SysUtils,
  SynCommons,
  SynSQLite3,
  SynCrypto;

type
  /// access class to the static .obj SQLite3 engine
  // - the intialization section of this unit calls:
  // ! sqlite3 := TSQLite3LibraryStatic.Create;
  // therefore, adding SynSQLite3Static to your uses clause is enough to use
  // the statically linked SQLite3 engine with SynSQLite3
  TSQLite3LibraryStatic = class(TSQLite3Library)
  public
    /// fill the internal API reference s with the static .obj engine
    constructor Create; override;
    /// unload the static library
    destructor Destroy; override;
  end;


/// use this procedure to change the password for an existing SQLite3 database file
// - convenient and faster alternative to the sqlite3.rekey() API call
// - conversion is done in-place at file level, with no SQL nor BTree pages
// involved, therefore it can process very big files with best possible speed
// - the OldPassWord must be correct, otherwise the resulting file will be corrupted
// - any password can be '' to mark no encryption as input or output
// - the password may be a JSON-serialized TSynSignerParams object, or will use
// AES-OFB-128 after SHAKE_128 with rounds=1000 and a fixed salt on plain password text
// - please note that this encryption is compatible only with SQlite3 files made
// with SynSQLiteStatic.pas unit (not external/official/wxsqlite3 dll)
// - implementation is NOT compatible with the official SQLite Encryption Extension
// (SEE) file format, not the wxsqlite3 extension, but is (much) faster thanks
// to our SynCrypto AES-NI enabled unit
// - if the key is not correct, a ESQLite3Exception will be raised with
// 'database disk image is malformed' (SQLITE_CORRUPT) at database opening
// - see also IsSQLite3File/IsSQLite3FileEncrypted functions
// - warning: this encryption is NOT compatible with our previous (<1.18.4413)
// cyphered format, which was much less safe (simple XOR on fixed tables), and
// was not working on any database size, making unclean patches to the official
// sqlite3.c amalgamation file, so is deprecated and unsupported any longer -
// see OldSQLEncryptTablePassWordToPlain() to convert your existing databases
function ChangeSQLEncryptTablePassWord(const FileName: TFileName;
  const OldPassWord, NewPassword: RawUTF8): boolean;

/// this function may be used to create a plain database file from an existing
// one encrypted with our old/deprecated/unsupported format (<1.18.4413)
// - then call ChangeSQLEncryptTablePassWord() to convert to the new safer format
procedure OldSQLEncryptTablePassWordToPlain(const FileName: TFileName;
  const OldPassWord: RawUTF8);

/// could be used to detect a database in old/deprecated/unsupported format (<1.18.4413)
// - to call OldSQLEncryptTablePassWordToPlain + ChangeSQLEncryptTablePassWord
// and switch to the new format
function IsOldSQLEncryptTable(const FileName: TFileName): boolean;

var
  /// global flag to use initial AES encryption scheme
  // - IV derivation was hardened in revision 1.18.4607 - set TRUE to this
  // global constant to use the former implementation (theoritically slightly
  // less resistant to brute force attacks) and convert existing databases
  ForceSQLite3LegacyAES: boolean;


implementation

{$ifdef FPC}  // FPC expects .o linking, and only one version including FTS

  {$ifdef MSWINDOWS}
    {$ifdef CPU64}
      const _PREFIX = '';
      {$L .\static\x86_64-win64\sqlite3.o}
      {$linklib .\static\x86_64-win64\libkernel32.a}
      {$linklib .\static\x86_64-win64\libgcc.a}
      {$linklib .\static\x86_64-win64\libmsvcrt.a}
    {$else}
      const _PREFIX = '_';
      {$L .\static\i386-win32\sqlite3.o}
      {$linklib .\static\i386-win32\libkernel32.a}
      {$linklib .\static\i386-win32\libgcc.a}
      {$linklib .\static\i386-win32\libmsvcrt.a}
    {$endif CPU64}
  {$endif MSWINDOWS}

  {$ifdef Darwin}
    const _PREFIX = '_';
    {$ifdef CPU64}
      {$linklib .\static\x86_64-darwin\libsqlite3.a}
    {$else}
      {$linklib .\static\i386-darwin\libsqlite3.a}
    {$endif}
  {$endif Darwin}

  {$ifdef ANDROID}
    const _PREFIX = '';
    {$ifdef CPUAARCH64}
      {$L .\static\aarch64-android\libsqlite3.a}
      {$L .\static\aarch64-android\libgcc.a}
    {$endif CPUAARCH64}
    {$ifdef CPUARM}
      {$L .\static\arm-android\libsqlite3.a}
      {$L .\static\arm-android\libgcc.a}
    {$endif CPUARM}
  {$endif ANDROID}

  {$ifdef FREEBSD}
    {$ifdef CPUX86}
    const _PREFIX = '';
    {$L .\static\i386-freebsd\sqlite3.o}
    {$ifdef FPC_CROSSCOMPILING}
      {$linklib .\static\i386-freebsd\libgcc.a}
    {$endif}
    {$endif CPUX86}
    {$ifdef CPUX64}
    const _PREFIX = '';
    {$L .\static\x86_64-freebsd\sqlite3.o}
    {$ifdef FPC_CROSSCOMPILING}
      {$linklib .\static\x86_64-freebsd\libgcc.a}
    {$endif}
    {$endif CPUX64}
  {$endif FREEBSD}

  {$ifdef OPENBSD}
    {$ifdef CPUX86}
      const _PREFIX = '';
      {$L .\static\i386-openbsd\sqlite3.o}
      {$ifdef FPC_CROSSCOMPILING}
        {$linklib .\static\i386-openbsd\libgcc.a}
      {$endif}
    {$endif CPUX86}
    {$ifdef CPUX64}
      const _PREFIX = '';
      {$L .\static\x86_64-openbsd\sqlite3.o}
      {$ifdef FPC_CROSSCOMPILING}
        {$linklib .\static\x86_64-openbsd\libgcc.a}
      {$endif}
    {$endif CPUX64}
  {$endif OPENBSD}

  {$if defined(Linux) and not defined(BSD) and not defined(Android)}
    const _PREFIX = '';
    {$ifdef CPUAARCH64}
      {$L .\static\aarch64-linux\sqlite3.o}
      {$L .\static\aarch64-linux\libgcc.a}
    {$endif CPUAARCH64}
    {$ifdef CPUARM}
      {$L .\static\arm-linux\sqlite3.o}
      {$L .\static\arm-linux\libgcc.a}
    {$endif CPUARM}
    {$ifdef CPUX86}
      {$L .\static\i386-linux\sqlite3.o}
      {$ifdef FPC_CROSSCOMPILING}
        {$linklib .\static\i386-linux\libgcc.a}
      {$endif}
    {$endif CPUX86}
    {$ifdef CPUX64}
      {$L .\static\x86_64-linux\sqlite3.o}
      {$ifdef FPC_CROSSCOMPILING}
        {$linklib .\static\x86_64-linux\libgcc.a}
      {$endif}
    {$endif CPUX64}
  {$ifend}

function log(x: double): double; cdecl; public name _PREFIX+'log'; export;
begin
  result := ln(x);
end;

{$ifdef MSWINDOWS}
{$ifdef CPUX86} // not a compiler intrinsic on x86
function _InterlockedCompareExchange(var Dest: longint; New,Comp: longint): longint; stdcall;
  public alias: '_InterlockedCompareExchange@12';
begin
  result := InterlockedCompareExchange(Dest,New,Comp);
end;
{$endif CPUX86}
{$endif MSWINDOWS}

{$ifdef DARWIN}

function moddi3(num, den: int64): int64; cdecl; public alias: '___moddi3';
begin
  result := num mod den;
end;
function umoddi3(num, den: uint64): uint64; cdecl; public alias: '___umoddi3';
begin
  result := num mod den;
end;
function divdi3(num, den: int64): int64; cdecl; public alias: '___divdi3';
begin
  result := num div den;
end;
function udivdi3(num, den: uint64): uint64; cdecl; public alias: '___udivdi3';
begin
  result := num div den;
end;

{$endif DARWIN}

{$ifdef ANDROID}
{$ifdef CPUARM}
function bswapsi2(num:uint32):uint32; cdecl; public alias: '__bswapsi2';
asm
  rev r0, r0	// reverse bytes in parameter and put into result register
  bx  lr
end;
function bswapdi2(num:uint64):uint64; cdecl; public alias: '__bswapdi2';
asm
  rev r2, r0  // r2 = rev(r0)
  rev r0, r1  // r0 = rev(r1)
  mov r1, r2  // r1 = r2 = rev(r0)
  bx  lr
end;
{$endif}
{$endif ANDROID}

{$else FPC}

  // Delphi has a diverse linking strategy, since $linklib doesn't exist :(
  {$ifdef MSWINDOWS}
    {$ifdef CPU64}
      {$L sqlite3.o}  // compiled with C++ Builder 10.3 Community Edition bcc64
    {$else}
      {$L sqlite3.obj}  // compiled with free Borland C++ Compiler 5.5
    {$endif}
  {$else}
  {$ifdef KYLIX3} // in practice, we failed to compile SQLite3 with gcc 2 :(
    {$L kylix/sqlite3/sqlite3.o}
    {$L kylix/sqlite3/_divdi3.o}
    {$L kylix/sqlite3/_moddi3.o}
    {$L kylix/sqlite3/_udivdi3.o}
    {$L kylix/sqlite3/_umoddi3.o}
    {$L kylix/sqlite3/_cmpdi2.o}
  {$endif KYLIX3}
  {$endif MSWINDOWS}

// those functions will be called only under Delphi + Win32/Win64

function malloc(size: cardinal): Pointer; cdecl; { always cdecl }
begin
  GetMem(Result, size);
end;

procedure free(P: Pointer); cdecl; { always cdecl }
begin
  FreeMem(P);
end;

function realloc(P: Pointer; Size: Integer): Pointer; cdecl; { always cdecl }
begin
  result := P;
  ReallocMem(result,Size);
end;

function rename(oldname, newname: PUTF8Char): integer; cdecl; { always cdecl }
begin
  if RenameFile(UTF8DecodeToString(oldname,StrLen(oldname)),
                UTF8DecodeToString(newname,StrLen(newname))) then
    result := 0 else
    result := -1;
end;

{$ifdef MSWINDOWS}
{$ifdef CPU32} // Delphi Win32 will link static Borland C++ sqlite3.obj

// we then implement all needed Borland C++ runtime functions in pure pascal:

function _ftol: Int64;
// Borland C++ float to integer (Int64) conversion
asm
  jmp System.@Trunc  // FST(0) -> EDX:EAX, as expected by BCC32 compiler
end;

function _ftoul: Int64;
// Borland C++ float to integer (Int64) conversion
asm
  jmp System.@Trunc  // FST(0) -> EDX:EAX, as expected by BCC32 compiler
end;

var __turbofloat: word; { not used, but must be present for linking }

// Borland C++ and Delphi share the same low level Int64 _ll*() functions:

procedure _lldiv;
asm
  jmp System.@_lldiv
end;

procedure _lludiv;
asm
  jmp System.@_lludiv
end;

procedure _llmod;
asm
  jmp System.@_llmod
end;

procedure _llmul;
asm
  jmp System.@_llmul
end;

procedure _llumod;
asm
  jmp System.@_llumod
end;

procedure _llshl;
asm
  jmp System.@_llshl
end;

procedure _llshr;
asm
{$ifndef ENHANCEDRTL} // need this code for Borland/CodeGear default System.pas
  shrd    eax, edx, cl
  sar     edx, cl
  cmp     cl, 32
  jl      @@Done
  cmp     cl, 64
  jge     @@RetSign
  mov     eax, edx
  sar     edx, 31
  ret
@@RetSign:
  sar     edx, 31
  mov     eax, edx
@@Done:
{$else}
  // our customized System.pas didn't forget to put _llshr in its interface :)
  jmp System.@_llshr
{$endif}
end;

procedure _llushr;
asm
  jmp System.@_llushr
end;

function log(const val: double): double; cdecl; { always cdecl }
asm
  fld qword ptr val
  fldln2
  fxch
  fyl2x
end;

{$endif CPU32}
{$endif MSWINDOWS}

function memset(P: Pointer; B: Integer; count: Integer): pointer; cdecl; { always cdecl }
// a fast full pascal version of the standard C library function
begin
  FillCharFast(P^, count, B);
  result := P;
end;

function memmove(dest, source: pointer; count: Integer): pointer; cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'memmove'{$else}'_memmove'{$endif};{$endif}
// a fast full pascal version of the standard C library function
begin
  MoveFast(source^, dest^, count); // move() is overlapping-friendly
  result := dest;
end;

function memcpy(dest, source: Pointer; count: Integer): pointer; cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'memcpy'{$else}'_memcpy'{$endif};{$endif}
// a fast full pascal version of the standard C library function
begin
  MoveFast(source^, dest^, count);
  result := dest;
end;

function strlen(p: PAnsiChar): integer; cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'strlen'{$else}'_strlen'{$endif};{$endif}
// a fast full pascal version of the standard C library function
begin // called only by some obscure FTS3 functions (normal code use dedicated functions)
  result := SynCommons.StrLen(pointer(p));
end;

function strcmp(p1,p2: PAnsiChar): integer; cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'strcmp'{$else}'_strcmp'{$endif};{$endif}
// a fast full pascal version of the standard C library function
begin // called only by some obscure FTS3 functions (normal code use dedicated functions)
  result := SynCommons.StrComp(p1,p2);
end;

function strcspn(str,reject: PAnsiChar): integer; cdecl;
  {$ifdef FPC}public name{$ifdef CPU64}'strcspn'{$else}'_strcspn'{$endif};{$endif}
begin // called e.g. during LIKE process
  result := SynCommons.strcspn(str,reject); // use SSE4.2 if available
end;

function strrchr(s: PAnsiChar; c: AnsiChar): PAnsiChar; cdecl;
  {$ifdef FPC}public name{$ifdef CPU64}'strrchr'{$else}'_strrchr'{$endif};{$endif}
begin // simple full pascal version of the standard C library function
  result := nil;
  if s<>nil then
    while s^<>#0 do begin
      if s^=c then
        result := s;
      inc(s);
    end;
end;

function memcmp(p1, p2: pByte; Size: integer): integer; cdecl; { always cdecl }
{$ifdef FPC}
  public name{$ifdef CPU64}'memcmp'{$else}'_memcmp'{$endif};
begin
  result := CompareByte(p1,p2,Size); // use FPC
end;
{$else}
begin // full pascal version of the standard C library function
  if (p1<>p2) and (Size<>0) then
    if p1<>nil then
      if p2<>nil then begin
        repeat
          if p1^=p2^ then begin
            inc(p1);
            inc(p2);
            dec(Size);
            if Size<>0 then
              continue else break;
          end;
          result := p1^-p2^;
          exit;
        until false;
        result := 0;
      end else
      result := 1 else
    result := -1 else
  result := 0;
end;
{$endif}

function strncmp(p1, p2: PByte; Size: integer): integer; cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'strncmp'{$else}'_strncmp'{$endif};{$endif}
var i: integer;
begin // a fast full pascal version of the standard C library function
  for i := 1 to Size do begin
    result := p1^-p2^;
    if (result<>0) or (p1^=0) then
      exit;
    inc(p1);
    inc(p2);
  end;
  result := 0;
end;

type
  // qsort() is used if SQLITE_ENABLE_FTS3 is defined
  // this function type is defined for calling termDataCmp() in sqlite3.c
  qsort_compare_func = function(P1,P2: pointer): integer; cdecl; { always cdecl }

procedure QuickSortPtr(base: PPointerArray; L, R: Integer; comparF: qsort_compare_func);
var I, J, P: Integer;
    PP, C: PAnsiChar;
begin
  repeat // from SQLite (FTS), With=sizeof(PAnsiChar) AFAIK
    I := L;
    J := R;
    P := (L+R) shr 1;
    repeat
      PP := @base[P];
      while comparF(@base[I],PP)<0 do
        inc(I);
      while comparF(@base[J],PP)>0 do
        dec(J);
      if I<=J then begin
        C := base[I];
        base[I] := base[J];
        base[J] := C; // fast memory exchange
        if P=I then P := J else if P=J then P := I;
        inc(I);
        dec(J);
      end;
    until I>J;
    if L<J then
      QuickSortPtr(base, L, J, comparF);
    L := I;
  until I>=R;
end;

procedure QuickSort(baseP: PAnsiChar; Width: integer; L, R: Integer; comparF: qsort_compare_func);
// code below is very fast and optimized
  procedure Exchg(P1,P2: PAnsiChar; Size: integer);
  var B: AnsiChar;
      i: integer;
  begin
    for i := 0 to Size-1 do begin
      B := P1[i];
      P1[i] := P2[i];
      P2[i] := B;
    end;
  end;
var I, J, P: Integer;
    PP, C: PAnsiChar;
begin
  repeat // generic sorting algorithm
    I := L;
    J := R;
    P := (L+R) shr 1;
    repeat
      PP := baseP+P*Width; // compute PP at every loop, since P may change
      C := baseP+I*Width;
      while comparF(C,PP)<0 do begin
        inc(I);
        inc(C,width); // avoid slower multiplication in loop
      end;
      C := baseP+J*Width;
      while comparF(C,PP)>0 do begin
        dec(J);
        dec(C,width); // avoid slower multiplication in loop
      end;
      if I<=J then begin
        Exchg(baseP+I*Width,baseP+J*Width,Width); // fast memory exchange
        if P=I then P := J else if P=J then P := I;
        inc(I);
        dec(J);
      end;
    until I>J;
    if L<J then
      QuickSort(baseP, Width, L, J, comparF);
    L := I;
  until I>=R;
end;

procedure qsort(baseP: pointer; NElem, Width: integer; comparF: pointer); cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'qsort'{$else}'_qsort'{$endif};{$endif}
// a fast full pascal version of the standard C library function
begin
  if (cardinal(NElem)>1) and (Width>0) then
    if Width=sizeof(pointer) then
      QuickSortPtr(baseP, 0, NElem-1, qsort_compare_func(comparF)) else
      QuickSort(baseP, Width, 0, NElem-1, qsort_compare_func(comparF));
end;

var
  { as standard C library documentation states:
  Statically allocated buffer, shared by the functions gmtime() and localtime().
  Each call of these functions overwrites the content of this structure.
  -> since timing is not thread-dependent, it's OK to share this buffer :) }
  atm: packed record
    tm_sec: Integer;            { Seconds.      [0-60] (1 leap second) }
    tm_min: Integer;            { Minutes.      [0-59]  }
    tm_hour: Integer;           { Hours.        [0-23]  }
    tm_mday: Integer;           { Day.          [1-31]  }
    tm_mon: Integer;            { Month.        [0-11]  }
    tm_year: Integer;           { Year          - 1900. }
    tm_wday: Integer;           { Day of week.  [0-6]   }
    tm_yday: Integer;           { Days in year. [0-365] }
    tm_isdst: Integer;          { DST.          [-1/0/1]}
    __tm_gmtoff: Integer;       { Seconds east of UTC.  }
    __tm_zone: ^Char;           { Timezone abbreviation.}
  end;

function localtime64(const t: Int64): pointer; cdecl; { always cdecl }
  {$ifdef FPC}public name '__imp__localtime64';{$endif}
// a fast full pascal version of the standard C library function
var {$ifdef MSWINDOWS}
    uTm: TFileTime;
    lTm: TFileTime;
    {$endif}
    S: TSystemTime;
begin
  {$ifdef MSWINDOWS}
  Int64(uTm) := (t+11644473600)*10000000; // unix time to dos file time
  FileTimeToLocalFileTime(uTM,lTM);
  FileTimeToSystemTime(lTM,S);
  atm.tm_sec := S.wSecond;
  atm.tm_min := S.wMinute;
  atm.tm_hour := S.wHour;
  atm.tm_mday := S.wDay;
  atm.tm_mon := S.wMonth-1;
  atm.tm_year := S.wYear-1900;
  atm.tm_wday := S.wDayOfWeek;
  {$else}
  GetNowUTCSystem(S);
  atm.tm_sec := S.Second;
  atm.tm_min := S.Minute;
  atm.tm_hour := S.Hour;
  atm.tm_mday := S.Day;
  atm.tm_mon := S.Month-1;
  atm.tm_year := S.Year-1900;
  atm.tm_wday := S.Day;
  {$endif}
  result := @atm;
end;

function localtime(t: PCardinal): pointer; cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'localtime32'{$else}'__localtime32'{$endif};{$endif}
begin
  result := localtime64(t^);
end;

{$ifdef MSWINDOWS}

const
  msvcrt = 'msvcrt.dll';
  kernel = 'kernel32.dll';

function _beginthreadex(security: pointer; stksize: dword;
  start,arg: pointer; flags: dword; var threadid: dword): THandle; cdecl; external msvcrt;
procedure _endthreadex(exitcode: dword); cdecl; external msvcrt;

{$ifdef CPU64}

// Delphi Win64 will link its own static sqlite3.o (diverse from FPC's)

function _log(x: double): double; export; // to link LLVM bcc64 compiler
begin
  result := ln(x);
end;

function log(x: double): double; export; // to link old non-LLVM bcc64 compiler
begin
  result := ln(x);
end;

procedure __chkstk;
begin
end;

procedure __faststorefence;
asm
  .noframe
  mfence;
end;

var
  _finf: double = 1.0 / 0.0; // compiles to some double infinity constant
  _fltused: Int64 = 0; // to link old non-LLVM bcc64 compiler

{$endif CPU64}

{$else MSWINDOWS}

{$ifdef KYLIX3}

function close(Handle: Integer): Integer; cdecl;
  external libcmodulename;
function read(Handle: Integer; var Buffer; Count: size_t): ssize_t; cdecl;
  external libcmodulename;
function write(Handle: Integer; const Buffer; Count: size_t): ssize_t; cdecl;
  external libcmodulename;

function __fixunsdfdi(a: double): Int64; cdecl;
begin
  if a<0 then
    result := 0 else
    result := round(a);
end;

{$endif KYLIX3}

{$endif MSWINDOWS}

{$endif FPC}

// some external functions as expected by codecext.c and our sqlite3mc.c wrapper

procedure CodecGenerateKey(var aes: TAES; userPassword: pointer; passwordLength: integer);
var s: TSynSigner;
    k: THash512Rec;
begin
  s.PBKDF2(userPassword,passwordLength,k,'J6CuDftfPr22FnYn');
  s.AssignTo(k,aes,{encrypt=}true);
end;

function CodecGetReadKey(codec: pointer): PAES; cdecl; external;
function CodecGetWriteKey(codec: pointer): PAES; cdecl; external;

procedure CodecGenerateReadKey(codec: pointer;
  userPassword: PAnsiChar; passwordLength: integer); cdecl;
  {$ifdef FPC}public name _PREFIX+'CodecGenerateReadKey';{$endif} export;
begin
  CodecGenerateKey(CodecGetReadKey(codec)^,userPassword,passwordLength);
end;

procedure CodecGenerateWriteKey(codec: pointer;
  userPassword: PAnsiChar; passwordLength: integer); cdecl;
  {$ifdef FPC}public name _PREFIX+'CodecGenerateWriteKey';{$endif} export;
begin
  CodecGenerateKey(CodecGetWriteKey(codec)^,userPassword,passwordLength);
end;

procedure CodecAESProcess(page: cardinal; data: PAnsiChar; len: integer;
  aes: PAES; encrypt: boolean);
var plain: Int64;    // bytes 16..23 should always be unencrypted
    iv: THash128Rec; // is genuine and AES-protected (since not random)
begin
  if (len and AESBlockMod<>0) or (len<=0) or (integer(page)<=0) then
    raise ESQLite3Exception.CreateUTF8('CodecAESProcess(page=%,len=%)', [page,len]);
  iv.c0 := page xor 668265263; // prime-based initialization
  iv.c1 := page*2654435761;
  iv.c2 := page*2246822519;
  iv.c3 := page*3266489917;
  if not ForceSQLite3LegacyAES then
    aes^.Encrypt(iv.b); // avoid potential brute force attack
  len := len shr AESBlockShift;
  if page=1 then // ensure header bytes 16..23 are stored unencrypted
    if (PInt64(data)^=SQLITE_FILE_HEADER128.lo) and
       (data[21]=#64) and (data[22]=#32) and (data[23]=#32) then
      if encrypt then begin
        plain := PInt64(data+16)^;
        aes^.DoBlocksOFB(iv.b,data+16,data+16,len-1);
        PInt64(data+8)^ := PInt64(data+16)^; // 8..15 are encrypted bytes 16..23
        PInt64(data+16)^ := plain;
      end else begin
        PInt64(data+16)^ := PInt64(data+8)^;
        aes^.DoBlocksOFB(iv.b,data+16,data+16,len-1);
        if (data[21]=#64) and (data[22]=#32) and (data[23]=#32) then
          PHash128(data)^ := SQLITE_FILE_HEADER128.b else
          FillZero(PHash128(data)^); // report incorrect password
      end else
      FillZero(PHash128(data)^) else
    aes^.DoBlocksOFB(iv.b,data,data,len);
end;

function CodecEncrypt(codec: pointer; page: integer; data: PAnsiChar;
  len, useWriteKey: integer): integer; cdecl;
  {$ifdef FPC}public name _PREFIX+'CodecEncrypt';{$endif} export;
begin
  if useWriteKey=1 then
     CodecAESProcess(page,data,len,CodecGetWriteKey(codec),true) else
     CodecAESProcess(page,data,len,CodecGetReadKey(codec),true);
  result := SQLITE_OK;
end;

function CodecDecrypt(codec: pointer; page: integer;
data: PAnsiChar; len: integer): integer; cdecl;
  {$ifdef FPC}public name _PREFIX+'CodecDecrypt';{$endif} export;
begin
  CodecAESProcess(page,data,len,CodecGetReadKey(codec),false);
  result := SQLITE_OK;
end;

function CodecTerm(codec: pointer): integer; cdecl;
  {$ifdef FPC}public name _PREFIX+'CodecTerm';{$endif} export;
begin
  CodecGetReadKey(codec)^.Done;
  CodecGetWriteKey(codec)^.Done;
  result := SQLITE_OK;
end;

function ChangeSQLEncryptTablePassWord(const FileName: TFileName;
  const OldPassWord, NewPassword: RawUTF8): boolean;
var F: THandle;
    bufsize,page,pagesize,pagecount,n,p,read: cardinal;
    head: THash256Rec;
    buf: PAnsiChar;
    temp: RawByteString;
    size: TQWordRec;
    posi: Int64;
    old, new: TAES;
begin
  result := false;
  if OldPassword=NewPassword then
    exit;
  F := FileOpen(FileName,fmOpenReadWrite);
  if F<>INVALID_HANDLE_VALUE then
  try
    if OldPassword<>'' then
      CodecGenerateKey(old,pointer(OldPassword),length(OldPassWord));
    if NewPassword<>'' then
      CodecGenerateKey(new,pointer(NewPassword),length(NewPassWord));
    size.L := GetFileSize(F,@size.H);
    read := FileRead(F,head,SizeOf(head));
    if read<>SizeOf(head) then
      exit;
    if size.V>4 shl 20 then // use up to 4MB of R/W buffer
      bufsize := 4 shl 20 else
      bufsize := size.L;
    pagesize := cardinal(head.b[16]) shl 8+head.b[17];
    pagecount := size.V div pagesize;
    if (pagesize<1024) or (pagesize and AESBlockMod<>0) or (pagesize>bufsize) or
       (QWord(pagecount)*pagesize<>size.V) or (head.d0<>SQLITE_FILE_HEADER128.Lo) or
       ((head.d1=SQLITE_FILE_HEADER128.Hi)<>(OldPassWord='')) then
      exit;
    FileSeek64(F,0,soFromBeginning);
    SetLength(temp,bufsize);
    posi := 0;
    page := 1;
    while page<=pagecount do begin
      n := bufsize div pagesize;
      read := pagecount-page+1;
      if read < n then
        n := read;
      buf := pointer(temp);
      read := FileRead(F,buf^,pagesize*n);
      if read<>pagesize*n then
        exit; // stop on any read error
      for p := 0 to n-1 do begin
        if OldPassword<>'' then begin
          CodecAESProcess(page+p,buf,pagesize,@old,false);
          if (p=0) and (page=1) and (PInteger(buf)^=0) then
            exit; // OldPassword is obviously incorrect
        end;
        if NewPassword<>'' then
          CodecAESProcess(page+p,buf,pagesize,@new,true);
        inc(buf,pagesize);
      end;
      FileSeek64(F,posi,soFromBeginning);
      FileWrite(F,pointer(temp)^,pagesize*n); // update in-place
      inc(posi,pagesize*n);
      inc(page,n);
    end;
    result := true;
  finally
    FileClose(F);
    if OldPassword<>'' then
      old.Done;
    if NewPassword<>'' then
      new.Done;
  end;
end;

function IsOldSQLEncryptTable(const FileName: TFileName): boolean;
var F: THandle;
    Header: array[0..2047] of byte;
begin
  result := false;
  F := FileOpen(FileName,fmOpenRead or fmShareDenyNone);
  if F=INVALID_HANDLE_VALUE then
    exit;
  if (FileRead(F,Header,SizeOf(Header))=SizeOf(Header)) and
      // see https://www.sqlite.org/fileformat.html (4 in big endian = 1024 bytes)
     (PWord(@Header[16])^=4) and
     IsEqual(PHash128(@Header)^,SQLITE_FILE_HEADER128.b) then
    if not(Header[1024] in [5,10,13]) then
      // B-tree leaf Type to be either 5 (interior) 10 (index) or 13 (table)
      result := true;
  FileClose(F);
end;

procedure OldSQLEncryptTablePassWordToPlain(const FileName: TFileName;
  const OldPassWord: RawUTF8);
  const
    SQLEncryptTableSize = $4000;
  procedure CreateSQLEncryptTableBytes(const PassWord: RawUTF8; Table: PByteArray);
  // very fast table (private key) computation from a given password
  // - use a simple prime-based random generator, strong enough for common use
  // - execution speed and code size was the goal here: can be easily broken
  // - SynCrypto proposes SHA-256 and AES-256 for more secure encryption
  var i, j, k, L: integer;
  begin
    L := length(Password)-1;
    j := 0;
    k := integer(L*ord(Password[1]))+134775813; // initial value, prime number derivated
    for i := 0 to SQLEncryptTableSize-1 do begin
      Table^[i] := (ord(PassWord[j+1])) xor byte(k);
      k := Integer(k*3+i); // fast prime-based pseudo random generator
      if j=L then
        j := 0 else
        inc(j);
    end;
  end;
  procedure XorOffset(P: PByte; Index, Count: cardinal; SQLEncryptTable: PByteArray);
  var Len: cardinal;
  begin // fast and simple Cypher using Index (= offset in file)
    if Count>0 then
    repeat
      Index := Index and (SQLEncryptTableSize-1);
      Len := SQLEncryptTableSize-Index;
      if Len>Count then
        Len := Count;
      XorMemory(pointer(P),@SQLEncryptTable^[Index],Len);
      inc(P,Len);
      inc(Index,Len);
      dec(Count,Len);
    until Count=0;
  end;
var F: THandle;
    R: integer;
    Buf: array[word] of byte; // temp buffer for read/write (64KB is enough)
    Size, Posi: TQWordRec;
    OldP: array[0..SQLEncryptTableSize-1] of byte; // 2x16KB tables
begin
  F := FileOpen(FileName,fmOpenReadWrite);
  if F=INVALID_HANDLE_VALUE then
    exit;
  Size.L := GetFileSize(F,@Size.H);
  if (Size.L<=1024) and (Size.H=0) then begin
    FileClose(F); // file is to small to be modified
    exit;
  end;
  if OldPassword<>'' then
    CreateSQLEncryptTableBytes(OldPassWord,@OldP);
  Posi.V := 1024; // don't change first page, which is uncrypted
  FileSeek(F,1024,soFromBeginning);
  while Posi.V<Size.V do begin
    R := FileRead(F,Buf,sizeof(Buf)); // read buffer
    if R<0 then
      break; // stop on any read error
    if OldPassword<>'' then
      XorOffset(@Buf,Posi.L,R,@OldP); // uncrypt with old key
    FileSeek64(F,Posi.V,soFromBeginning);
    FileWrite(F,Buf,R); // update buffer
    inc(Posi.V,cardinal(R));
  end;
  FileClose(F);
end;

function sqlite3_initialize: integer; cdecl; external;
function sqlite3_shutdown: integer; cdecl; external;
function sqlite3_open(filename: PUTF8Char; var DB: TSQLite3DB): integer; cdecl; external;
function sqlite3_open_v2(filename: PUTF8Char; var DB: TSQLite3DB; flags: Integer; vfs: PUTF8Char): integer; cdecl; external;
function sqlite3_close(DB: TSQLite3DB): integer; cdecl; external;
function sqlite3_key(DB: TSQLite3DB; key: pointer; keyLen: Integer): integer; cdecl; external;
function sqlite3_rekey(DB: TSQLite3DB; key: pointer; keyLen: Integer): integer; cdecl; external;
function sqlite3_create_function(DB: TSQLite3DB; FunctionName: PUTF8Char;
  nArg, eTextRep: integer; pApp: pointer; xFunc, xStep: TSQLFunctionFunc;
  xFinal: TSQLFunctionFinal): Integer; cdecl; external;
function sqlite3_create_function_v2(DB: TSQLite3DB; FunctionName: PUTF8Char;
  nArg, eTextRep: integer; pApp: pointer; xFunc, xStep: TSQLFunctionFunc;
  xFinal: TSQLFunctionFinal; xDestroy: TSQLDestroyPtr): Integer; cdecl; external;
function sqlite3_create_window_function(DB: TSQLite3DB; FunctionName: PUTF8Char;
  nArg, eTextRep: integer; pApp: pointer; xStep: TSQLFunctionFunc;
  xFinal, xValue: TSQLFunctionFinal; xInverse: TSQLFunctionFunc; xDestroy: TSQLDestroyPtr): Integer;   cdecl; external;
function sqlite3_create_collation(DB: TSQLite3DB; CollationName: PUTF8Char;
  StringEncoding: integer; CollateParam: pointer; cmp: TSQLCollateFunc): integer; cdecl; external;
function sqlite3_libversion: PUTF8Char; cdecl; external;
function sqlite3_errmsg(DB: TSQLite3DB): PAnsiChar; cdecl; external;
function sqlite3_extended_errcode(DB: TSQLite3DB): integer; cdecl; external;
function sqlite3_last_insert_rowid(DB: TSQLite3DB): Int64; cdecl; external;
function sqlite3_busy_timeout(DB: TSQLite3DB; Milliseconds: integer): integer; cdecl; external;
function sqlite3_busy_handler(DB: TSQLite3DB;
  CallbackPtr: TSQLBusyHandler; user: Pointer): integer;  cdecl; external;
function sqlite3_prepare_v2(DB: TSQLite3DB; SQL: PUTF8Char; SQL_bytes: integer;
  var S: TSQLite3Statement; var SQLtail: PUTF8Char): integer; cdecl; external;
function sqlite3_finalize(S: TSQLite3Statement): integer; cdecl; external;
function sqlite3_next_stmt(DB: TSQLite3DB; S: TSQLite3Statement): TSQLite3Statement; cdecl; external;
function sqlite3_reset(S: TSQLite3Statement): integer; cdecl; external;
function sqlite3_stmt_readonly(S: TSQLite3Statement): integer; cdecl; external;
function sqlite3_step(S: TSQLite3Statement): integer; cdecl; external;
function sqlite3_column_count(S: TSQLite3Statement): integer; cdecl; external;
function sqlite3_column_type(S: TSQLite3Statement; Col: integer): integer; cdecl; external;
function sqlite3_column_decltype(S: TSQLite3Statement; Col: integer): PAnsiChar; cdecl; external;
function sqlite3_column_name(S: TSQLite3Statement; Col: integer): PUTF8Char; cdecl; external;
function sqlite3_column_bytes(S: TSQLite3Statement; Col: integer): integer; cdecl; external;
function sqlite3_column_value(S: TSQLite3Statement; Col: integer): TSQLite3Value; cdecl; external;
function sqlite3_column_double(S: TSQLite3Statement; Col: integer): double; cdecl; external;
function sqlite3_column_int(S: TSQLite3Statement; Col: integer): integer; cdecl; external;
function sqlite3_column_int64(S: TSQLite3Statement; Col: integer): int64; cdecl; external;
function sqlite3_column_text(S: TSQLite3Statement; Col: integer): PUTF8Char; cdecl; external;
function sqlite3_column_text16(S: TSQLite3Statement; Col: integer): PWideChar; cdecl; external;
function sqlite3_column_blob(S: TSQLite3Statement; Col: integer): PAnsiChar; cdecl; external;
function sqlite3_value_type(Value: TSQLite3Value): integer; cdecl; external;
function sqlite3_value_numeric_type(Value: TSQLite3Value): integer; cdecl; external;
function sqlite3_value_bytes(Value: TSQLite3Value): integer; cdecl; external;
function sqlite3_value_double(Value: TSQLite3Value): double; cdecl; external;
function sqlite3_value_int64(Value: TSQLite3Value): Int64; cdecl; external;
function sqlite3_value_text(Value: TSQLite3Value): PUTF8Char; cdecl; external;
function sqlite3_value_blob(Value: TSQLite3Value): pointer; cdecl; external;
procedure sqlite3_result_null(Context: TSQLite3FunctionContext); cdecl; external;
procedure sqlite3_result_int64(Context: TSQLite3FunctionContext; Value: Int64); cdecl; external;
procedure sqlite3_result_double(Context: TSQLite3FunctionContext; Value: double); cdecl; external;
procedure sqlite3_result_blob(Context: TSQLite3FunctionContext; Value: Pointer;
  Value_bytes: Integer=0; DestroyPtr: TSQLDestroyPtr=SQLITE_TRANSIENT); cdecl; external;
procedure sqlite3_result_text(Context: TSQLite3FunctionContext; Value: PUTF8Char;
  Value_bytes: Integer=-1; DestroyPtr: TSQLDestroyPtr=SQLITE_TRANSIENT); cdecl; external;
procedure sqlite3_result_value(Context: TSQLite3FunctionContext; Value: TSQLite3Value); cdecl; external;
procedure sqlite3_result_error(Context: TSQLite3FunctionContext; Msg: PUTF8Char; MsgLen: integer=-1); cdecl; external;
function sqlite3_user_data(Context: TSQLite3FunctionContext): pointer; cdecl; external;
function sqlite3_context_db_handle(Context: TSQLite3FunctionContext): TSQLite3DB; cdecl; external;
function sqlite3_aggregate_context(Context: TSQLite3FunctionContext;
   nBytes: integer): pointer; cdecl; external;
function sqlite3_bind_text(S: TSQLite3Statement; Param: integer;
  Text: PUTF8Char; Text_bytes: integer=-1; DestroyPtr: TSQLDestroyPtr=SQLITE_TRANSIENT): integer;
  cdecl; external;
function sqlite3_bind_blob(S: TSQLite3Statement; Param: integer; Buf: pointer; Buf_bytes: integer;
  DestroyPtr: TSQLDestroyPtr=SQLITE_TRANSIENT): integer; cdecl; external;
function sqlite3_bind_zeroblob(S: TSQLite3Statement; Param: integer; Size: integer): integer; cdecl; external;
function sqlite3_bind_double(S: TSQLite3Statement; Param: integer; Value: double): integer; cdecl; external;
function sqlite3_bind_int(S: TSQLite3Statement; Param: integer; Value: integer): integer; cdecl; external;
function sqlite3_bind_int64(S: TSQLite3Statement; Param: integer; Value: Int64): integer; cdecl; external;
function sqlite3_bind_null(S: TSQLite3Statement; Param: integer): integer; cdecl; external;
function sqlite3_clear_bindings(S: TSQLite3Statement): integer; cdecl; external;
function sqlite3_bind_parameter_count(S: TSQLite3Statement): integer; cdecl; external;
function sqlite3_blob_open(DB: TSQLite3DB; DBName, TableName, ColumnName: PUTF8Char;
  RowID: Int64; Flags: Integer; var Blob: TSQLite3Blob): Integer; cdecl; external;
function sqlite3_blob_reopen(DB: TSQLite3DB; RowID: Int64): Integer; cdecl; external;
function sqlite3_blob_close(Blob: TSQLite3Blob): Integer; cdecl; external;
function sqlite3_blob_read(Blob: TSQLite3Blob; const Data; Count, Offset: Integer): Integer; cdecl; external;
function sqlite3_blob_write(Blob: TSQLite3Blob; const Data; Count, Offset: Integer): Integer; cdecl; external;
function sqlite3_blob_bytes(Blob: TSQLite3Blob): Integer; cdecl; external;
function sqlite3_create_module_v2(DB: TSQLite3DB; const zName: PAnsiChar;
  var p: TSQLite3Module; pClientData: Pointer; xDestroy: TSQLDestroyPtr): Integer; cdecl; external;
function sqlite3_declare_vtab(DB: TSQLite3DB; const zSQL: PAnsiChar): Integer; cdecl; external;
function sqlite3_set_authorizer(DB: TSQLite3DB; xAuth: TSQLAuthorizerCallback;
  pUserData: Pointer): Integer;   cdecl; external;
function sqlite3_update_hook(DB: TSQLite3DB; xCallback: TSQLUpdateCallback;
  pArg: pointer): pointer; cdecl; external;
function sqlite3_commit_hook(DB: TSQLite3DB; xCallback: TSQLCommitCallback;
  pArg: Pointer): Pointer; cdecl; external;
function sqlite3_rollback_hook(DB: TSQLite3DB;  xCallback: TSQLCommitCallback;
  pArg: Pointer): Pointer; cdecl; external;
function sqlite3_changes(DB: TSQLite3DB): Integer; cdecl; external;
function sqlite3_total_changes(DB: TSQLite3DB): Integer; cdecl; external;
function sqlite3_malloc(n: Integer): Pointer; cdecl; external;
function sqlite3_realloc(pOld: Pointer; n: Integer): Pointer; cdecl; external;
procedure sqlite3_free(p: Pointer); cdecl; external;
function sqlite3_memory_used: Int64; cdecl; external;
function sqlite3_memory_highwater(resetFlag: Integer): Int64; cdecl; external;
function sqlite3_limit(DB: TSQLite3DB; id,newValue: integer): integer; cdecl; external;
function sqlite3_backup_init(DestDB: TSQLite3DB; DestDatabaseName: PUTF8Char;
  SourceDB: TSQLite3DB; SourceDatabaseName: PUTF8Char): TSQLite3Backup; cdecl; external;
function sqlite3_backup_step(Backup: TSQLite3Backup; nPages: integer): integer; cdecl; external;
function sqlite3_backup_finish(Backup: TSQLite3Backup): integer; cdecl; external;
function sqlite3_backup_remaining(Backup: TSQLite3Backup): integer; cdecl; external;
function sqlite3_backup_pagecount(Backup: TSQLite3Backup): integer; cdecl; external;
function sqlite3_serialize(DB: TSQLite3DB; Schema: PUTF8Char; Size: PInt64;
  Flags: integer): pointer; cdecl; external;
function sqlite3_deserialize(DB: TSQLite3DB; Schema: PUTF8Char; Data: pointer;
  DBSize, BufSize: Int64; Flags: integer): pointer; cdecl; external;
{$ifndef DELPHI5OROLDER}
function sqlite3_config(operation: integer): integer; cdecl varargs; external;
function sqlite3_db_config(DB: TSQLite3DB; operation: integer): integer; cdecl varargs; external;
{$endif}
function sqlite3_trace_v2(DB: TSQLite3DB; Mask: integer; Callback: TSQLTraceCallback;
  UserData: Pointer): Pointer; cdecl; external;


{ TSQLite3LibraryStatic }

const
  // error message if statically linked sqlite3.o(bj) does not match this
  // - Android may be a little behind, so we don't check exact version
  EXPECTED_SQLITE3_VERSION = {$ifdef ANDROID}''{$else}'3.34.1'{$endif};

constructor TSQLite3LibraryStatic.Create;
var error: RawUTF8;
begin
  initialize             := @sqlite3_initialize;
  shutdown               := @sqlite3_shutdown;
  open                   := @sqlite3_open;
  open_v2                := @sqlite3_open_v2;
  key                    := @sqlite3_key;
  rekey                  := @sqlite3_rekey;
  close                  := @sqlite3_close;
  libversion             := @sqlite3_libversion;
  errmsg                 := @sqlite3_errmsg;
  extended_errcode       := @sqlite3_extended_errcode;
  create_function        := @sqlite3_create_function;
  create_function_v2     := @sqlite3_create_function_v2;
  create_window_function := @sqlite3_create_window_function;
  create_collation       := @sqlite3_create_collation;
  last_insert_rowid      := @sqlite3_last_insert_rowid;
  busy_timeout           := @sqlite3_busy_timeout;
  busy_handler           := @sqlite3_busy_handler;
  prepare_v2             := @sqlite3_prepare_v2;
  finalize               := @sqlite3_finalize;
  next_stmt              := @sqlite3_next_stmt;
  reset                  := @sqlite3_reset;
  stmt_readonly          := @sqlite3_stmt_readonly;
  step                   := @sqlite3_step;
  column_count           := @sqlite3_column_count;
  column_type            := @sqlite3_column_type;
  column_decltype        := @sqlite3_column_decltype;
  column_name            := @sqlite3_column_name;
  column_bytes           := @sqlite3_column_bytes;
  column_value           := @sqlite3_column_value;
  column_double          := @sqlite3_column_double;
  column_int             := @sqlite3_column_int;
  column_int64           := @sqlite3_column_int64;
  column_text            := @sqlite3_column_text;
  column_text16          := @sqlite3_column_text16;
  column_blob            := @sqlite3_column_blob;
  value_type             := @sqlite3_value_type;
  value_numeric_type     := @sqlite3_value_numeric_type;
  value_bytes            := @sqlite3_value_bytes;
  value_double           := @sqlite3_value_double;
  value_int64            := @sqlite3_value_int64;
  value_text             := @sqlite3_value_text;
  value_blob             := @sqlite3_value_blob;
  result_null            := @sqlite3_result_null;
  result_int64           := @sqlite3_result_int64;
  result_double          := @sqlite3_result_double;
  result_blob            := @sqlite3_result_blob;
  result_text            := @sqlite3_result_text;
  result_value           := @sqlite3_result_value;
  result_error           := @sqlite3_result_error;
  user_data              := @sqlite3_user_data;
  context_db_handle      := @sqlite3_context_db_handle;
  aggregate_context      := @sqlite3_aggregate_context;
  bind_text              := @sqlite3_bind_text;
  bind_blob              := @sqlite3_bind_blob;
  bind_zeroblob          := @sqlite3_bind_zeroblob;
  bind_double            := @sqlite3_bind_double;
  bind_int               := @sqlite3_bind_int;
  bind_int64             := @sqlite3_bind_int64;
  bind_null              := @sqlite3_bind_null;
  clear_bindings         := @sqlite3_clear_bindings;
  bind_parameter_count   := @sqlite3_bind_parameter_count;
  blob_open              := @sqlite3_blob_open;
  blob_reopen            := @sqlite3_blob_reopen;
  blob_close             := @sqlite3_blob_close;
  blob_read              := @sqlite3_blob_read;
  blob_write             := @sqlite3_blob_write;
  blob_bytes             := @sqlite3_blob_bytes;
  create_module_v2       := @sqlite3_create_module_v2;
  declare_vtab           := @sqlite3_declare_vtab;
  set_authorizer         := @sqlite3_set_authorizer;
  update_hook            := @sqlite3_update_hook;
  commit_hook            := @sqlite3_commit_hook;
  rollback_hook          := @sqlite3_rollback_hook;
  changes                := @sqlite3_changes;
  total_changes          := @sqlite3_total_changes;
  malloc                 := @sqlite3_malloc;
  realloc                := @sqlite3_realloc;
  free_                  := @sqlite3_free;
  memory_used            := @sqlite3_memory_used;
  memory_highwater       := @sqlite3_memory_highwater;
  trace_v2               := @sqlite3_trace_v2;
  limit                  := @sqlite3_limit;
  backup_init            := @sqlite3_backup_init;
  backup_step            := @sqlite3_backup_step;
  backup_finish          := @sqlite3_backup_finish;
  backup_remaining       := @sqlite3_backup_remaining;
  backup_pagecount       := @sqlite3_backup_pagecount;
  serialize              := @sqlite3_serialize;
  deserialize            := @sqlite3_deserialize;
  {$ifndef DELPHI5OROLDER} // varargs calls
  config                 := @sqlite3_config;
  db_config              := @sqlite3_db_config;
  {$endif}

  // our static SQLite3 is compiled with SQLITE_OMIT_AUTOINIT defined
  {$ifdef FPC}
  ForceToUseSharedMemoryManager; // before sqlite3_initialize otherwise SQLITE_MISUSE
  {$else}
  {$ifdef CPUX86}
  fUseInternalMM := true; // Delphi .obj are using FastMM4
  {$else}
  ForceToUseSharedMemoryManager; // Delphi .o
  {$endif}
  {$endif}
  sqlite3_initialize;
  inherited Create; // set fVersionNumber/fVersionText
  if (EXPECTED_SQLITE3_VERSION='') or (fVersionText=EXPECTED_SQLITE3_VERSION) then
    exit;
  // you should never see it if you cloned https://github.com/synopse/mORMot
  FormatUTF8('Static SQLite3 library as included within % is outdated!'#13+
    'Linked version is % whereas the current/expected is '+EXPECTED_SQLITE3_VERSION+'.'#13#13+
    'Please download supported latest SQLite3 '+EXPECTED_SQLITE3_VERSION+' revision'#13+
    'from https://synopse.info/files/sqlite3'+{$ifdef FPC}'fpc'{$else}'obj'{$endif}+'.7z',
    [ExeVersion.ProgramName,fVersionText],error);
  LogToTextFile(error); // annoyning enough on all platforms
  // SynSQLite3Log.Add.Log() would do nothing: we are in .exe initialization
  {$ifdef MSWINDOWS} // PITA popup
  // better than a MessageBox() especially for services
  raise Exception.CreateFmt('Deprecated SQLite3 engine: %s',
    [error, ExeVersion.ProgramName]);
  {$endif MSWINDOWS}
end;

destructor TSQLite3LibraryStatic.Destroy;
begin
  if Assigned(shutdown) then
    shutdown;
  inherited;
end;

initialization
  FreeAndNil(sqlite3);
  sqlite3 := TSQLite3LibraryStatic.Create;
{$endif NOSQLITE3STATIC}

end.


