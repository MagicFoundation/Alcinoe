/// SQLite3 3.21.0 Database engine - statically linked for Windows/Linux 32 bit
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynSQLite3Static;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2018 Arnaud Bouchez
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

  Portions created by the Initial Developer are Copyright (C) 2018
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



    Statically linked SQLite3 3.21.0 engine
   *****************************************

  To be declared in your project uses clause:  will fill SynSQlite3.sqlite3
  global variable with all statically linked .obj API entries.
  sqlite3 := TSQLite3LibraryStatic.Create; is called at unit initialization.

  Will work on Windows 32-bit or 64-bit (with Delphi or FPC, with expected
  .obj / .o) or Linux 32 bit (with FPC, with the corresponding .o)
  under other platforms, this unit will just do nothing (but compile).

  To compile our patched SQlite3.c version, available in this source folder:
  - Run c.bat to compile the sqlite3*.obj for Win32/Delphi
  - Run c-fpcmingw.bat to compile sqlite3.o for Win32/FPC
  - Run c-fpcgcclin.sh to compile sqlite3.o for Linux32/FPC

  Uses TSQLite3LibraryDynamic to access external library (e.g. sqlite3.dll/.so)

  To retrieve and install the latest sqlite3 debian package on Ubuntu:
  - retrieve latest .deb from https://launchpad.net/ubuntu/...
  - for a 32 bit system, install e.g. as
    sudo dpkg -i libsqlite3-0_3.8.7.4-1_i386.deb
  - for a 64 bit system, you need to download and install both packages, e.g.
    sudo dpkg -i libsqlite3-0_3.8.2-1ubuntu2_amd64.deb libsqlite3-0_3.8.2-1ubuntu2_i386.deb

  Version 1.18
  - initial revision, extracted from SynSQLite3.pas unit
  - updated SQLite3 engine to latest version 3.21.0
  - now all sqlite3_*() API calls are accessible via sqlite3.*()
  - our custom file encryption is now called via sqlite3.key() - i.e. official
    SQLite Encryption Extension (SEE) sqlite3_key() API
  - Memory-Mapped I/O support - see http://www.sqlite.org/mmap.html
  - under Win64, expects an external sqlite3-64.dll file to be available, which
    may be downloaded from https://synopse.info/files/SQLite3-64.7z
  - added sqlite3.backup_*() Online Backup API functions
  - added missing function sqlite3_column_text16() - fixed ticket [25d8d1f47a]
  - added sqlite3.db_config() support
  - enabled FTS5 and RBU support
  - added FPC cross-platform support, statically linked for Win32/Win64

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER SQLITE3_FASTCALL

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
  SynSQLite3;


{$ifdef INCLUDE_FTS3}
  {$define INCLUDE_TRACE}
  { define this is you want to include the TRACE feature into the library
   - our C source code custom header will define SQLITE_OMIT_TRACE if FTS3/FST4
   is not defined }
{$endif}

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


{$ifndef NOSQLITE3ENCRYPT}
/// use this procedure to change the password for an existing SQLite3 database file
// - use this procedure instead of the "classic" sqlite3.rekey() API call
// - conversion is done in-place, therefore this procedure can handle very big files
// - the OldPassWord must be correct, otherwise the resulting file will be corrupted
// - any password can be '' to mark no encryption
// - you may use instead SynCrypto unit for more secure SHA-256 and AES-256 algos
// - please note that this encryption is compatible only with SQlite3 files
// using the default page size of 1024
// - implementation is NOT compatible with the official SQLite Encryption Extension
// (SEE) file format: it provides only simple XOR encryption (no RC4/AES)
// - the first page (first 1024 bytes) is not encrypted, since its content
// (mostly zero) can be used to easily guess the beginning of the key
// - if the key is not correct, a ESQLite3Exception will be raised with
// 'database disk image is malformed' (SQLITE_CORRUPT) at database opening
procedure ChangeSQLEncryptTablePassWord(const FileName: TFileName;
  const OldPassWord, NewPassword: RawUTF8);
{$endif}


implementation

const
  /// encryption XOR mask table size (in bytes)
  // - must be a power of 2
  // - bigger encryption table makes stronger encryption, but use more memory
  // - it's faster when the mask table can stay in the CPU L1 cache
  // - default size is therefore 16KB
  SQLEncryptTableSize = $4000;

(*
  Code below will link all database engine, from amalgamation source file:

 - to compile with free Borland C++ compiler 5.5.1 from the command line:
     \dev\bcc\bin\bcc32 -6 -O2 -c -d -u- sqlite3.c
    FastCall use must be set with defining SQLITE3_FASTCALL above, and
     int __cdecl fts3CompareElemByTerm(const void *lhs, const void *rhs)
     \dev\bcc\bin\bcc32 -6 -O2 -c -d -pr -u- sqlite3.c
 - to compile for FPC using the MinGW compiler: run c-fpcmingw.bat
    gcc -O2 -c -DSQLITE_ENABLE_FTS3 sqlite3.c
    and copy libgcc.a and libkernel32.a from your MinGW folders
 - to compile for FPC using gcc under Linux: run c-fpcgcclin.sh
    gcc -O2 -c -lpthread -ldl -DSQLITE_ENABLE_FTS3 sqlite3.c
    and copy the libgcc.a and sqlite3.o (and libc.a if needed) into the linuxlibrary folder and off you go
    For CentOS 7.0, take a look at https://synopse.info/forum/viewtopic.php?pid=13193#p13193

and, in the sqlite3.c source file, the following functions are made external
in order to allow our proprietary but simple and efficient encryption system:

extern int winRead(
  sqlite3.file *id,          /* File to read from */
  void *pBuf,                /* Write content into this buffer */
  int amt,                   /* Number of bytes to read */
  sqlite3_int64 offset       /* Begin reading at this offset */
);

extern int winWrite(
  sqlite3_file *id,         /* File to write into */
  const void *pBuf,         /* The bytes to be written */
  int amt,                  /* Number of bytes to write */
  sqlite3_int64 offset      /* Offset into the file to begin writing at */
);

Under Linux (thanks Alf for the patch!), change the following lines of sqlite3.c:

extern int unixRead(
  sqlite3_file *id,
  void *pBuf,
  int amt,
  sqlite3_int64 offset
);

extern int unixWrite(
  sqlite3_file *id,
  const void *pBuf,
  int amt,
  sqlite3_int64 offset
);

*)

{$ifdef FPC}  // FPC expects .o linking, and only one version including FTS3

  {$ifdef MSWINDOWS}
    {$ifdef CPU64}
      {$L fpc-win64\sqlite3-64.o}
      {$linklib fpc-win64\libkernel32.a}
      {$linklib fpc-win64\libgcc.a}
      {$linklib fpc-win64\libmsvcrt.a}
      const LOGFUNCLINKNAME = 'log';
    {$else}
      {$L fpc-win32\sqlite3.o}
      {$linklib fpc-win32\libkernel32.a}
      {$linklib fpc-win32\libgcc.a}
      {$linklib fpc-win32\libmsvcrt.a}
      const LOGFUNCLINKNAME = '_log';
    {$endif CPU64}
  {$else}
    {$ifdef Darwin}
      {$ifdef CPU64}
        {$linklib .\..\fpc-darwin64\libsqlite3.a}
      {$else}
        {$linklib .\..\fpc-darwin32\libsqlite3.a}
      {$endif}
      const LOGFUNCLINKNAME = 'log';
    {$else Darwin}
      {$ifndef FPC_CROSSCOMPILING}
        {$linklib gcc.a}
      {$endif}
      {$ifdef CPUARM}
        {$L fpc-linuxarm\sqlite3.o}
        {$ifdef FPC_CROSSCOMPILING}
          {$linklib fpc-linuxarm\gcc.a}
          {$L libgcc_s.so.1}
        {$else}
          {$linklib gcc_s.so.1}
        {$endif}
        const LOGFUNCLINKNAME = 'log';
      {$endif}
      {$ifdef CPUINTEL}
        {$ifdef CPU64}
          {$L fpc-linux64\sqlite3-64.o}
          {$ifdef FPC_CROSSCOMPILING}
            {$linklib fpc-linux64\gcc.a}
          {$endif}
          const LOGFUNCLINKNAME = 'log';
        {$else}
          {$L fpc-linux32\sqlite3.o}
          {$ifdef FPC_CROSSCOMPILING}
            {$linklib fpc-linux32\gcc.a}
          {$endif}
          const LOGFUNCLINKNAME = 'log';
        {$endif CPU64}
      {$endif CPUINTEL}
    {$endif Darwin}
  {$endif MSWINDOWS}

function log(x: double): double; cdecl; public name LOGFUNCLINKNAME; export;
begin
  result := ln(x);
end;

{$ifdef Darwin}
function moddi3(num,den:int64):int64; cdecl; [public, alias: '___moddi3'];
begin
 result := num mod den;
end;
function umoddi3(num,den:uint64):uint64; cdecl; [public, alias: '___umoddi3'];
begin
 result := num mod den;
end;
function divdi3(num,den:int64):int64; cdecl; [public, alias: '___divdi3'];
begin
 result := num div den;
end;
function udivdi3(num,den:uint64):uint64; cdecl; [public, alias: '___udivdi3'];
begin
 result := num div den;
end;

{$endif}

{$else}

  // Delphi has a more complex linking strategy, since $linklib doesn't exist :(
  {$ifdef MSWINDOWS}
    {$ifdef CPU64}
      {$L fpc-win64\sqlite3-64.o} // compiled with gcc for FPC ... try
    {$else}
      {$ifdef INCLUDE_FTS3}
      {$L sqlite3fts3.obj}   // link SQlite3 with FTS3/FTS4/FTS5 + TRACE
      {$else}
      {$L sqlite3.obj}       // link SQlite3 database engine
      {$endif INCLUDE_FTS3}
    {$endif}
  {$else}
  {$ifdef KYLIX3} // in practice, failed to compile SQLite3 with gcc 2 :(
    {$L kylix/sqlite3/sqlite3.o}
    {$L kylix/sqlite3/_divdi3.o}
    {$L kylix/sqlite3/_moddi3.o}
    {$L kylix/sqlite3/_udivdi3.o}
    {$L kylix/sqlite3/_umoddi3.o}
    {$L kylix/sqlite3/_cmpdi2.o}
  {$endif KYLIX3}
  {$endif MSWINDOWS}

// those functions will be called only under Delphi + Win32

function malloc(size: cardinal): Pointer; cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'malloc'{$else}'_malloc'{$endif};{$endif}
// the SQLite3 database engine will use the FastMM4/SynScaleMM fast heap manager
begin
  GetMem(Result, size);
end;

procedure free(P: Pointer); cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'free'{$else}'_free'{$endif};{$endif}
// the SQLite3 database engine will use the FastMM4 very fast heap manager
begin
  FreeMem(P);
end;

function realloc(P: Pointer; Size: Integer): Pointer; cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'realloc'{$else}'_realloc'{$endif};{$endif}
// the SQLite3 database engine will use the FastMM4/SynScaleMM very fast heap manager
begin
  result := P;
  ReallocMem(result,Size);
end;

function rename(oldname, newname: PUTF8Char): integer; cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'rename'{$else}'_rename'{$endif};{$endif}
// the SQLite3 database engine will use the FastMM4/SynScaleMM fast heap manager
begin
  if RenameFile(UTF8DecodeToString(oldname,StrLen(oldname)),
                UTF8DecodeToString(newname,StrLen(newname))) then
    result := 0 else
    result := -1;
end;

{$ifdef MSWINDOWS}
{$ifdef CPU32}

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

function log(const val: extended): extended;
asm
  fld val
  fldln2
  fxch
  fyl2x
  fwait
end;

{$endif CPU32}
{$endif MSWINDOWS}

function memset(P: Pointer; B: Integer; count: Integer): pointer; cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'memset'{$else}'_memset'{$endif};{$endif}
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

function memcmp(p1, p2: pByte; Size: integer): integer; cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'memcmp'{$else}'_memcmp'{$endif};{$endif}
// a fast full pascal version of the standard C library function
begin
  if (p1<>p2) and (Size<>0) then
    if p1<>nil then
      if p2<>nil then begin
        repeat
          if p1^<>p2^ then begin
            result := p1^-p2^;
            exit;
          end;
          dec(Size);
          inc(p1);
          inc(p2);
        until Size=0;
        result := 0;
      end else
      result := 1 else
    result := -1 else
  result := 0;
end;

function strncmp(p1, p2: PByte; Size: integer): integer; cdecl; { always cdecl }
  {$ifdef FPC}public name{$ifdef CPU64}'strncmp'{$else}'_strncmp'{$endif};{$endif}
// a fast full pascal version of the standard C library function
var i: integer;
begin
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

procedure QuickSort4(base: PPointerArray; L, R: Integer; comparF: qsort_compare_func);
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
      QuickSort4(base, L, J, comparF);
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
      QuickSort4(baseP, 0, NElem-1, qsort_compare_func(comparF)) else
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

// first try for static on Win64 with Delphi
function __imp__beginthreadex(security: pointer; stksize: dword;
  start,arg: pointer; flags: dword; var threadid: dword): THandle; cdecl; external msvcrt name '_beginthreadex';
procedure __imp__endthreadex(exitcode: dword); cdecl; external msvcrt name '_endthreadex';

function __imp_TryEnterCriticalSection(lpCriticalSection:pointer): BOOL; cdecl; external kernel name 'TryEnterCriticalSection';
procedure __imp_LeaveCriticalSection(lpCriticalSection:pointer); cdecl; external kernel name 'LeaveCriticalSection';
procedure __imp_EnterCriticalSection(lpCriticalSection:pointer); cdecl; external kernel name 'EnterCriticalSection';
procedure __imp_DeleteCriticalSection(lpCriticalSection:pointer); cdecl; external kernel name 'DeleteCriticalSection';
procedure __imp_InitializeCriticalSection(lpCriticalSection:pointer); cdecl; external kernel name 'InitializeCriticalSection';
function __imp_GetCurrentThreadId:dword; cdecl; external kernel name 'GetCurrentThreadId';
function __imp_CloseHandle(hObject:THandle): BOOL; cdecl; external kernel name 'CloseHandle';
function __imp__localtime64(t: PCardinal): pointer; cdecl;
begin
  result := localtime64(t^);
end;
function log(x: double): double; cdecl; export;
begin
  result := ln(x);
end;
// try ends here

procedure __chkstk;
begin
end;

procedure __faststorefence;
asm
  .noframe
  mfence;
end;

var
  _fltused: byte; // not used, but needed for linking

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


{$ifndef NOSQLITE3ENCRYPT}

procedure ChangeSQLEncryptTablePassWord(const FileName: TFileName;
  const OldPassWord, NewPassword: RawUTF8);
var F: THandle;
    R: integer;
    Buf: array[word] of byte; // temp buffer for read/write (64KB is enough)
    Size, Posi: Int64Rec;
    OldP, NewP: array[0..SQLEncryptTableSize-1] of byte; // 2x16KB tables
begin
  if OldPassword=NewPassword then
    exit;
  F := FileOpen(FileName,fmOpenReadWrite);
  if F=INVALID_HANDLE_VALUE then
    exit;
  Size.Lo := GetFileSize(F,@Size.Hi);
  if (Size.Lo<=1024) and (Size.Hi=0) then begin
    FileClose(F); // file is to small to be modified
    exit;
  end;
  if OldPassword<>'' then
    CreateSQLEncryptTableBytes(OldPassWord,@OldP);
  if NewPassword<>'' then
    CreateSQLEncryptTableBytes(NewPassWord,@NewP);
  Int64(Posi) := 1024; // don't change first page, which is uncrypted
  FileSeek(F,1024,soFromBeginning);
  while Int64(Posi)<Int64(Size) do begin
    R := FileRead(F,Buf,sizeof(Buf)); // read buffer
    if R<0 then
      break; // stop on any read error
    if OldPassword<>'' then
      XorOffset(@Buf,Posi.Lo,R,@OldP); // uncrypt with old key
    if NewPassword<>'' then
      XorOffset(@Buf,Posi.Lo,R,@NewP); // crypt with new key
    FileSeek64(F,Int64(Posi),soFromBeginning);
    FileWrite(F,Buf,R); // update buffer
    inc(Int64(Posi),cardinal(R));
  end;
  FileClose(F);
end;

{$endif}

// we override default WinRead() and WinWrite() functions below, in order
// to add our proprietary (but efficient) encryption engine
// - should be modified to match other Operating Systems than Windows
// - code is private to SynSQLite3Static, since it shall follow the same
// exact SQlite3.c revision compiled within the linked .obj

type
{$ifndef DELPHI5OROLDER} // Delphi 5 is already aligning records by 4 bytes
{$ifdef FPC}
{$PACKRECORDS C}
{$else}
{$A4} // bcc32 default alignment is 4 bytes
{$endif}
{$endif}
  {$ifdef MSWINDOWS}
  TSQLFile = record // see struct winFile in sqlite3.c
    pMethods: pointer;     // sqlite3.io_methods_ptr
    pVfs: pointer;         // The VFS used to open this file (new in version 3.7)
    h: THandle;            // Handle for accessing the file
    locktype: byte;        // Type of lock currently held on this file */
    sharedLockByte: word;  // Randomly chosen byte used as a shared lock */
    ctrlFlags: byte;       // Flags.  See WINFILE_* below */
    lastErrno: cardinal;   // The Windows errno from the last I/O error
    // asm code generated from c is [esi+20] for lastErrNo -> OK
    pShm: pointer; // not there if SQLITE_OMIT_WAL is defined
    zPath: PAnsiChar;
    szChunk, nFetchOut: integer;
    hMap: THandle;
    pMapRegion: PAnsiChar;
    mmapSize, mmapSizeActual, mmapSizeMax: Int64Rec;
  end;
  {$else}
  TSQLFile = record            // see struct unixFile in sqlite3.c
    pMethods: pointer;         // sqlite3.io_methods_ptr
    pVfs: pointer;             // VFS used to open this file (new in version 3.7)
    pINode: pointer;           // Info about locks on this inode
    h: THandle;                // Handle for accessing the file
    eFileLock: cuchar;         // The type of lock held on this fd
    ctrlFlags: cushort;        // Behavioral bits.  UNIXFILE_* flags
    lastErrno: cint;           // The unix errno from the last I/O error
    lockingContext: PAnsiChar; // Locking style specific state
    UnixUnusedFd: pointer;     // unused
    zPath: PAnsiChar;          // Name of the file
    pShm: pointer; // not there if SQLITE_OMIT_WAL is defined
    szChunk: cint;
    nFetchOut: cint;
    mmapSize, mmapSizeActual, mmapSizeMax: Int64Rec;
    pMapRegion: PAnsiChar;
  end;
  {$endif}
  PSQLFile = ^TSQLFile;

  // those structures are used to retrieve the Windows/Linux file handle
  TSQLPager = record            // see struct Pager in sqlite3.c
    pVfs: pointer;
    exclusiveMode, journalMode, useJournal, noSync, fullSync,
    extraSync, // new in 3.12.0
    syncFlags, // modified in 3.21.0
    walsyncFlags, tempFile, noLock, readOnly, memDb,
    eState, eLock, changeCountDone, setMaster, doNotSpill, subjInMemory,
    bUseFetch, hasHeldSharedLock: Byte;
    dbSize, dbOrigSize, dbFileSize, dbHintSize, errCode, nRec, cksumInit,
    nSubRec: cardinal;
    pInJournal: pointer;
    fd: PSQLFile;   // File descriptor for database
    jfd: PSQLFile;  // File descriptor for main journal
    sjfd: PSQLFile; // File descriptor for sub-journal
  end;
  TSQLBtShared = record
    pPager: ^TSQLPager;
  end;
  TSQLBTree = record
    db: TSQLite3DB;
    pBt: ^TSQLBtShared;
  end;
  PSQLBTree = ^TSQLBTree;
  TSQLDBOneStruct = record
    zName: PAnsiChar;
    Btree: PSQLBTree;
  end;
  // will map TSQLite3DB
  PSQLDBStruct = ^TSQLDBStruct;
  TSQLDBStruct = record
    pVfs, pVdbe, pDfltColl, mutex: pointer;
    DB0: ^TSQLDBOneStruct;
    nDb: integer;  // Number of backends currently in use
  end;
{$A+}
  // used to store all currently per-database encryption tables
  TSQLCypher = record
    Handle: THandle;
    CypherBuf: RawByteString;
  end;
  TSQLCypherDynArray = array of TSQLCypher;

var
  Cyphers: TSQLCypherDynArray;
  CypherCount: integer;
  Cypher: TDynArray;

function sqlite3_key(DB: TSQLite3DB; key: pointer; keyLen: Integer): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
var Cyph: TSQLCypher;
    pass, buf: RawByteString;
begin
  result := SQLITE_OK;
  if (DB=0) or (key=nil) or (keyLen<=0) then
    exit;
  SetString(pass,PAnsiChar(key),keyLen);
  SetLength(buf,SQLEncryptTableSize);
  CreateSQLEncryptTableBytes(pass,pointer(buf));
  Cyph.Handle := PSQLDBStruct(DB)^.DB0^.Btree^.pBt^.pPager^.fd^.h;
  if Cyphers=nil then
    Cypher.InitSpecific(TypeInfo(TSQLCypherDynArray),Cyphers,djCardinal,@CypherCount);
  if Cypher.Find(Cyph.Handle)>=0 then
    raise ESQLite3Exception.Create('Invalid call to sqlite3_key() with no previous sqlite3_close()');
  Cyph.CypherBuf := buf;
  Cypher.Add(Cyph);
end;

function sqlite3_rekey(DB: TSQLite3DB; key: pointer; keyLen: Integer): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
begin
  raise ESQLite3Exception.Create('sqlite3_rekey() not implemented yet');
end;

function sqlite3_close(DB: TSQLite3DB): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;

function sqlite3_closeInternal(DB: TSQLite3DB): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
var i: integer;
begin
  if Cyphers<>nil then
    i := Cypher.Find(PSQLDBStruct(DB)^.DB0^.Btree^.pBt^.pPager^.fd^.h) else
    i := -1;
  result := sqlite3_close(DB);
  if i>=0 then
    Cypher.Delete(i); // do it after file closing
end;

const
  SQLITE_IOERR_READ       = $010A;
  SQLITE_IOERR_SHORT_READ = $020A;
  SQLITE_IOERR_WRITE      = $030A;

{$ifdef MSWINDOWS}
function WinWrite(FP: pointer; buf: PByte; buflen: cardinal; off: Int64): integer;
{$else}
function unixWrite(FP: pointer; buf: PByte; buflen: cint; off: Int64): integer;
{$endif}
  {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
  {$ifdef FPC}
    public name
    {$ifdef MSWINDOWS}
      {$ifdef CPU64}
      'winWrite';
      {$else}
      '_winWrite';
      {$endif}
    {$else}
    {$ifdef Darwin}
    '_unixWrite'; export;
    {$else}
    'unixWrite'; export;
    {$endif}
    {$endif}
  {$endif}
// Write data from a buffer into a file.  Return SQLITE_OK on success
// or some other error code on failure
var n, i, written: integer;
    EncryptTable: PByteArray;
    off64: Int64Rec absolute off;
    F: PSQLFile absolute FP;
    nCopy: cardinal;
    h: THandle;
    b: PByte;
    {$ifdef MSWINDOWS}
    ol: TOverlapped;
    ol64: Int64;
    {$endif}
begin
  if off<Int64(F.mmapSize) then // handle memory mapping (SQLite3>=3.7.17)
    if CypherCount=0 then
      if off+buflen<=Int64(F.mmapSize) then begin
        MoveFast(buf^,F.pMapRegion[off64.Lo],bufLen);
        result := SQLITE_OK;
        exit;
      end else begin
        nCopy := F.mmapSize.Lo-off64.Lo;
        MoveFast(buf^,F.pMapRegion[off64.Lo],nCopy);
        inc(buf,nCopy);
        dec(buflen,nCopy);
        inc(off,nCopy);
      end else
      raise ESynException.CreateUTF8(
        'sqlite3_key(%) expects PRAGMA mmap_size=0 write(off=% mmapSize=% buflen=%)',
        [F.zPath,off,Int64(F.mmapSize),bufLen]);
  //SynSQLite3Log.Add.Log(sllCustom2,'WinWrite % off=% len=%',[F.h,off,buflen]);
  off64.Hi := off64.Hi and $7fffffff; // offset must be positive (u64)
  {$ifdef MSWINDOWS}
  FillCharFast(ol,sizeof(ol),0);
  ol64 := off;
  {$else}
  if FileSeek64(F.h,off,soFromBeginning)=-1 then begin
    result := GetLastError;
    if result<>NO_ERROR then begin
      F.lastErrno := result;
      result := SQLITE_FULL;
      exit;
    end;
  end;
  {$endif}
  EncryptTable := nil; // mark no encryption
  if CypherCount>0 then
    if (off64.Lo>=1024) or (off64.Hi<>0) then begin // crypt after first page
      h := F.h;
      for i := 0 to CypherCount-1 do // (a bit) faster than Cypher.Find(F.h)
        if Cyphers[i].Handle=h then begin
          EncryptTable := Pointer(Cyphers[i].CypherBuf);
          XorOffset(buf,off64.Lo,buflen,EncryptTable);
          break;
        end;
    end;
  b := buf;
  n := buflen;
  while n>0 do begin
    {$ifdef MSWINDOWS}
    ol.Offset := Int64Rec(ol64).Lo;
    ol.OffsetHigh := Int64Rec(ol64).Hi;
    if WriteFile(F.h,b^,n,cardinal(written),@ol) then
      inc(ol64,written) else
      written := -1;
    {$else}
    written := FileWrite(F.h,b^,n);
    {$endif}
    if written=0 then
      break;
    if written=-1 then begin
      F.lastErrno := GetLastError;
      {$ifdef MSWINDOWS}
      if not (F.lastErrno in [ERROR_HANDLE_DISK_FULL,ERROR_DISK_FULL]) then
        result := SQLITE_IOERR_WRITE else
      {$endif}
        result := SQLITE_FULL;
      if EncryptTable<>nil then // restore buf content
        XorOffset(buf,off64.Lo,buflen,EncryptTable);
      exit;
    end;
    dec(n,written);
    inc(b,written);
  end;
  result := SQLITE_OK;
  if EncryptTable<>nil then // restore buf content
    XorOffset(buf,off64.Lo,buflen,EncryptTable);
end;


{$ifdef MSWINDOWS}
function WinRead(FP: pointer; buf: PByte; buflen: Cardinal; off: Int64): integer;
{$else}
function unixRead(FP: pointer; buf: PByte; buflen: cint; off: Int64): integer;
{$endif}
  {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
  {$ifdef FPC}
    public name
    {$ifdef MSWINDOWS}
      {$ifdef CPU64}
      'winRead';
      {$else}
      '_winRead';
      {$endif}
    {$else}
    {$ifdef Darwin}
    '_unixRead'; export;
    {$else}
    'unixRead'; export;
    {$endif}
    {$endif}
  {$endif}
// Read data from a file into a buffer.  Return SQLITE_OK on success
// or some other error code on failure
var off64: Int64Rec absolute off;
    F: PSQLFile absolute FP;
    nCopy: cardinal;
    b: PByte;
    h: THandle;
    i,n,read: integer;
    {$ifdef MSWINDOWS}
    ol: TOverlapped;
    {$endif}
begin
  if off<Int64(F.mmapSize) then // handle memory mapping (SQLite3>=3.7.17)
    if CypherCount=0 then
      if off+buflen<=Int64(F.mmapSize) then begin
        MoveFast(F.pMapRegion[off64.Lo],buf^,bufLen);
        result := SQLITE_OK;
        exit;
      end else begin
        nCopy := F.mmapSize.Lo-off64.Lo;
        MoveFast(F.pMapRegion[off64.Lo],buf^,nCopy);
        inc(buf,nCopy);
        dec(buflen,nCopy);
        inc(off,nCopy);
      end else
      raise ESynException.CreateUTF8(
        'sqlite3_key(%) expects PRAGMA mmap_size=0 read(off=% mmapSize=% buflen=%)',
        [F.zPath,off,Int64(F.mmapSize),bufLen]);
  //SynSQLite3Log.Add.Log(sllCustom2,'WinRead % off=% len=%',[F.h,off,buflen]);
  {$ifdef MSWINDOWS} // read chunk in one single API call
  FillCharFast(ol,sizeof(ol),0);
  ol.Offset := off64.Lo;
  ol.OffsetHigh := off64.Hi and $7fffffff;
  b := buf;
  n := buflen;
  if not ReadFile(F.h,b^,n,cardinal(read),@ol) then begin
    i := GetLastError;
    if i<>ERROR_HANDLE_EOF then begin
      F.lastErrno := i;
      result := SQLITE_IOERR_READ;
      exit;
    end;
  end;
  inc(b,read);
  dec(n,read);
  {$else} // use standard cross-platform FPC/Delphi RTL calls
  off64.Hi := off64.Hi and $7fffffff; // offset must be positive (u64)
  if FileSeek64(F.h,off,soFromBeginning)=-1 then begin
    result := GetLastError;
    if result<>NO_ERROR then begin
      F.lastErrno := result;
      result := SQLITE_FULL;
      exit;
    end;
  end;
  b := buf;
  n := buflen;
  repeat
    read := FileRead(F.h,b^,n);
    if read=0 then
      break;
    if read=-1 then begin
      F.lastErrno := GetLastError;
      result := SQLITE_IOERR_READ;
      exit;
    end;
    inc(b,read);
    dec(n,read);
  until n=0;
  {$endif}
  if CypherCount>0 then
    if (off64.Lo>=1024) or (off64.Hi<>0) then begin // uncrypt after first page
      h := F.h;
      for i := 0 to CypherCount-1 do // (a bit) faster than Cypher.Find(F.h)
        if Cyphers[i].Handle=h then begin
          XorOffset(buf,off64.Lo,buflen,pointer(Cyphers[i].CypherBuf));
          break;
        end;
    end;
  if n>0 then begin // remaining bytes are set to 0
    FillcharFast(b^,n,0);
    result := SQLITE_IOERR_SHORT_READ;
  end else
    result := SQLITE_OK;
end;


function sqlite3_initialize: integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_shutdown: integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_open(filename: PUTF8Char; var DB: TSQLite3DB): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_open_v2(filename: PUTF8Char; var DB: TSQLite3DB; flags: Integer; vfs: PUTF8Char): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_create_function(DB: TSQLite3DB; FunctionName: PUTF8Char;
  nArg, eTextRep: integer; pApp: pointer; xFunc, xStep: TSQLFunctionFunc;
  xFinal: TSQLFunctionFinal): Integer;
  {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_create_function_v2(DB: TSQLite3DB; FunctionName: PUTF8Char;
  nArg, eTextRep: integer; pApp: pointer; xFunc, xStep: TSQLFunctionFunc;
  xFinal: TSQLFunctionFinal; xDestroy: TSQLDestroyPtr): Integer;
  {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_create_collation(DB: TSQLite3DB; CollationName: PUTF8Char;
  StringEncoding: integer; CollateParam: pointer; cmp: TSQLCollateFunc): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_libversion: PUTF8Char; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_errmsg(DB: TSQLite3DB): PAnsiChar; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_extended_errcode(DB: TSQLite3DB): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_last_insert_rowid(DB: TSQLite3DB): Int64; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_busy_timeout(DB: TSQLite3DB; Milliseconds: integer): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_busy_handler(DB: TSQLite3DB;
  CallbackPtr: TSQLBusyHandler; user: Pointer): integer;  {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_prepare_v2(DB: TSQLite3DB; SQL: PUTF8Char; SQL_bytes: integer;
  var S: TSQLite3Statement; var SQLtail: PUTF8Char): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_finalize(S: TSQLite3Statement): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_next_stmt(DB: TSQLite3DB; S: TSQLite3Statement): TSQLite3Statement; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_reset(S: TSQLite3Statement): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_stmt_readonly(S: TSQLite3Statement): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_step(S: TSQLite3Statement): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_column_count(S: TSQLite3Statement): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_column_type(S: TSQLite3Statement; Col: integer): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_column_decltype(S: TSQLite3Statement; Col: integer): PAnsiChar; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_column_name(S: TSQLite3Statement; Col: integer): PUTF8Char; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_column_bytes(S: TSQLite3Statement; Col: integer): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_column_value(S: TSQLite3Statement; Col: integer): TSQLite3Value; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_column_double(S: TSQLite3Statement; Col: integer): double; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_column_int(S: TSQLite3Statement; Col: integer): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_column_int64(S: TSQLite3Statement; Col: integer): int64; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_column_text(S: TSQLite3Statement; Col: integer): PUTF8Char; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_column_text16(S: TSQLite3Statement; Col: integer): PWideChar; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_column_blob(S: TSQLite3Statement; Col: integer): PAnsiChar; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_value_type(Value: TSQLite3Value): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_value_numeric_type(Value: TSQLite3Value): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_value_bytes(Value: TSQLite3Value): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_value_double(Value: TSQLite3Value): double; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_value_int64(Value: TSQLite3Value): Int64; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_value_text(Value: TSQLite3Value): PUTF8Char; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_value_blob(Value: TSQLite3Value): pointer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
procedure sqlite3_result_null(Context: TSQLite3FunctionContext); {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
procedure sqlite3_result_int64(Context: TSQLite3FunctionContext; Value: Int64); {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
procedure sqlite3_result_double(Context: TSQLite3FunctionContext; Value: double); {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
procedure sqlite3_result_blob(Context: TSQLite3FunctionContext; Value: Pointer;
  Value_bytes: Integer=0; DestroyPtr: TSQLDestroyPtr=SQLITE_TRANSIENT); {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
procedure sqlite3_result_text(Context: TSQLite3FunctionContext; Value: PUTF8Char;
  Value_bytes: Integer=-1; DestroyPtr: TSQLDestroyPtr=SQLITE_TRANSIENT); {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
procedure sqlite3_result_value(Context: TSQLite3FunctionContext; Value: TSQLite3Value); {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
procedure sqlite3_result_error(Context: TSQLite3FunctionContext; Msg: PUTF8Char; MsgLen: integer=-1); {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_user_data(Context: TSQLite3FunctionContext): pointer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_context_db_handle(Context: TSQLite3FunctionContext): TSQLite3DB; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_aggregate_context(Context: TSQLite3FunctionContext;
   nBytes: integer): pointer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_bind_text(S: TSQLite3Statement; Param: integer;
  Text: PUTF8Char; Text_bytes: integer=-1; DestroyPtr: TSQLDestroyPtr=SQLITE_TRANSIENT): integer;
  {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_bind_blob(S: TSQLite3Statement; Param: integer; Buf: pointer; Buf_bytes: integer;
  DestroyPtr: TSQLDestroyPtr=SQLITE_TRANSIENT): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_bind_zeroblob(S: TSQLite3Statement; Param: integer; Size: integer): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_bind_double(S: TSQLite3Statement; Param: integer; Value: double): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_bind_int(S: TSQLite3Statement; Param: integer; Value: integer): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_bind_int64(S: TSQLite3Statement; Param: integer; Value: Int64): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_bind_null(S: TSQLite3Statement; Param: integer): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_clear_bindings(S: TSQLite3Statement): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_bind_parameter_count(S: TSQLite3Statement): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_blob_open(DB: TSQLite3DB; DBName, TableName, ColumnName: PUTF8Char;
  RowID: Int64; Flags: Integer; var Blob: TSQLite3Blob): Integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_blob_close(Blob: TSQLite3Blob): Integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_blob_read(Blob: TSQLite3Blob; const Data; Count, Offset: Integer): Integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_blob_write(Blob: TSQLite3Blob; const Data; Count, Offset: Integer): Integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_blob_bytes(Blob: TSQLite3Blob): Integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_create_module_v2(DB: TSQLite3DB; const zName: PAnsiChar;
  var p: TSQLite3Module; pClientData: Pointer; xDestroy: TSQLDestroyPtr): Integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_declare_vtab(DB: TSQLite3DB; const zSQL: PAnsiChar): Integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_set_authorizer(DB: TSQLite3DB; xAuth: TSQLAuthorizerCallback;
  pUserData: Pointer): Integer;   {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_update_hook(DB: TSQLite3DB; xCallback: TSQLUpdateCallback;
  pArg: pointer): pointer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_commit_hook(DB: TSQLite3DB; xCallback: TSQLCommitCallback;
  pArg: Pointer): Pointer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_rollback_hook(DB: TSQLite3DB;  xCallback: TSQLCommitCallback;
  pArg: Pointer): Pointer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_changes(DB: TSQLite3DB): Integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_total_changes(DB: TSQLite3DB): Integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_malloc(n: Integer): Pointer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_realloc(pOld: Pointer; n: Integer): Pointer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
procedure sqlite3_free(p: Pointer); {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_memory_used: Int64; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_memory_highwater(resetFlag: Integer): Int64; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_limit(DB: TSQLite3DB; id,newValue: integer): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_backup_init(DestDB: TSQLite3DB; DestDatabaseName: PUTF8Char;
  SourceDB: TSQLite3DB; SourceDatabaseName: PUTF8Char): TSQLite3Backup; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_backup_step(Backup: TSQLite3Backup; nPages: integer): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_backup_finish(Backup: TSQLite3Backup): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_backup_remaining(Backup: TSQLite3Backup): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
function sqlite3_backup_pagecount(Backup: TSQLite3Backup): integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
{$ifndef DELPHI5OROLDER}
function sqlite3_config(operation: integer): integer; cdecl varargs; external;
function sqlite3_db_config(DB: TSQLite3DB; operation: integer): integer; cdecl varargs; external;
{$endif}
{$ifdef INCLUDE_TRACE}
function sqlite3_trace_v2(DB: TSQLite3DB; Mask: integer; Callback: TSQLTraceCallback;
  UserData: Pointer): Pointer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif} external;
{$endif INCLUDE_TRACE}


{ TSQLite3LibraryStatic }

const
  // error message if linked sqlite3.obj does not match this
  EXPECTED_SQLITE3_VERSION = '3.21.0';
  
constructor TSQLite3LibraryStatic.Create;
var error: RawUTF8;
begin
  initialize           := @sqlite3_initialize;
  shutdown             := @sqlite3_shutdown;
  open                 := @sqlite3_open;
  open_v2              := @sqlite3_open_v2;
  key                  := @sqlite3_key;
  rekey                := @sqlite3_rekey;
  close                := @sqlite3_closeInternal;
  libversion           := @sqlite3_libversion;
  errmsg               := @sqlite3_errmsg;
  extended_errcode     := @sqlite3_extended_errcode;
  create_function      := @sqlite3_create_function;
  create_function_v2   := @sqlite3_create_function_v2;
  create_collation     := @sqlite3_create_collation;
  last_insert_rowid    := @sqlite3_last_insert_rowid;
  busy_timeout         := @sqlite3_busy_timeout;
  busy_handler         := @sqlite3_busy_handler;
  prepare_v2           := @sqlite3_prepare_v2;
  finalize             := @sqlite3_finalize;
  next_stmt            := @sqlite3_next_stmt;
  reset                := @sqlite3_reset;
  stmt_readonly        := @sqlite3_stmt_readonly;
  step                 := @sqlite3_step;
  column_count         := @sqlite3_column_count;
  column_type          := @sqlite3_column_type;
  column_decltype      := @sqlite3_column_decltype;
  column_name          := @sqlite3_column_name;
  column_bytes         := @sqlite3_column_bytes;
  column_value         := @sqlite3_column_value;
  column_double        := @sqlite3_column_double;
  column_int           := @sqlite3_column_int;
  column_int64         := @sqlite3_column_int64;
  column_text          := @sqlite3_column_text;
  column_text16        := @sqlite3_column_text16;
  column_blob          := @sqlite3_column_blob;
  value_type           := @sqlite3_value_type;
  value_numeric_type   := @sqlite3_value_numeric_type;
  value_bytes          := @sqlite3_value_bytes;
  value_double         := @sqlite3_value_double;
  value_int64          := @sqlite3_value_int64;
  value_text           := @sqlite3_value_text;
  value_blob           := @sqlite3_value_blob;
  result_null          := @sqlite3_result_null;
  result_int64         := @sqlite3_result_int64;
  result_double        := @sqlite3_result_double;
  result_blob          := @sqlite3_result_blob;
  result_text          := @sqlite3_result_text;
  result_value         := @sqlite3_result_value;
  result_error         := @sqlite3_result_error;
  user_data            := @sqlite3_user_data;
  context_db_handle    := @sqlite3_context_db_handle;
  aggregate_context    := @sqlite3_aggregate_context;
  bind_text            := @sqlite3_bind_text;
  bind_blob            := @sqlite3_bind_blob;
  bind_zeroblob        := @sqlite3_bind_zeroblob;
  bind_double          := @sqlite3_bind_double;
  bind_int             := @sqlite3_bind_int;
  bind_int64           := @sqlite3_bind_int64;
  bind_null            := @sqlite3_bind_null;
  clear_bindings       := @sqlite3_clear_bindings;
  bind_parameter_count := @sqlite3_bind_parameter_count;
  blob_open            := @sqlite3_blob_open;
  blob_close           := @sqlite3_blob_close;
  blob_read            := @sqlite3_blob_read;
  blob_write           := @sqlite3_blob_write;
  blob_bytes           := @sqlite3_blob_bytes;
  create_module_v2     := @sqlite3_create_module_v2;
  declare_vtab         := @sqlite3_declare_vtab;
  set_authorizer       := @sqlite3_set_authorizer;
  update_hook          := @sqlite3_update_hook;
  commit_hook          := @sqlite3_commit_hook;
  rollback_hook        := @sqlite3_rollback_hook;
  changes              := @sqlite3_changes;
  total_changes        := @sqlite3_total_changes;
  malloc               := @sqlite3_malloc;
  realloc              := @sqlite3_realloc;
  free_                := @sqlite3_free;
  memory_used          := @sqlite3_memory_used;
  memory_highwater     := @sqlite3_memory_highwater;
{$ifdef INCLUDE_TRACE}
  trace_v2             := @sqlite3_trace_v2;
{$endif}
  limit                := @sqlite3_limit;
  backup_init          := @sqlite3_backup_init;
  backup_step          := @sqlite3_backup_step;
  backup_finish        := @sqlite3_backup_finish;
  backup_remaining     := @sqlite3_backup_remaining;
  backup_pagecount     := @sqlite3_backup_pagecount;
  {$ifndef DELPHI5OROLDER}
  config               := @sqlite3_config;
  db_config            := @sqlite3_db_config;
  {$endif}

  // sqlite3.obj is compiled with SQLITE_OMIT_AUTOINIT defined
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
  if fVersionText=EXPECTED_SQLITE3_VERSION then
    exit;
  FormatUTF8('Static sqlite3.obj as included within % is outdated!'#13+
    'Linked version is % whereas the current/expected is '+EXPECTED_SQLITE3_VERSION+'.'#13#13+
    'Please download latest SQLite3 '+EXPECTED_SQLITE3_VERSION+' revision'#13+
    'from https://synopse.info/files/sqlite3obj.7z',
    [ExeVersion.ProgramName,fVersionText],error);
  LogToTextFile(error); // annoyning enough on all platforms
  // SynSQLite3Log.Add.Log() would do nothing: we are in .exe initialization
  {$ifdef MSWINDOWS} // PITA popup 
  MessageBoxA(0,pointer(error),' WARNING: deprecated SQLite3 engine',MB_OK or MB_ICONWARNING);
  {$endif}
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


