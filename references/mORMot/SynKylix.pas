/// wrapper for Windows functions translated to Linux for Kylix
unit SynKylix;

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

  The Initial Developer of the Original Code is Arnaud Bouchez

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. if you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. if you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  LibC,
  Types,
  Classes,
  SysUtils;

type
  PSystemTime = ^TSystemTime;
  /// System Time, as represented in FPC for POSIX systems
  // - Windows.TSystemTime do have a "w" prefix, e.g. wYear vs Year below
  TSystemTime = record
    Year: Word;
    Month: Word;
    DayOfWeek: Word;
    Day: Word;
    Hour: Word;
    Minute: Word;
    Second: Word;
    Millisecond: Word;
  end;

  cint = integer;
  cuchar = byte;
  cushort = word;

const
  NOERROR = 0;
  NO_ERROR = 0;
  INVALID_HANDLE_VALUE = THandle(-1);
  INFINITE = LongWord($FFFFFFFF);

  LOCALE_USER_DEFAULT = $400;
  NORM_IGNORECASE = 1;

  FILE_BEGIN = SEEK_SET;
  FILE_CURRENT = SEEK_CUR;
  FILE_END = SEEK_END;

  CLOCK_REALTIME = 0;
  CLOCK_MONOTONIC = 1;
  CLOCK_REALTIME_COARSE = 5;
  CLOCK_MONOTONIC_COARSE = 6; // see http://lwn.net/Articles/347811

var
  // contains CLOCK_REALTIME_COARSE since kernel 2.6.32
  CLOCK_REALTIME_TICKCOUNT: integer = CLOCK_REALTIME;
  // contains CLOCK_MONOTONIC_COARSE since kernel 2.6.32
  CLOCK_MONOTONIC_TICKCOUNT: integer = CLOCK_MONOTONIC;

/// compatibility function, wrapping Win32 API high resolution timer
// - this version will return the CLOCK_MONOTONIC value, with a 1 ns resolution
procedure QueryPerformanceCounter(var Value: Int64);

/// slightly faster than QueryPerformanceCounter() div 1000 - but not for Windows
procedure QueryPerformanceMicroSeconds(out Value: Int64);

/// compatibility function, wrapping Win32 API high resolution timer
function QueryPerformanceFrequency(var Value: Int64): boolean;

/// compatibility function, wrapping Win32 API file position change
function SetFilePointer(hFile: THandle; lDistanceToMove: integer;
  lpDistanceToMoveHigh: Pointer; dwMoveMethod: integer): dword;

/// compatibility function, wrapping Win32 API file truncate at current position
procedure SetEndOfFile(hFile: integer);

/// compatibility function, wrapping Win32 API file flush to disk
procedure FlushFileBuffers(hFile: THandle);

/// compatibility function, wrapping Win32 API text comparison
function CompareStringW(GetThreadLocale: DWORD; dwCmpFlags: DWORD; lpString1: Pwidechar;
  cchCount1: longint; lpString2: Pwidechar; cchCount2: longint): longint;

/// a wrapper around stat() to retrieve a file size
function GetFileSize(hFile: THandle; lpFileSizeHigh: PDWORD): DWORD;

/// returns the current UTC time
function GetNowUTC: TDateTime;

/// returns the current UTC time, as Unix Epoch seconds
function GetUnixUTC: Int64;

/// returns the current UTC time, as Unix Epoch milliseconds
// - will call clock_gettime(CLOCK_REALTIME_COARSE) if available
function GetUnixMSUTC: Int64;

/// returns the current UTC time as TSystemTime
procedure GetNowUTCSystem(var result: TSystemTime);

/// compatibility function, to be implemented according to the running OS
// - expect more or less the same result as the homonymous Win32 API function
// - will use CLOCK_MONOTONIC_COARSE, available since Linux 2.6.32, which
// is very fast, and has a 1 ms resolution
function GetTickCount64: Int64;

/// compatibility function, to be implemented according to the running OS
// - expect more or less the same result as the homonymous Win32 API function
function GetTickCount: cardinal;

/// compatibility function, to be implemented according to the running OS
procedure GetLocalTime(var result: TSystemTime);

/// compatibility function, available in SysUtils.pas only for Windows
function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;

/// wrapper around libiconv buffer conversion
// - will write any invalid chars as '?'
// - returns the final Dest position
function IconvBufConvert(context: iconv_t;
  Src: pointer; SrcBytes, SrcCharSize: Integer;
  Dest: pointer; DestBytes, DestCharSize: integer): pointer;

/// similar to Windows sleep() API call, to be truly cross-platform
// - it should have a millisecond resolution, and handle ms=0 as a switch to
// another pending thread, i.e. call sched_yield() API
procedure SleepHiRes(ms: cardinal);

/// similar to FPC's unix.GetHostName function
function GetHostName: string;

var
  /// will contain the current Linux kernel revision, as one integer
  // - e.g. $030d02 for 3.13.2, or $020620 for 2.6.32
  KernelRevision: cardinal;


implementation

const
  C_THOUSAND = Int64(1000);
  C_MILLION  = Int64(C_THOUSAND * C_THOUSAND);
  C_BILLION  = Int64(C_THOUSAND * C_THOUSAND * C_THOUSAND);

procedure QueryPerformanceCounter(var Value: Int64);
var r: TTimeSpec;
begin
  clock_gettime(CLOCK_MONOTONIC,r);
  value := r.tv_nsec+r.tv_sec*C_BILLION; // nanosecond resolution
end;

procedure QueryPerformanceMicroSeconds(out Value: Int64);
var r : TTimeSpec;
begin
  clock_gettime(CLOCK_MONOTONIC,r);
  value := r.tv_nsec div 1000+r.tv_sec*C_MILLION;
end;

function QueryPerformanceFrequency(var Value: Int64): boolean;
begin
  Value := C_BILLION; // 1 second = 1e9 nanoseconds
  result := true;
end;

function SetFilePointer(hFile: THandle; lDistanceToMove: integer;
  lpDistanceToMoveHigh: Pointer; dwMoveMethod: integer): dword;
var offs: Int64;
begin
  Int64Rec(Offs).Lo := lDistanceToMove;
  if lpDistanceToMoveHigh=nil then
    Int64Rec(Offs).Hi := 0 else
    Int64Rec(Offs).Hi := PDWord(lpDistanceToMoveHigh)^;
  offs := lseek64(hFile,offs,dwMoveMethod);
  result := Int64Rec(offs).Lo;
  if lpDistanceToMoveHigh<>nil then
    PDWord(lpDistanceToMoveHigh)^ := Int64Rec(offs).Hi;
end;

procedure SetEndOfFile(hFile: integer);
begin
  ftruncate64(hFile,lseek64(hFile,0,SEEK_CUR));
end;

procedure FlushFileBuffers(hFile: THandle);
begin
  fsync(hFile);
end;

function CompareStringW(GetThreadLocale: DWORD; dwCmpFlags: DWORD; lpString1: Pwidechar;
  cchCount1: longint; lpString2: Pwidechar; cchCount2: longint): longint;
var W1,W2: WideString;
begin
  W1 := lpString1;
  W2 := lpString2;
  if dwCmpFlags and NORM_IGNORECASE<>0 then
    result := WideCompareText(W1,W2) else
    result := WideCompareStr(W1,W2);
end;

function GetFileSize(hFile: THandle; lpFileSizeHigh: PDWORD): DWORD;
var current,size: Int64;
begin // fstat64() returns error EBADF depending on open flags -> use lseek64
  current := lseek64(hFile,0,SEEK_CUR);
  size := lseek64(hFile,0,SEEK_END);
  lseek64(hFile,current,SEEK_SET);
  result := Int64Rec(size).Lo;
  if lpFileSizeHigh<>nil then
    lpFileSizeHigh^ := Int64Rec(size).Hi;
end;

const
  HoursPerDay = 24;
  MinsPerHour = 60;
  SecsPerMin  = 60;
  SecsPerHour = MinsPerHour*SecsPerMin;
  SecsPerDay  = HoursPerDay*SecsPerHour;
  C1970       = 2440588;
  D0          = 1461;
  D1          = 146097;
  D2          = 1721119;

procedure JulianDaysNumberToGregorian(JulianDN: integer; out Year,Month,Day: Word);
var YYear,XYear,Temp,TempMonth: integer;
begin // see http://en.wikipedia.org/wiki/Julian_day
  Temp := ((JulianDN-D2) shl 2)-1;
  JulianDN := Temp div D1;
  XYear := (Temp mod D1) or 3;
  YYear := (XYear div D0);
  Temp := ((((XYear mod D0)+4) shr 2)*5)-3;
  Day := ((Temp mod 153)+5) div 5;
  TempMonth := Temp div 153;
  if TempMonth>=10 then begin
    inc(YYear);
    dec(TempMonth,12);
  end;
  inc(TempMonth,3);
  Month := TempMonth;
  Year := YYear+(JulianDN*100);
end;

procedure TimeValToSystemTime(const tz: TTimeVal; var result: TSystemTime);
var days, secs: integer;
begin
  days := tz.tv_sec div SecsPerDay;
  JulianDaysNumberToGregorian(days+c1970,result.Year,result.Month,result.Day);
  secs := abs(tz.tv_sec mod SecsPerDay);
  result.hour := secs div SecsPerHour;
  secs := secs mod SecsPerHour;
  result.minute := secs div SecsPerMin;
  result.second := secs mod SecsPerMin;
  result.MilliSecond := tz.tv_usec div 1000;
end;

function GetNowUTC: TDateTime;
var SystemTime: TSystemTime;
begin
  GetNowUTCSystem(SystemTime);
  result := SystemTimeToDateTime(SystemTime);
end;

function GetUnixUTC: Int64;
var r: TTimeSpec;
begin
  clock_gettime(CLOCK_REALTIME_TICKCOUNT,r); // faster than gettimeofday
  result := r.tv_sec;
end;

function GetUnixMSUTC: Int64;
var r: TTimeSpec;
begin
  clock_gettime(CLOCK_REALTIME_TICKCOUNT,r);
  result := Int64(r.tv_sec)*C_THOUSAND+(cardinal(r.tv_nsec) div 1000000); // in ms
end;

procedure GetNowUTCSystem(var result: TSystemTime);
var tz: TTimeVal;
begin
  gettimeofday(tz,nil);
  TimeValToSystemTime(tz,result);
end;

procedure GetLocalTime(var result: TSystemTime);
var tv: TTimeVal;
    ut: TUnixTime;
    t: TTime_T;
begin
  gettimeofday(tv,nil);
  t := tv.tv_sec;
  localtime_r(@t,ut);
  result.Year := ut.tm_year+1900;
  result.Month := ut.tm_mon+1;
  result.Day := ut.tm_mday;
  result.Hour := ut.tm_hour;
  result.Minute := ut.tm_min;
  result.Second := ut.tm_sec;
  result.MilliSecond := tv.tv_usec div 1000;
end;

function GetTickCount64: Int64;
var r: TTimeSpec;
begin
  clock_gettime(CLOCK_MONOTONIC_TICKCOUNT,r);
  result := Int64(r.tv_sec)*C_THOUSAND+(cardinal(r.tv_nsec) div 1000000); // in ms
end;

function GetTickCount: cardinal;
begin
  result := cardinal(GetTickCount64);
end;

function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;
var time: TDateTime;
begin
  with SystemTime do begin
    result := EncodeDate(Year,Month,Day);
    time := EncodeTime(Hour,Minute,Second,MilliSecond);
    if result>=0 then
      result := result+time else
      result := result-time;
  end;
end;

function IconvBufConvert(context: iconv_t;
  Src: pointer; SrcBytes, SrcCharSize: Integer;
  Dest: pointer; DestBytes, DestCharSize: integer): pointer;
var SrcBytesLeft, DestBytesLeft, Zero: size_t;
    LastError: Integer;
    pNil: pointer;
begin
  DestBytesLeft := DestBytes;
  SrcBytesLeft := SrcBytes;
  repeat
    if LibC.iconv(context,PChar(Src),SrcBytesLeft,Dest,DestBytesLeft)<>size_t(-1) then
      break; // success
    LastError := GetLastError;
    if (LastError=E2BIG) and (SrcBytesLeft>0) and (DestBytesLeft>0) then
      continue;
    if (LastError<>EINVAL) and (LastError<>EILSEQ) then
      raise Exception.CreateFmt('SynKylix: iconv() fatal error %d',[LastError]);
    Zero := 0;
    pNil := nil;
    LibC.iconv(context,PChar(pNil),Zero,pNil,Zero); // reset
    inc(PByte(Src),SrcCharSize);
    dec(SrcBytesLeft,SrcCharSize);
    PWord(Dest)^ := ord('?');
    inc(PByte(Dest),DestCharSize);
    dec(DestBytesLeft,DestCharSize);
  until false;
  result := Dest;
end;

procedure SleepHiRes(ms: cardinal);
begin
  if ms=0 then
    usleep(1) else // sched_yield() is buggy on multi CPU
    usleep(ms shl 10); // from ms to us
end;

function GetHostName: string;
var tmp: array[byte] of AnsiChar;
begin
  LibC.gethostname(tmp,sizeof(tmp)-1);
  result := tmp;
end;

procedure GetKernelRevision;
var uts: UtsName;
    P: PByte;
  function GetNext: cardinal;
  var c: cardinal;
  begin
    result := 0;
    repeat
      c := P^-48;
      if c>9 then
        break else
        result := result*10+c;
      inc(P);
    until false;
    if P^=ord('.') then
      inc(P);
  end;
begin
  uname(uts);
  P := @uts.release;
  KernelRevision := GetNext shl 16+GetNext shl 8+GetNext;
  if KernelRevision>=$020620 then begin // expects kernel 2.6.32 or higher
    CLOCK_MONOTONIC_TICKCOUNT := CLOCK_MONOTONIC_COARSE;
    CLOCK_REALTIME_TICKCOUNT := CLOCK_REALTIME_COARSE;
  end;
end;

initialization
  GetKernelRevision;
end.
