{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsXPlatform;

{$I dws.inc}

//
// This unit should concentrate all non-UI cross-platform aspects,
// cross-Delphi versions, ifdefs and other conditionals
//
// no ifdefs in the main code.

{$WARN SYMBOL_PLATFORM OFF}

{$IFDEF FPC}
   {$DEFINE VER200}  // FPC compatibility = D2009
{$ENDIF}
{$IFDEF MSWINDOWS}
   {$DEFINE WINDOWS}  // Define Delphi <==> FPC "WINDOWS" Compiler Switch
{$ENDIF}
{$IFDEF ANDROID}
   {$DEFINE UNIX}
{$ENDIF}
{$IFDEF LINUX}
   {$DEFINE UNIX}  // Define Delphi <==> FPC "UNIX" Compiler Switch
{$ENDIF}

{$ifdef UNIX}
   {$ifndef ANDROID}
      {$DEFINE POSIXSYSLOG} // If defined Posix Syslog is used in Unix environments
   {$endif}
{$endif}

interface

uses
   System.Classes, System.SysUtils, System.Types
   {$ifdef DELPHI_XE3_PLUS}
   , System.DateUtils
   {$endif}
   {$IFDEF FPC}
      {$IFDEF WINDOWS}
         , Windows
      {$ELSE}
         , LCLIntf
      {$ENDIF}
   {$ELSE}
      , System.IOUtils
      {$IFDEF WINDOWS}
      , Winapi.Windows, System.Win.Registry
      {$ENDIF}
      {$IFDEF UNIX}
         {$IFDEF POSIXSYSLOG}, Posix.Syslog{$ENDIF}
         , System.Internal.ICU, System.SyncObjs
         , Posix.Unistd, Posix.Time, Posix.Pthread, Posix.Base, Posix.Stdlib, Posix.Stdio
         , dwsXPlatformTimer
      {$ENDIF}
   {$ENDIF}
   ;

const
   {$IFDEF UNIX}
   cLineTerminator  = #10;
   {$ELSE}
   cLineTerminator  = #13#10;
   {$ENDIF}

   // following is missing from D2010
   INVALID_HANDLE_VALUE = NativeUInt(-1);

   {$ifdef FPC}
   // FreePascal RTL declares this constant, but does not support it,
   // so it just leads to runtime crashes, this attempts to trigger compile-time crashes instead
   varUString = 'varUString is not supported by FreePascal';
   {$endif}

type

   // see http://delphitools.info/2011/11/30/fixing-tcriticalsection/
   {$HINTS OFF}
   {$ifdef UNIX}
   TdwsCriticalSection = class (TCriticalSection)
      public
         function TryEnterOrTimeout(delayMSec : Integer) : Boolean;
   end;
   {$else}
   TdwsCriticalSection = class
      private
         FDummy : array [0..63-SizeOf(TRTLCRiticalSection)-2*SizeOf(Pointer)] of Byte;
         FCS : TRTLCriticalSection;

      public
         constructor Create;
         destructor Destroy; override;

         procedure Enter;
         procedure Leave;

         function TryEnter : Boolean;
         function TryEnterOrTimeout(delayMSec : Integer) : Boolean;
   end;
   {$endif}

   IMultiReadSingleWrite = interface
      procedure BeginRead;
      function  TryBeginRead : Boolean;
      procedure EndRead;

      procedure BeginWrite;
      function  TryBeginWrite : Boolean;
      procedure EndWrite;
   end;

   TMultiReadSingleWriteState = (mrswUnlocked, mrswReadLock, mrswWriteLock);

   {$ifdef UNIX}{$define SRW_FALLBACK}{$endif}

   TMultiReadSingleWrite = class (TInterfacedObject, IMultiReadSingleWrite)
      private
         {$ifndef SRW_FALLBACK}
         FSRWLock : Pointer;
         FDummy : array [0..63-4*SizeOf(Pointer)] of Byte; // padding
         {$else}
         FLock : TLightweightMREW;
         {$endif}

      public
         procedure BeginRead; inline;
         function  TryBeginRead : Boolean; inline;
         procedure EndRead; inline;

         procedure BeginWrite; inline;
         function  TryBeginWrite : Boolean; inline;
         procedure EndWrite; inline;

         // use for diagnostic only
         function State : TMultiReadSingleWriteState;
   end;

   {$HINTS ON}

procedure SetDecimalSeparator(c : Char);
function GetDecimalSeparator : Char;

type
   TCollectFileProgressEvent = procedure (const directory : TFileName; var skipScan : Boolean) of object;

procedure CollectFiles(const directory, fileMask : TFileName;
                       list : TStrings; recurseSubdirectories: Boolean = False;
                       onProgress : TCollectFileProgressEvent = nil);
procedure CollectSubDirs(const directory : TFileName; list : TStrings);

type
   {$IFNDEF FPC}
   {$IF CompilerVersion<22.0}
   // NativeUInt broken in D2009, and PNativeInt is missing in D2010
   // http://qc.embarcadero.com/wc/qcmain.aspx?d=71292
   NativeInt = Integer;
   PNativeInt = ^NativeInt;
   NativeUInt = Cardinal;
   PNativeUInt = ^NativeUInt;
   {$IFEND}
   {$ENDIF}

   {$IFDEF FPC}
   TBytes = array of Byte;

   RawByteString = String;

   PNativeInt = ^NativeInt;
   PUInt64 = ^UInt64;
   {$ENDIF}

   PInt32 = PInteger;

   TdwsLargeInteger = record
      case Integer of
      0: (
         LowPart: DWORD;
         HighPart: DWORD;
      );
      1: (
         QuadPart: Int64
      );
   end;

   TPath = class
      class function GetTempPath : String; static;
      class function GetTempFileName : String; static;
   end;

   TFile = class
      class function ReadAllBytes(const filename : String) : TBytes; static; inline;
   end;

   TdwsThread = class (TThread)
      {$IFNDEF FPC}
      {$IFDEF VER200}
      procedure Start;
      {$ENDIF}
      {$ENDIF}

      procedure SetTimeCriticalPriority;
   end;

   // Wrap in a record so it is not assignment compatible without explicit casts
   // Internal representation is UnixTime in milliseconds (same as JavaScript)
   TdwsDateTime = record
      private
         FValue : Int64;

         function GetAsUnixTime : Int64;
         procedure SetAsUnixTime(const val : Int64);

         function GetAsFileTime : TFileTime;
         procedure SetAsFileTime(const val : TFileTime);
         function GetAsDosDateTime : Integer;

         function GetAsLocalDateTime : TDateTime;
         procedure SetAsLocalDateTime(const val : TDateTime);
         function GetAsUTCDateTime : TDateTime;
         procedure SetAsUTCDateTime(const val : TDateTime);

      public
         class function Now : TdwsDateTime; static;
         class function FromLocalDateTime(const dt : TDateTime) : TdwsDateTime; static;
         class function FromUTCDateTime(const dt : TDateTime) : TdwsDateTime; static;

         procedure Clear; inline;
         function IsZero : Boolean; inline;
         function IsNotZero : Boolean; inline;

         class operator Equal(const a, b : TdwsDateTime) : Boolean; static; inline;
         class operator NotEqual(const a, b : TdwsDateTime) : Boolean; static; inline;
         class operator GreaterThan(const a, b : TdwsDateTime) : Boolean; static; inline;
         class operator GreaterThanOrEqual(const a, b : TdwsDateTime) : Boolean; static; inline;
         class operator LessThan(const a, b : TdwsDateTime) : Boolean; static; inline;
         class operator LessThanOrEqual(const a, b : TdwsDateTime) : Boolean; static; inline;

         function MillisecondsAheadOf(const d : TdwsDateTime) : Int64; inline;
         procedure IncMilliseconds(const msec : Int64); inline;

         property Value : Int64 read FValue write FValue;

         property AsUnixTime : Int64 read GetAsUnixTime write SetAsUnixTime;
         property AsJavaScriptTime : Int64 read FValue write FValue;
         property AsFileTime : TFileTime read GetAsFileTime write SetAsFileTime;
         property AsDosDateTime : Integer read GetAsDosDateTime;

         property AsLocalDateTime : TDateTime read GetAsLocalDateTime write SetAsLocalDateTime;
         property AsUTCDateTime : TDateTime read GetAsUTCDateTime write SetAsUTCDateTime;
   end;

// 64bit system clock reference in milliseconds since boot
function GetSystemMilliseconds : Int64;
function UTCDateTime : TDateTime;
function UnixTime : Int64;
function EpochTimeStamp : Int64;

function LocalDateTimeToUTCDateTime(t : TDateTime) : TDateTime;
function UTCDateTimeToLocalDateTime(t : TDateTime) : TDateTime;

function SystemMillisecondsToUnixTime(t : Int64) : Int64;
function UnixTimeToSystemMilliseconds(ut : Int64) : Int64;

procedure SystemSleep(msec : Integer);

function FirstWideCharOfString(const s : String; const default : WideChar = #0) : WideChar; inline;
procedure CodePointToUnicodeString(c : Integer; var result : UnicodeString);
procedure CodePointToString(const c : Integer; var result : String); inline;

{$ifndef FPC}
function UnicodeCompareStr(const S1, S2 : String) : Integer; inline;
function UnicodeStringReplace(const s, oldPattern, newPattern: String; flags: TReplaceFlags) : String; inline;
{$endif}

function UnicodeCompareP(p1 : PWideChar; n1 : Integer; p2 : PWideChar; n2 : Integer) : Integer; overload;
function UnicodeCompareP(p1, p2 : PWideChar; n : Integer) : Integer; overload;
function UnicodeCompareEx(const a, b : UnicodeString; const locale : UnicodeString; caseSensitive : Boolean) : Integer;

procedure UnicodeLowerCase(const s : UnicodeString; var result : UnicodeString); overload;
function  UnicodeLowerCase(const s : UnicodeString) : UnicodeString; overload; inline; deprecated 'use procedure form';

procedure UnicodeUpperCase(const s : UnicodeString; var result : UnicodeString); overload;
function  UnicodeUpperCase(const s : UnicodeString) : UnicodeString; overload; inline; deprecated 'use procedure form';

{$ifdef FPC}
function UnicodeLowerCase(const s : String) : String; overload;
function UnicodeUpperCase(const s : String) : String; overload;
{$endif}

function ASCIICompareText(const s1, s2 : String) : Integer; inline;
function ASCIISameText(const s1, s2 : String) : Boolean; inline;

function NormalizeString(const s, form : String) : String;
function StripAccents(const s : String) : String;

function InterlockedIncrement(var val : Integer) : Integer; overload; {$IFDEF PUREPASCAL} inline; {$endif} deprecated 'use AtomicIncrement';
function InterlockedDecrement(var val : Integer) : Integer; {$IFDEF PUREPASCAL} inline; {$endif} deprecated 'use AtomicDecrement';

procedure FastInterlockedIncrement(var val : Integer); {$IFDEF PUREPASCAL} inline; {$endif}
procedure FastInterlockedDecrement(var val : Integer); {$IFDEF PUREPASCAL} inline; {$endif}

function InterlockedExchangePointer(var target : Pointer; val : Pointer) : Pointer; {$IFDEF PUREPASCAL} inline; {$endif}

function InterlockedCompareExchangePointer(var destination : Pointer; exchange, comparand : Pointer) : Pointer; {$IFDEF PUREPASCAL} inline; {$endif}

procedure SetThreadName(const threadName : PAnsiChar; threadID : Cardinal = Cardinal(-1));

procedure OutputDebugString(const msg : String);

procedure WriteToOSEventLog(const logName, logCaption, logDetails : String;
                            const logRawData : RawByteString = ''); overload;

{$ifdef FPC}
procedure VarCopy(out dest : Variant; const src : Variant); inline;
{$else}
function VarToUnicodeStr(const v : Variant) : String; inline;
{$endif}

{$ifdef FPC}
function Utf8ToUnicodeString(const buf : RawByteString) : UnicodeString; inline;
{$endif}

function RawByteStringToBytes(const buf : RawByteString) : TBytes;
function BytesToRawByteString(const buf : TBytes; startIndex : Integer = 0) : RawByteString; overload;
function BytesToRawByteString(p : Pointer; size : Integer) : RawByteString; overload;

function PosExA(const needle, haystack : RawByteString; hayStackOffset : Integer) : Integer;

procedure BytesToScriptString(const p : PByteArray; n : Integer; var result : UnicodeString);

procedure WordsToBytes(src : PWordArray; dest : PByteArray; nbWords : Integer);
procedure BytesToWords(src : PByteArray; dest : PWordArray; nbBytes : Integer);
procedure BytesToWordsInPlace(p : Pointer; n : NativeInt);

type
   TOpenFileOption = ( ofoNoRaiseError );
   TOpenFileOptions = set of TOpenFileOption;

function LoadDataFromFile(const fileName : TFileName; const options : TOpenFileOptions = [ ]) : TBytes;
procedure SaveDataToFile(const fileName : TFileName; const data : TBytes);

function LoadRawBytesFromFile(const fileName : TFileName) : RawByteString;
function SaveRawBytesToFile(const fileName : TFileName; const data : RawByteString) : Integer;

procedure LoadRawBytesAsScriptStringFromFile(const fileName : TFileName; var result : String);

function LoadTextFromBuffer(const buf : TBytes) : UnicodeString;
function LoadTextFromRawBytes(const buf : RawByteString) : UnicodeString;
function LoadTextFromStream(aStream : TStream) : UnicodeString;
function LoadTextFromFile(const fileName : TFileName; const options : TOpenFileOptions = [ ]) : UnicodeString;
procedure SaveTextToUTF8File(const fileName : TFileName; const text : String);
procedure AppendTextToUTF8File(const fileName : TFileName; const text : UTF8String);

function OpenFileForSequentialReadOnly(
   const fileName : TFileName;
   const options : TOpenFileOptions = [ ]
   ) : THandle;
function OpenFileForSequentialWriteOnly(
   const fileName : TFileName;
   const options : TOpenFileOptions = [ ]
   ) : THandle;

function CloseFileHandle(hFile : THandle) : Boolean;
function FileClose(hFile : THandle) : Boolean; inline;
function FileWrite(hFile : THandle; buffer : Pointer; byteCount : Int64) : Int64;
function FileRead(hFile : THandle; buffer : Pointer; byteCount : Int64) : Int64;
function FileFlushBuffers(hFile : THandle) : Boolean;
function SetEndOfFile(hFile : THandle) : Boolean;
function FileCopy(const existing, new : TFileName; failIfExists : Boolean) : Boolean;
function FileMove(const existing, new : TFileName) : Boolean;
function FileDelete(const fileName : TFileName) : Boolean;
function FileRename(const oldName, newName : TFileName) : Boolean;
function FileSize(const name : TFileName) : Int64;
function FileCreationTime(const name : TFileName) : TdwsDateTime; overload;
function FileDateTime(const name : TFileName; lastAccess : Boolean = False) : TdwsDateTime; overload;
function FileDateTime(hFile : THandle; lastAccess : Boolean = False) : TdwsDateTime; overload;
procedure FileSetDateTime(hFile : THandle; const aDateTime : TdwsDateTime); overload;
procedure FileSetDateTime(const name : TFileName; const aDateTime : TdwsDateTime); overload;

function DeleteDirectory(const path : String; recursive : Boolean = True) : Boolean;

type
   TdwsMemoryMappedFile = record
      FileHandle : THandle;
      MapHandle : THandle;
      Base : Pointer;
      Size : Int64;

      function DetectEncoding(var encoding : TEncoding) : Integer;
   end;

function FileMemoryMapReadOnly(const fileName : TFileName) : TdwsMemoryMappedFile;
procedure FileUnMap(var map : TdwsMemoryMappedFile);

function DirectSet8087CW(newValue : Word) : Word; register;
function DirectSetMXCSR(newValue : Word) : Word; register;

function SwapBytes(v : Cardinal) : Cardinal;
procedure SwapBytesBlock(src, dest : PByte; nb : Integer);
procedure SwapBytesInt16(src, dest : PWORD);
procedure SwapBytesInt32(src, dest : PInt32);
procedure SwapBytesInt64(src, dest : PInt64);

function RDTSC : UInt64;

function GetCurrentUserName : String;

{$ifndef FPC}
// Generics helper functions to handle Delphi 2009 issues - HV
function TtoObject(const T): TObject; inline;
function TtoPointer(const T): Pointer; inline;
procedure GetMemForT(var T; Size: integer); inline;
{$endif}

procedure InitializeWithDefaultFormatSettings(var fmt : TFormatSettings);

type
   TTimerEvent = procedure of object;

   ITimer = interface
      procedure Cancel;
   end;

   TTimerTimeout = class (TInterfacedObject, ITimer)
      private
         FTimer : THandle;
         FOnTimer : TTimerEvent;

      public
         class function Create(delayMSec : Cardinal; onTimer : TTimerEvent) : ITimer;
         destructor Destroy; override;

         procedure Cancel;
   end;

{$ifndef SRW_FALLBACK}
procedure AcquireSRWLockExclusive(var SRWLock : Pointer); stdcall; external 'kernel32.dll';
function TryAcquireSRWLockExclusive(var SRWLock : Pointer) : ByteBool; stdcall; external 'kernel32.dll';
procedure ReleaseSRWLockExclusive(var SRWLock : Pointer); stdcall; external 'kernel32.dll';

procedure AcquireSRWLockShared(var SRWLock : Pointer); stdcall; external 'kernel32.dll';
function TryAcquireSRWLockShared(var SRWLock : Pointer) : ByteBool; stdcall; external 'kernel32.dll';
procedure ReleaseSRWLockShared(var SRWLock : Pointer); stdcall; external 'kernel32.dll';
{$endif}

type
   TModuleVersion = record
      Major, Minor : Word;
      Release, Build : Word;
      function GetAsString : String;
      procedure SetAsString(const s : String);
      property AsString : String read GetAsString write SetAsString;
   end;

   TApplicationVersionOption = (
      avoBitness     // if set, mention 32bit or 64bit after version number
   );
   TApplicationVersionOptions = set of TApplicationVersionOption;

function GetModuleVersion(instance : THandle; var version : TModuleVersion) : Boolean;
function GetApplicationVersion(var version : TModuleVersion) : Boolean;
function ApplicationVersion(const options : TApplicationVersionOptions = [ avoBitness ]) : String;

type
   TCPUFeature = (
      cpuFeaturesRead,  // internal flag to indicate features were read
      cpuSSE41,         // SSE4.1
      cpuFMA,           // FMA3
      cpuAVX,           // AVX
      cpuAVX2           // AVX2
   );
   TCPUFeatures = set of TCPUFeature;

function WIN64CPUFeatures : TCPUFeatures;
function Win64SSE41Supported : Boolean; inline;
function Win64FMASupported : Boolean; inline;
function Win64AVX2Supported : Boolean; inline;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
   System.Masks, System.Variants, dwsUTF8;

// FileMemoryMapReadOnly
//
function FileMemoryMapReadOnly(const fileName : TFileName) : TdwsMemoryMappedFile;
{$ifdef WINDOWS}
begin
   Result.Size := 0;
   Result.Base := nil;
   Result.MapHandle := 0;
   Result.FileHandle := FileOpen(fileName, fmOpenRead+fmShareDenyNone);
   if Result.FileHandle = INVALID_HANDLE_VALUE then begin
      Result.FileHandle := 0;
      RaiseLastOSError;
   end;
   try
      if not GetFileSizeEx(Result.FileHandle, Result.Size) then
         RaiseLastOSError;
      if Result.Size > 0 then begin
         Result.MapHandle := CreateFileMapping(Result.FileHandle, nil, PAGE_READONLY, 0, 0, nil);
         if Result.MapHandle = 0 then
            RaiseLastOSError;
         try
            Result.Base := MapViewOfFile(Result.MapHandle, FILE_MAP_READ, 0, 0, Result.Size);
            if Result.Base = nil then
               RaiseLastOSError;
         except
            CloseHandle(Result.MapHandle);
            Result.MapHandle := 0;
            raise;
         end;
      end;
   except
      FileClose(Result.FileHandle);
      Result.FileHandle := 0;
      raise;
   end;
end;
{$else}
begin
  { TODO : Check unix implementation }
  raise Exception.Create('not implemented');
end;
{$endif}


// FileUnMap
//
procedure FileUnMap(var map : TdwsMemoryMappedFile);
{$ifdef WINDOWS}
begin
   if map.Base <> nil then begin
      UnmapViewOfFile(map.Base);
      map.Base := nil;
   end;
   if map.MapHandle <> 0 then begin
      CloseHandle(map.MapHandle);
      map.MapHandle := 0;
   end;
   if map.FileHandle <> 0 then begin
      FileClose(map.FileHandle);
      map.FileHandle := 0;
   end;
end;
{$else}
begin
  { TODO : Check unix implementation }
  raise Exception.Create('not implemented');
end;
{$endif}

// DetectEncoding
//
function TdwsMemoryMappedFile.DetectEncoding(var encoding : TEncoding) : Integer;

   function DetectComplexEncoding(var encoding : TEncoding) : Integer;
   var
      buf : TBytes;
      n : Integer;
   begin
      n := Size;
      if n > 64 then
         n := 64;
      SetLength(buf, n);
      System.Move(Base^, buf[0], n);
      Result := TEncoding.GetBufferEncoding(buf, encoding);
   end;

begin
   if (Size <= 0) or (Base = nil) then Exit(0);

   // shortcut for UTF-8 BOM
   if Size >= 3 then begin
      if (PWord(Base)^ = $BBEF) and (Ord(PAnsiChar(Base)[2]) = $BF) then begin
         encoding := TEncoding.UTF8;
         Exit(3);
      end;
   end;
   Result := DetectComplexEncoding(encoding);
end;

{$ifdef FPC}
type
   TFindExInfoLevels = FINDEX_INFO_LEVELS;
{$endif}

// GetSystemTimeMilliseconds
//
function GetSystemTimeMilliseconds : Int64; stdcall;
begin
   Result := TdwsDateTime.Now.Value;
end;

// GetSystemMilliseconds
//
var
   vGetSystemMilliseconds : function : Int64; stdcall;
function GetSystemMilliseconds : Int64;
{$ifdef WIN32_ASM}
asm
   jmp [vGetSystemMilliseconds]
{$else}
begin
   Result := vGetSystemMilliseconds;
{$endif}
end;

// InitializeGetSystemMilliseconds
//
procedure InitializeGetSystemMilliseconds;
{$ifdef WINDOWS}
var
   h : THandle;
begin
   h := LoadLibrary('kernel32.dll');
   vGetSystemMilliseconds := GetProcAddress(h, 'GetTickCount64');
end;
{$else}
begin
   if not Assigned(vGetSystemMilliseconds) then
      vGetSystemMilliseconds:=@GetSystemTimeMilliseconds;
end;
{$endif}

// UTCDateTime
//
function UTCDateTime : TDateTime;
{$ifdef WINDOWS}
var
   systemTime : TSystemTime;
begin
   FillChar(systemTime, SizeOf(systemTime), 0);
   GetSystemTime(systemTime);
   with systemTime do
      Result:= EncodeDate(wYear, wMonth, wDay)
              +EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
end;
{$else}
begin
   Result := TTimeZone.Local.ToUniversalTime(Now);
end;
{$endif}

// UnixTime
//
function UnixTime : Int64;
{$ifdef WINDOWS}
const
   cUNIX_TIME_START : Int64 = $019DB1DED53E8000;
   cTICKS_PER_SECOND : Int64 = 10000000;  // 100ns
var
   ft : FILETIME;
   t : TdwsLargeInteger;
begin
   GetSystemTimeAsFileTime(ft);
   t.LowPart := ft.dwLowDateTime;
   t.HighPart := ft.dwHighDateTime;
   Result := (t.QuadPart - cUNIX_TIME_START) div cTICKS_PER_SECOND;
end;
{$else}
begin
  Result := Trunc(UTCDateTime*86400)-Int64(25569)*86400;
end;
{$endif}

// EpochTimeStamp
//
function EpochTimeStamp : Int64;
{$ifdef WINDOWS}
const
   cUNIX_TIME_START : Int64 = $019DB1DED53E8000;
   cTICKS_PER_SECOND : Int64 = 10000000;  // 100ns
   cEPOCH_TICKS_PER_SECOND : Int64 = 1000;
var
   ft : FILETIME;
   t : TdwsLargeInteger;
begin
   GetSystemTimeAsFileTime(ft);
   t.LowPart := ft.dwLowDateTime;
   t.HighPart := ft.dwHighDateTime;
   Result := (t.QuadPart - cUNIX_TIME_START) div (cTICKS_PER_SECOND div cEPOCH_TICKS_PER_SECOND);
end;
{$else}
begin
  { TODO : Check unix implementation }
  raise Exception.Create('not implemented');
end;
{$endif}

{$IFNDEF UNIX}
type
   TDynamicTimeZoneInformation = record
      Bias : Longint;
      StandardName : array[0..31] of WideChar;
      StandardDate : TSystemTime;
      StandardBias : Longint;
      DaylightName : array[0..31] of WideChar;
      DaylightDate : TSystemTime;
      DaylightBias : Longint;
      TimeZoneKeyName : array[0..127] of WideChar;
      DynamicDaylightTimeDisabled : Boolean;
   end;
   PDynamicTimeZoneInformation = ^TDynamicTimeZoneInformation;

function GetDynamicTimeZoneInformation(
      var pTimeZoneInformation: TDynamicTimeZoneInformation): DWORD; stdcall; external 'kernel32' {$ifndef FPC}delayed{$endif};
function GetTimeZoneInformationForYear(wYear: USHORT; lpDynamicTimeZoneInformation: PDynamicTimeZoneInformation;
      var lpTimeZoneInformation: TTimeZoneInformation): BOOL; stdcall; external 'kernel32' {$ifndef FPC}delayed{$endif};
function TzSpecificLocalTimeToSystemTime(lpTimeZoneInformation: PTimeZoneInformation;
      var lpLocalTime, lpUniversalTime: TSystemTime): BOOL; stdcall; external 'kernel32' {$ifndef FPC}delayed{$endif};
{$ENDIF}

// LocalDateTimeToUTCDateTime
//
type TTimeZoneCracker = class (TTimeZone);
function LocalDateTimeToUTCDateTime(t : TDateTime) : TDateTime;
{$ifdef DELPHI_XE3_PLUS}
var
   utcOffsetInSeconds, dstOffset : Int64;
   timeType: TLocalTimeType;
begin
   // ideally this would be just this line, but DoGetOffsetsAndType has the undesirable behavior
   // of unconditionnally raising an exception in case of a time falling in DST "hole",
   // which is something that can happen in historic data when a clock did not get adjusted
   // we handle it the same way as the ambiguous one here: we ignore it and assume DST off

   // Result := TTimeZone.Local.ToUniversalTime(t, False);

   TTimeZoneCracker(TTimeZone.Local).DoGetOffsetsAndType(t, utcOffsetInSeconds, dstOffset, timeType);
   case timeType of
      lttDaylight, lttAmbiguous :
         utcOffsetInSeconds := utcOffsetInSeconds + dstOffset;
   end;

   if utcOffsetInSeconds <> 0 then
      Result := IncMilliSecond(t, -utcOffsetInSeconds*1000)
   else Result := t;
end;
{$else}{$ifdef FPC}
begin
   Result := LocalTimeToUniversal(t);
end;
{$else}
var
   localSystemTime, universalSystemTime : TSystemTime;
   tzDynInfo : TDynamicTimeZoneInformation;
   tzInfo : TTimeZoneInformation;
   y, m, d : Word;
begin
   DateTimeToSystemTime(t, localSystemTime);
   if GetDynamicTimeZoneInformation(tzDynInfo) = TIME_ZONE_ID_INVALID then
      RaiseLastOSError;
   DecodeDate(t, y, m, d);
   if not GetTimeZoneInformationForYear(y, @tzDynInfo, tzInfo) then
      RaiseLastOSError;
   if not TzSpecificLocalTimeToSystemTime(@tzInfo, localSystemTime, universalSystemTime) then
      RaiseLastOSError;
   Result := SystemTimeToDateTime(universalSystemTime);
end;
{$endif}{$endif}

// UTCDateTimeToLocalDateTime
//
function UTCDateTimeToLocalDateTime(t : TDateTime) : TDateTime;
{$ifdef DELPHI_XE3_PLUS}
begin
   Result := TTimeZone.Local.ToLocalTime(t);
end;
{$else}{$ifdef FPC}
begin
   Result := UniversalTimeToLocal(t);
end;
{$else}
var
   tzDynInfo : TDynamicTimeZoneInformation;
   tzInfo : TTimeZoneInformation;
   localSystemTime, universalSystemTime : TSystemTime;
begin
   DateTimeToSystemTime(t, universalSystemTime);
   if GetDynamicTimeZoneInformation(tzDynInfo) = TIME_ZONE_ID_INVALID then
      RaiseLastOSError;
   if not GetTimeZoneInformationForYear(universalSystemTime.wYear, @tzDynInfo, tzInfo) then
      RaiseLastOSError;
   if not SystemTimeToTzSpecificLocalTime(@tzInfo, universalSystemTime, localSystemTime) then
      RaiseLastOSError;
   Result := SystemTimeToDateTime(localSystemTime);
end;
{$endif}{$endif}

// SystemMillisecondsToUnixTime
//
function SystemMillisecondsToUnixTime(t : Int64) : Int64;
begin
   Result := UnixTime - (GetSystemTimeMilliseconds-t) div 1000;
end;

// UnixTimeToSystemMilliseconds
//
function UnixTimeToSystemMilliseconds(ut : Int64) : Int64;
begin
   Result := GetSystemTimeMilliseconds - (UnixTime-ut)*1000;
end;

// SystemSleep
//
procedure SystemSleep(msec : Integer);
begin
   if msec >= 0 then
      Sleep(msec);
end;

// FirstWideCharOfString
//
function FirstWideCharOfString(const s : String; const default : WideChar = #0) : WideChar;
begin
   {$ifdef FPC}
   if s <> '' then
      Result := PWideChar(String(s))^
   else Result := default;
   {$else}
   if s <> '' then
      Result := PWideChar(Pointer(s))^
   else Result := default;
   {$endif}
end;

// CodePointToUnicodeString
//
procedure CodePointToUnicodeString(c : Integer; var result : UnicodeString);
begin
   case c of
      0..$FFFF :
         Result := WideChar(c);
      $10000..$10FFFF : begin
         c := c-$10000;
         Result := WideChar($D800+(c shr 10))+WideChar($DC00+(c and $3FF));
      end;
   else
      raise EConvertError.CreateFmt('Invalid codepoint: %d', [c]);
   end;
end;

// CodePointToString
//
procedure CodePointToString(const c : Integer; var result : String); inline;
{$ifdef FPC}
var
   buf : UnicodeString;
begin
   CodePointToUnicodeString(c, buf);
   result := String(buf);
{$else}
begin
   CodePointToUnicodeString(c, result);
{$endif}
end;

// UnicodeCompareStr
//
{$ifndef FPC}
function UnicodeCompareStr(const S1, S2 : String) : Integer;
begin
   Result:=CompareStr(S1, S2);
end;
{$endif}

// UnicodeStringReplace
//
function UnicodeStringReplace(const s, oldPattern, newPattern: String; flags: TReplaceFlags) : String;
begin
   Result := System.SysUtils.StringReplace(s, oldPattern, newPattern, flags);
end;

{$ifdef WINDOWS}
const
   CSTR_LESS_THAN    = 1;
   CSTR_EQUAL        = 2;
   CSTR_GREATER_THAN = 3;
function CompareStringEx(
   lpLocaleName: LPCWSTR; dwCmpFlags: DWORD;
   lpString1: LPCWSTR; cchCount1: Integer;
   lpString2: LPCWSTR; cchCount2: Integer;
   lpVersionInformation: Pointer; lpReserved: LPVOID;
   lParam: LPARAM): Integer; stdcall; external 'kernel32.dll';
{$endif}

// UnicodeCompareP
//
function UnicodeCompareP(p1 : PWideChar; n1 : Integer; p2 : PWideChar; n2 : Integer) : Integer;
{$ifdef WINDOWS}
begin
   Result := CompareStringEx(nil, NORM_IGNORECASE, p1, n1, p2, n2, nil, nil, 0);
   if Result = 0 then
      RaiseLastOSError
   else Dec(Result, CSTR_EQUAL);
end;
{$else}
begin
   if IsICUAvailable then
      Result := Integer(ucol_strcoll(GetCollator(UTF8CompareLocale, [coIgnoreCase]), p1, n1, p2, n2))
   else raise Exception.Create('ICU not available (http://site.icu-project.org/home)');
end;
{$endif}

// UnicodeCompareEx
//
function UnicodeCompareEx(const a, b : UnicodeString; const locale : UnicodeString; caseSensitive : Boolean) : Integer;
{$ifdef WINDOWS}
var
   flags : Integer;
begin
   if caseSensitive then
      flags := 0
   else flags := NORM_IGNORECASE;
   Result := CompareStringEx(PChar(locale), flags, PChar(a), Length(a), PChar(b), Length(b), nil, nil, 0);
   if Result = 0 then
      RaiseLastOSError
   else Dec(Result, CSTR_EQUAL);
end;
{$else}
begin
  { TODO : Check unix implementation }
  raise Exception.Create('not implemented');
end;
{$endif}

// UnicodeCompareP
//
function UnicodeCompareP(p1, p2 : PWideChar; n : Integer) : Integer; overload;
{$ifdef WINDOWS}
begin
   Result := CompareStringEx(nil, NORM_IGNORECASE, p1, n, p2, n, nil, nil, 0);
   if Result = 0 then
      RaiseLastOSError
   else Dec(Result, CSTR_EQUAL);
end;
{$else}
begin
   if IsICUAvailable then
      Result := Integer(ucol_strcoll(GetCollator(UTF8CompareLocale, [coIgnoreCase]), p1, n, p2, n))
   else raise Exception.Create('ICU not available (http://site.icu-project.org/home)');
end;
{$endif}

// UnicodeLowerCase
//
procedure UnicodeLowerCase(const s : UnicodeString; var result : UnicodeString);
var
   n : Integer;
begin
   n := Length(s);
   if n > 0 then begin
      {$ifdef WINDOWS}
      SetLength(result, n);
      Winapi.Windows.LCMapStringEx(nil, LCMAP_LOWERCASE or LCMAP_LINGUISTIC_CASING,
                            PWideChar(Pointer(s)), n, PWideChar(Pointer(result)), n,
                            nil, nil, 0);
      {$else}
      result := s.ToLower;
      {$endif}
   end else Result := '';
end;

// UnicodeLowerCase
//
function UnicodeLowerCase(const s : UnicodeString) : UnicodeString;
begin
   UnicodeLowerCase(s, Result);
end;

// UnicodeUpperCase
//
procedure UnicodeUpperCase(const s : UnicodeString; var result : UnicodeString);
var
   n : Integer;
begin
   n := Length(s);
   if n > 0 then begin
      {$ifdef WINDOWS}
      SetLength(result, n);
      Winapi.Windows.LCMapStringEx(nil, LCMAP_UPPERCASE or LCMAP_LINGUISTIC_CASING,
                            PWideChar(Pointer(s)), n, PWideChar(Pointer(result)), n,
                            nil, nil, 0);
      {$else}
      Result := s.ToUpper;
      {$endif}
   end else Result := '';
end;

// UnicodeUpperCase
//
function UnicodeUpperCase(const s : UnicodeString) : UnicodeString;
begin
   UnicodeUpperCase(s, Result);
end;

{$ifdef FPC}
// UnicodeLowerCase
//
function UnicodeLowerCase(const s : String) : String;
begin
   Result := String(UnicodeLowerCase(UnicodeString(s)));
end;

// UnicodeUpperCase
//
function UnicodeUpperCase(const s : String) : String;
begin
   Result := String(UnicodeUpperCase(UnicodeString(s)));
end;
{$endif}

// ASCIICompareText
//
function ASCIICompareText(const s1, s2 : String) : Integer; inline;
begin
   {$ifdef FPC}
   Result := CompareText(UTF8Encode(s1), UTF8Encode(s2));
   {$else}
   Result := CompareText(s1, s2);
   {$endif}
end;

// ASCIISameText
//
function ASCIISameText(const s1, s2 : String) : Boolean; inline;
begin
   {$ifdef FPC}
   Result := (ASCIICompareText(s1, s2)=0);
   {$else}
   Result := SameText(s1, s2);
   {$endif}
end;

// NormalizeString
//
{$ifdef WINDOWS}
function APINormalizeString(normForm : Integer; lpSrcString : LPCWSTR; cwSrcLength : Integer;
                            lpDstString : LPWSTR; cwDstLength : Integer) : Integer;
                            stdcall; external 'Normaliz.dll' name 'NormalizeString' {$ifndef FPC}delayed{$endif};
function NormalizeString(const s, form : String) : String;
var
   nf, len, n : Integer;
begin
   if s = '' then Exit('');
   if (form = '') or (form = 'NFC') then
      nf := 1
   else if form = 'NFD' then
      nf := 2
   else if form = 'NFKC' then
      nf := 5
   else if form = 'NFKD' then
      nf := 6
   else raise Exception.CreateFmt('Unsupported normalization form "%s"', [form]);
   n := 10;
   len := APINormalizeString(nf, Pointer(s), Length(s), nil, 0);
   repeat
      SetLength(Result, len);
      len := APINormalizeString(nf, PWideChar(s), Length(s), Pointer(Result), len);
      if len <= 0 then begin
         if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
            RaiseLastOSError;
         Dec(n);
         if n <= 0 then
            RaiseLastOSError;
         len := -len;
         len := len + (len div 4); // extra margin since estimation failed
         continue;
      end;
   until True;
   SetLength(Result, len);
end;
{$else}
function NormalizeString(const s, form : String) : String;
begin
   { TODO : Unicode character normalization for non Windows platforms }

   // See http://www.unicode.org/reports/tr15/
   // Possible solutions:
   // http://www.delphitop.com/html/danyuan/1472.html
   // https://github.com/graemeg/freepascal/blob/master/rtl/objpas/unicodedata.pas
   Result := s; // TODO
end;
{$endif}

// StripAccents
//
function StripAccents(const s : String) : String;
var
   i : Integer;
   pSrc, pDest : PWideChar;
begin
   Result := NormalizeString(s, 'NFD');
   pSrc := Pointer(Result);
   pDest := pSrc;
   for i := 1 to Length(Result) do begin
      case Ord(pSrc^) of
         $300..$36F : ; // diacritic range
      else
         pDest^ := pSrc^;
         Inc(pDest);
      end;
      Inc(pSrc);
   end;
   SetLength(Result, (NativeUInt(pDest)-NativeUInt(Pointer(Result))) div 2);
end;

// InterlockedIncrement
//
function InterlockedIncrement(var val : Integer) : Integer;
{$ifndef WIN32_ASM}
begin
   {$ifdef WINDOWS}
   Result := Winapi.Windows.InterlockedIncrement(val);
   {$else}
   Result := TInterlocked.Increment(val);
   {$endif}
{$else}
asm
   mov   ecx,  eax
   mov   eax,  1
   lock  xadd [ecx], eax
   inc   eax
{$endif}
end;

// InterlockedDecrement
//
function InterlockedDecrement(var val : Integer) : Integer;
{$ifndef WIN32_ASM}
begin
   {$ifdef WINDOWS}
   Result := Winapi.Windows.InterlockedDecrement(val);
   {$else}
   Result := TInterlocked.Decrement(val);
   {$endif}
{$else}
asm
   mov   ecx,  eax
   mov   eax,  -1
   lock  xadd [ecx], eax
   dec   eax
{$endif}
end;

// FastInterlockedIncrement
//
procedure FastInterlockedIncrement(var val : Integer);
{$ifndef WIN32_ASM}
begin
   AtomicIncrement(val);
{$else}
asm
   lock  inc [eax]
{$endif}
end;

// FastInterlockedDecrement
//
procedure FastInterlockedDecrement(var val : Integer);
{$ifndef WIN32_ASM}
begin
   AtomicDecrement(val);
{$else}
asm
   lock  dec [eax]
{$endif}
end;

// InterlockedExchangePointer
//
function InterlockedExchangePointer(var target : Pointer; val : Pointer) : Pointer;
{$ifndef WIN32_ASM}
begin
   {$ifdef FPC}
   Result := System.InterLockedExchange(target, val);
   {$else}
      {$ifdef WINDOWS}
      Result := Winapi.Windows.InterlockedExchangePointer(target, val);
      {$else}
      Result := TInterlocked.Exchange(target, val);
      {$endif}
   {$endif}
{$else}
asm
   lock  xchg dword ptr [eax], edx
   mov   eax, edx
{$endif}
end;

// InterlockedCompareExchangePointer
//
function InterlockedCompareExchangePointer(var destination : Pointer; exchange, comparand : Pointer) : Pointer; {$IFDEF PUREPASCAL} inline; {$endif}
begin
   {$ifdef FPC}
      {$ifdef CPU64}
      Result := Pointer(System.InterlockedCompareExchange64(QWord(destination), QWord(exchange), QWord(comparand)));
      {$else}
      Result:=System.InterLockedCompareExchange(destination, exchange, comparand);
      {$endif}
   {$else}
      {$ifdef WINDOWS}
      Result := Winapi.Windows.InterlockedCompareExchangePointer(destination, exchange, comparand);
      {$else}
      Result := TInterlocked.CompareExchange(destination, exchange, comparand);
      {$endif}
   {$endif}
end;

// SetThreadName
//
{$ifdef WINDOWS}
function IsDebuggerPresent : BOOL; stdcall; external kernel32 name 'IsDebuggerPresent';
procedure SetThreadName(const threadName : PAnsiChar; threadID : Cardinal = Cardinal(-1));
// http://www.codeproject.com/Articles/8549/Name-your-threads-in-the-VC-debugger-thread-list
type
   TThreadNameInfo = record
      dwType : Cardinal;      // must be 0x1000
      szName : PAnsiChar;     // pointer to name (in user addr space)
      dwThreadID : Cardinal;  // thread ID (-1=caller thread)
      dwFlags : Cardinal;     // reserved for future use, must be zero
   end;
var
   info : TThreadNameInfo;
begin
   if not IsDebuggerPresent then Exit;

   info.dwType:=$1000;
   info.szName:=threadName;
   info.dwThreadID:=threadID;
   info.dwFlags:=0;
   {$ifndef FPC}
   try
      RaiseException($406D1388, 0, SizeOf(info) div SizeOf(Cardinal), @info);
   except
   end;
   {$endif}
end;
{$else}
procedure SetThreadName(const threadName : PAnsiChar; threadID : Cardinal = Cardinal(-1));
begin
   // This one appears limited to Embarcadero debuggers
   TThread.NameThreadForDebugging(threadName, threadID);
end;
{$endif}

// OutputDebugString
//
procedure OutputDebugString(const msg : String);
begin
   {$ifdef WINDOWS}
   Winapi.Windows.OutputDebugStringW(PWideChar(msg));
   {$else}
   { TODO : Check for Linux debugger functionalities }
   {$endif}
end;

// WriteToOSEventLog
//
procedure WriteToOSEventLog(const logName, logCaption, logDetails : String;
                            const logRawData : RawByteString = '');
{$ifdef WINDOWS}
var
  eventSource : THandle;
  detailsPtr : array [0..1] of PWideChar;
begin
   if logName<>'' then
      eventSource:=RegisterEventSourceW(nil, PWideChar(logName))
   else eventSource:=RegisterEventSourceW(nil, PWideChar(ChangeFileExt(ExtractFileName(ParamStr(0)), '')));
   if eventSource>0 then begin
      try
         detailsPtr[0]:=PWideChar(logCaption);
         detailsPtr[1]:=PWideChar(logDetails);
         ReportEventW(eventSource, EVENTLOG_INFORMATION_TYPE, 0, 0, nil,
                      2, Length(logRawData),
                      @detailsPtr, Pointer(logRawData));
      finally
         DeregisterEventSource(eventSource);
      end;
   end;
end;
{$else}
begin
   {$ifdef ANDROID}
      // TODO LogCat
   {$else}
      {$ifdef POSIXSYSLOG}
      Posix.Syslog.syslog(LOG_INFO,logCaption + ': ' + logDetails + '(' + logRawData + ')');
      {$endif}
   {$endif}
end;
{$endif}

// SetDecimalSeparator
//
procedure SetDecimalSeparator(c : Char);
begin
   {$IFDEF FPC}
      FormatSettings.DecimalSeparator := c;
   {$ELSE}
      {$IF CompilerVersion >= 22.0}
      FormatSettings.DecimalSeparator := c;
      {$ELSE}
      DecimalSeparator := c;
      {$IFEND}
   {$ENDIF}
end;

// GetDecimalSeparator
//
function GetDecimalSeparator : Char;
begin
   {$IFDEF FPC}
      Result := FormatSettings.DecimalSeparator;
   {$ELSE}
      {$IF CompilerVersion >= 22.0}
      Result := FormatSettings.DecimalSeparator;
      {$ELSE}
      Result := DecimalSeparator;
      {$IFEND}
   {$ENDIF}
end;

// CollectFiles
//
type
   {$ifdef WINDOWS}
   TFindDataRec = record
      Handle : THandle;
      Data : TWin32FindDataW;
   end;
   {$endif}

   TMasks = array of TMask;

// CollectFilesMasked
//
procedure CollectFilesMasked(const directory : TFileName;
                             const masks : TMasks; list : TStrings;
                             recurseSubdirectories: Boolean = False;
                             onProgress : TCollectFileProgressEvent = nil);
{$ifdef WINDOWS}
const
   // contant defined in Windows.pas is incorrect
   FindExInfoBasic = 1;
var
   searchRec : TFindDataRec;
   infoLevel : TFindexInfoLevels;
   fileName : TFileName;
   skipScan, addToList : Boolean;
   i : Integer;
begin
   // 6.1 required for FindExInfoBasic (Win 2008 R2 or Win 7)
   if ((Win32MajorVersion shl 8) or Win32MinorVersion)>=$601 then
      infoLevel:=TFindexInfoLevels(FindExInfoBasic)
   else infoLevel:=FindExInfoStandard;

   if Assigned(onProgress) then begin
      skipScan := False;
      onProgress(directory, skipScan);
      if skipScan then Exit;
   end;

   fileName := directory+'*';
   searchRec.Handle:=FindFirstFileEx(PChar(fileName), infoLevel,
                                     @searchRec.Data, FINDEX_SEARCH_OPS.FindExSearchNameMatch,
                                     nil, 0);
   if searchRec.Handle<>INVALID_HANDLE_VALUE then begin
      repeat
         if (searchRec.Data.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)=0 then begin
            // check file against mask
            fileName:=searchRec.Data.cFileName;
            addToList := True;
            for i := 0 to High(masks) do begin
               addToList := masks[i].Matches(fileName);
               if addToList then Break;
            end;
            if addToList then begin
               fileName := directory + fileName;
               list.Add(fileName);
            end;
         end else if recurseSubdirectories then begin
            // dive in subdirectory
            if searchRec.Data.cFileName[0]='.' then begin
               if searchRec.Data.cFileName[1]='.' then begin
                  if searchRec.Data.cFileName[2]=#0 then continue;
               end else if searchRec.Data.cFileName[1]=#0 then continue;
            end;
            // decomposed cast and concatenation to avoid implicit string variable
            fileName:=searchRec.Data.cFileName;
            fileName:=directory+fileName+PathDelim;
            CollectFilesMasked(fileName, masks, list, True, onProgress);
         end;
      until not FindNextFileW(searchRec.Handle, searchRec.Data);
      Winapi.Windows.FindClose(searchRec.Handle);
   end;
end;
{$else}
var
   searchRec : TSearchRec;
   fileName : TFileName;
   skipScan : Boolean;
   addToList : Boolean;
   i  : Integer;
begin
   try
      if Assigned(onProgress) then begin
         skipScan := False;
         onProgress(directory, skipScan);
         if skipScan then Exit;
      end;

      fileName := directory + '*';
      if System.SysUtils.FindFirst(fileName,faAnyfile,searchRec) = 0 then begin
         repeat
            if (searchRec.Attr and faVolumeId) = 0 then begin
               if (searchRec.Attr and faDirectory) = 0 then begin
                  fileName  := searchRec.Name;
                  addToList := True;
                  for i := 0 to High(masks) do begin
                     addToList := masks[i].Matches(fileName);
                     if addToList then Break;
                  end;
                  if addToList then begin
                     fileName := directory+fileName;
                     list.Add(fileName);
                  end;
               end else if     recurseSubdirectories
                           and (searchRec.Name <> '.')
                           and (searchRec.Name <> '..') then begin
                  fileName := directory + searchRec.Name + PathDelim;
                  CollectFilesMasked(fileName, masks, list, recurseSubdirectories, onProgress);
               end;
            end;
         until System.SysUtils.FindNext(searchRec) <> 0;
      end;
   finally
      System.SysUtils.FindClose(searchRec);
   end;
end;
{$endif}

// CollectFiles
//
procedure CollectFiles(const directory, fileMask : TFileName; list : TStrings;
                       recurseSubdirectories: Boolean = False;
                       onProgress : TCollectFileProgressEvent = nil);
var
   masks : TMasks;
   p, pNext : Integer;
begin
   if fileMask <> '' then begin
      p := 1;
      repeat
         pNext := Pos(';', fileMask, p);
         if pNext < p then begin
            SetLength(masks, Length(masks)+1);
            masks[High(masks)] := TMask.Create(Copy(fileMask, p));
            break;
         end;
         if pNext > p then begin
            SetLength(masks, Length(masks)+1);
            masks[High(masks)] := TMask.Create(Copy(fileMask, p, pNext-p));
         end;
         p := pNext + 1;
      until p > Length(fileMask);
   end;
   // Windows can match 3 character filters with old DOS filenames
   // Mask confirmation is necessary
   try
      CollectFilesMasked(IncludeTrailingPathDelimiter(directory), masks,
                         list, recurseSubdirectories, onProgress);
   finally
      for p := 0 to High(masks) do
         masks[p].Free;
   end;
end;

// CollectSubDirs
//
procedure CollectSubDirs(const directory : TFileName; list : TStrings);
{$ifdef WINDOWS}
const
   // contant defined in Windows.pas is incorrect
   FindExInfoBasic = 1;
var
   searchRec : TFindDataRec;
   infoLevel : TFindexInfoLevels;
   fileName : TFileName;
begin
   // 6.1 required for FindExInfoBasic (Win 2008 R2 or Win 7)
   if ((Win32MajorVersion shl 8) or Win32MinorVersion)>=$601 then
      infoLevel:=TFindexInfoLevels(FindExInfoBasic)
   else infoLevel:=FindExInfoStandard;

   fileName := directory+'*';
   searchRec.Handle:=FindFirstFileEx(PChar(fileName), infoLevel,
                                     @searchRec.Data, FINDEX_SEARCH_OPS.FindExSearchLimitToDirectories,
                                     nil, 0);
   if searchRec.Handle<>INVALID_HANDLE_VALUE then begin
      repeat
         if (searchRec.Data.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)<>0 then begin
            if searchRec.Data.cFileName[0]='.' then begin
               if searchRec.Data.cFileName[1]='.' then begin
                  if searchRec.Data.cFileName[2]=#0 then continue;
               end else if searchRec.Data.cFileName[1]=#0 then continue;
            end;
            // decomposed cast and concatenation to avoid implicit string variable
            fileName := searchRec.Data.cFileName;
            list.Add(fileName);
         end;
      until not FindNextFileW(searchRec.Handle, searchRec.Data);
      Winapi.Windows.FindClose(searchRec.Handle);
   end;
end;
{$else}
var
   searchRec : TSearchRec;
begin
   try
      if System.SysUtils.FindFirst(directory + '*', faDirectory, searchRec) = 0 then begin
         repeat
            if     (searchRec.Attr and faDirectory > 0)
               and (searchRec.Name <> '.' )
               and (searchRec.Name <> '..' ) then
               list.Add(searchRec.Name);
         until System.SysUtils.FindNext(searchRec) <> 0;
      end;
   finally
      System.SysUtils.FindClose(searchRec);
   end;
end;
{$endif}

{$ifdef FPC}
// VarCopy
//
procedure VarCopy(out dest : Variant; const src : Variant);
begin
   dest:=src;
end;
{$else}
// VarToUnicodeStr
//
function VarToUnicodeStr(const v : Variant) : String; inline;
begin
   Result := VarToStr(v);
end;
{$endif FPC}

{$ifdef FPC}
// Utf8ToUnicodeString
//
function Utf8ToUnicodeString(const buf : RawByteString) : UnicodeString; inline;
begin
   Result := UTF8Decode(buf);
end;
{$endif}

// RawByteStringToBytes
//
function RawByteStringToBytes(const buf : RawByteString) : TBytes;
var
   n : Integer;
begin
   n:=Length(buf);
   SetLength(Result, n);
   if n>0 then
      System.Move(buf[1], Result[0], n);
end;

// BytesToRawByteString
//
function BytesToRawByteString(const buf : TBytes; startIndex : Integer = 0) : RawByteString;
var
   n : Integer;
begin
   n:=Length(buf)-startIndex;
   if n<=0 then
      Result:=''
   else begin
      SetLength(Result, n);
      System.Move(buf[startIndex], Pointer(Result)^, n);
   end;
end;

// PosExA
//
function PosExA(const needle, haystack : RawByteString; hayStackOffset : Integer) : Integer;
var
	lenNeedle : Integer;
   charToFind : AnsiChar;
   pHaystack, pEndSearch : PAnsiChar;
begin
	Result := 0;
	if (haystack = '') or (needle = '') then Exit;
   if hayStackOffset <= 0 then
      hayStackOffset := 1;

   lenNeedle := Length(needle);

   charToFind := PAnsiChar(Pointer(needle))^;

   pHaystack := Pointer(hayStack);
   Inc(pHaystack, hayStackOffset - 1);

   pEndSearch := Pointer(hayStack);
   Inc(pEndSearch, Length(haystack) - lenNeedle + 1);

   while pHaystack <= pEndSearch do begin
      while pHaystack^ <> charToFind do begin
         Inc(pHaystack);
         if pHaystack > pEndSearch then Break;
      end;
      if CompareMem(pHaystack, Pointer(needle), lenNeedle) then begin
         Result := NativeUInt(pHaystack) - NativeUInt(Pointer(haystack)) + 1;
         Break;
      end;
      Inc(pHaystack);
   end;
end;

// BytesToRawByteString
//
function BytesToRawByteString(p : Pointer; size : Integer) : RawByteString;
begin
   SetLength(Result, size);
   System.Move(p^, Pointer(Result)^, size);
end;

// BytesToScriptString
//
procedure BytesToScriptString(const p : PByteArray; n : Integer; var result : UnicodeString);
begin
   SetLength(result, n);
   BytesToWords(p, PWordArray(Pointer(result)), n);
end;

// WordsToBytes
//
procedure WordsToBytes(src : PWordArray; dest : PByteArray; nbWords : Integer);
{$ifdef WIN64_ASM}
asm  // src -> rcx     dest -> rdx      nbBytes -> r8
   cmp         r8, 16
   jb          @@tail8

   mov         eax, r8d
   shr         eax, 4
   and         r8, 15

@@loop16:
   movdqu      xmm1, [rcx]
   movdqu      xmm2, [rcx+16]
   packuswb    xmm1, xmm2
   movdqu      [rdx], xmm1
   add         rcx,  32
   add         rdx,  16
   dec         eax
   jnz         @@loop16

@@tail8:
   cmp         r8, 8
   jb          @@tail

   and         r8, 7
   movdqu      xmm1, [rcx]
   packuswb    xmm1, xmm1
   movq        [rdx], xmm1
   add         rcx,  16
   add         rdx,  8

@@tail:
   test        r8, r8
   jz          @@end

@@loop1:
   mov         ax, [rcx+r8*2-2]
   mov         [rdx+r8-1], al
   dec         r8
   jnz         @@loop1

@@end:
end;
{$else}
begin
   while nbWords >= 4 do begin
      Dec(nbWords, 4);
      dest[0] := Byte(src[0]);
      dest[1] := Byte(src[1]);
      dest[2] := Byte(src[2]);
      dest[3] := Byte(src[3]);
      dest := @dest[4];
      src := @src[4];
   end;
   while nbWords > 0 do begin
      Dec(nbWords);
      dest[0] := Byte(src[0]);
      dest := @dest[1];
      src := @src[1];
   end;
end;
{$endif}

// BytesToWords
//
procedure BytesToWords(src : PByteArray; dest : PWordArray; nbBytes : Integer);
{$ifdef WIN64_ASM}
asm  // src -> rcx     dest -> rdx      nbBytes -> r8
   pxor        xmm0, xmm0

   cmp         r8, 16
   jb          @@tail8

   mov         eax, r8d
   shr         eax, 4
   and         r8, 15

@@loop16:
   movq        xmm1, [rcx]
   movq        xmm2, [rcx+8]
   punpcklbw   xmm1, xmm0
   punpcklbw   xmm2, xmm0
   movdqu      [rdx], xmm1
   movdqu      [rdx+16], xmm2
   add         rcx,  16
   add         rdx,  32
   dec         eax
   jnz         @@loop16

@@tail8:
   cmp         r8, 8
   jb          @@tail

   and         r8, 7
   movq        xmm1, [rcx]
   punpcklbw   xmm1, xmm0
   movdqu      [rdx], xmm1
   add         rcx,  8
   add         rdx,  16

@@tail:
   test        r8, r8
   jz          @@end

@@loop1:
   movzx       eax, [rcx + r8 - 1]
   mov         [rdx + r8*2 - 2], ax
   dec         r8
   jnz         @@loop1

@@end:
end;
{$else}
begin
   while nbBytes >= 4 do begin
      Dec(nbBytes, 4);
      dest[0] := src[0];
      dest[1] := src[1];
      dest[2] := src[2];
      dest[3] := src[3];
      dest := @dest[4];
      src := @src[4];
   end;
   while nbBytes > 0 do begin
      Dec(nbBytes);
      dest[0] := src[0];
      dest := @dest[1];
      src := @src[1];
   end;
end;
{$endif}

// TryTextToFloat
//
function TryTextToFloat(const s : PChar; var value : Extended; const formatSettings : TFormatSettings) : Boolean;
{$ifdef FPC}
var
   cw : Word;
begin
   cw:=Get8087CW;
   Set8087CW($133F);
   if TryStrToFloat(s, value, formatSettings) then
      Result:=(value>-1.7e308) and (value<1.7e308);
   if not Result then
      value:=0;
   asm fclex end;
   Set8087CW(cw);
{$else}
begin
//   Result:=TextToFloat(s, value, fvExtended, formatSettings);
   Result := TryStrToFloat(s, value, formatSettings);
//   Result := StrToFloat(s, formatSettings);
{$endif}
end;

// TryTextToFloatW
//
function TryTextToFloatW(const s : PWideChar; var value : Extended;
                        const formatSettings : TFormatSettings) : Boolean;
{$ifdef FPC}
var
   bufU : UnicodeString;
   buf : String;
begin
   bufU := s;
   buf := String(bufU);
   Result := TryTextToFloat(PChar(buf), value, formatSettings);
{$else}
begin
   Result:=TextToFloat(s, value, fvExtended, formatSettings)
{$endif}
end;

// LoadTextFromBuffer
//
function LoadTextFromBuffer(const buf : TBytes) : UnicodeString;
var
   n, sourceLen, len : Integer;
   encoding : TEncoding;
begin
   if buf=nil then
      Result:=''
   else begin
      encoding := nil;
      n := TEncoding.GetBufferEncoding(buf, encoding);
      if n = 0 then
         encoding := TEncoding.UTF8;
      if encoding = TEncoding.UTF8 then begin
         // handle UTF-8 directly, encoding.GetString returns an empty string
         // whenever a non-utf-8 character is detected, the implementation below
         // will return a '?' for non-utf8 characters instead
         sourceLen := Length(buf)-n;
         SetLength(Result, sourceLen);
         len := Utf8ToUnicode(Pointer(Result), sourceLen+1, PAnsiChar(buf)+n, sourceLen)-1;
         if len>0 then begin
            if len<>sourceLen then
               SetLength(Result, len);
         end else Result:=''
      end else begin
         Result:=encoding.GetString(buf, n, Length(buf)-n);
      end;
   end;
end;

// LoadTextFromRawBytes
//
function LoadTextFromRawBytes(const buf : RawByteString) : UnicodeString;
var
   b : TBytes;
begin
   if buf='' then Exit('');
   SetLength(b, Length(buf));
   System.Move(buf[1], b[0], Length(buf));
   Result:=LoadTextFromBuffer(b);
end;

// LoadTextFromStream
//
function LoadTextFromStream(aStream : TStream) : UnicodeString;
var
   n : Integer;
   buf : TBytes;
begin
   n := aStream.Size-aStream.Position;
   SetLength(buf, n);
   aStream.Read(buf[0], n);
   Result:=LoadTextFromBuffer(buf);
end;

// LoadTextFromFile
//
function LoadTextFromFile(const fileName : TFileName; const options : TOpenFileOptions = [ ]) : UnicodeString;
var
   buf : TBytes;
begin
   buf := LoadDataFromFile(fileName, options);
   Result := LoadTextFromBuffer(buf);
end;

// ReadFileChunked
//
{$IFDEF WINDOWS}
function ReadFileChunked(hFile : THandle; const buffer; size : Integer) : Integer;
const
   CHUNK_SIZE = 16384;
var
   p : PByte;
   nRemaining : Integer;
   nRead : Cardinal;
begin
   p := @buffer;
   nRemaining := size;
   repeat
      if nRemaining > CHUNK_SIZE then
         nRead := CHUNK_SIZE
      else nRead := nRemaining;
      if not ReadFile(hFile, p^, nRead, nRead, nil) then
         RaiseLastOSError
      else if nRead = 0 then begin
         // file got trimmed while we were reading
         Exit(size-nRemaining);
      end;
      Dec(nRemaining, nRead);
      Inc(p, nRead);
   until nRemaining <= 0;
   Result := size;
end;
{$ENDIF}

// LoadDataFromFile
//
function LoadDataFromFile(const fileName : TFileName; const options : TOpenFileOptions = [ ]) : TBytes;
{$ifdef WINDOWS}
const
   INVALID_FILE_SIZE = DWORD($FFFFFFFF);
var
   hFile : THandle;
   n, nRead : Cardinal;
begin
   if fileName='' then Exit(nil);
   hFile:=OpenFileForSequentialReadOnly(fileName, options);
   if hFile=INVALID_HANDLE_VALUE then Exit(nil);
   try
      n:=GetFileSize(hFile, nil);
      if n=INVALID_FILE_SIZE then
         RaiseLastOSError;
      if n>0 then begin
         SetLength(Result, n);
         nRead := ReadFileChunked(hFile, Result[0], n);
         if nRead < n then
            SetLength(Result, nRead);
      end else Result:=nil;
   finally
      CloseFileHandle(hFile);
   end;
end;
{$else}
begin
   if fileName = '' then
      Result := nil
   else Result := System.IOUTils.TFile.ReadAllBytes(filename);
end;
{$endif}

// SaveDataToFile
//
procedure SaveDataToFile(const fileName : TFileName; const data : TBytes);
{$IFDEF WINDOWS}
var
   hFile : THandle;
   n, nWrite : DWORD;
begin
   hFile:=OpenFileForSequentialWriteOnly(fileName);
   try
      n:=Length(data);
      if n>0 then
         if not WriteFile(hFile, data[0], n, nWrite, nil) then
            RaiseLastOSError;
   finally
      CloseFileHandle(hFile);
   end;
end;
{$ELSE}
begin
   System.IOUTils.TFile.WriteAllBytes(fileName, data);
end;
{$ENDIF}

// LoadRawBytesFromFile
//
function LoadRawBytesFromFile(const fileName : TFileName) : RawByteString;
{$ifdef WINDOWS}
const
   INVALID_FILE_SIZE = DWORD($FFFFFFFF);
var
   hFile : THandle;
   n, nRead : Cardinal;
begin
   if fileName = '' then Exit;
   hFile := OpenFileForSequentialReadOnly(fileName);
   if hFile = INVALID_HANDLE_VALUE then Exit;
   try
      n:=GetFileSize(hFile, nil);
      if n=INVALID_FILE_SIZE then
         RaiseLastOSError;
      if n>0 then begin
         SetLength(Result, n);
         nRead := ReadFileChunked(hFile, Pointer(Result)^, n);
         if nRead < n then
            SetLength(Result, nRead);
      end;
   finally
      CloseFileHandle(hFile);
   end;
end;
{$else}
var
   fs : TFileStream;
begin
   if fileName = '' then Exit;
   fs := TFileStream.Create(fileName, fmOpenRead);
   try
      SetLength(Result, fs.Size);
      if fs.Read(Pointer(Result)^, fs.Size) <> fs.Size then
         raise Exception.Create('stream read exception - data size mismatch');
   finally
      fs.Free;
   end;
end;
{$endif}

// SaveRawBytesToFile
//
function SaveRawBytesToFile(const fileName : TFileName; const data : RawByteString) : Integer;
{$ifdef WINDOWS}
var
   hFile : THandle;
   nWrite : DWORD;
begin
   Result:=0;
   hFile:=OpenFileForSequentialWriteOnly(fileName);
   try
      if data<>'' then begin
         Result:=Length(data);
         if not WriteFile(hFile, data[1], Result, nWrite, nil) then
            RaiseLastOSError;
      end;
   finally
      CloseFileHandle(hFile);
   end;
end;
{$else}
var
   fs   : TFileStream;
   dataSize : LongInt;
begin
   fs := TFileStream.Create(fileName,fmCreate);
   fs.Seek(0, soEnd);
   try
      dataSize := Length(data);
      Result := fs.Write(Pointer(data)^, dataSize);
      if Result <> dataSize then
         raise Exception.Create('stream write exception - data size mismatch')
   finally
      fs.Free;
   end;
end;
{$endif}

// LoadRawBytesAsScriptStringFromFile
//
procedure BytesToWordsInPlace(p : Pointer; n : NativeInt);
{$ifdef WIN64_ASM}
asm  // p -> rcx     n -> rdx
   lea         r8,  [ rcx + rdx ]   //  r8 -> src
   lea         rcx, [ rcx + rdx*2 ] // rcx -> dest

   cmp         rdx, 16
   jb          @@lessthan16

   mov         rax, rdx
   shr         rax, 4
   and         rdx, 15

   pxor        xmm0, xmm0

@@loop16:
   sub         r8,  16
   sub         rcx, 32

   movq        xmm1, [r8]
   movq        xmm2, [r8+8]
   punpcklbw   xmm1, xmm0
   punpcklbw   xmm2, xmm0
   movdqu      [rcx], xmm1
   movdqu      [rcx+16], xmm2

   sub         rax, 1
   jnz         @@loop16

@@lessthan16:
   test        rdx, rdx
   jz          @@end

@@loop1:
   sub         r8, 1
   sub         rcx, 2
   movzx       ax, [r8]
   mov         [rcx], ax
   sub         rdx, 1
   jnz         @@loop1

@@end:
end;
{$else}
begin
   var pw := PWord(IntPtr(p) + (n-1) * SizeOf(Word));
   var pb := PByte(IntPtr(p) + (n-1) * SizeOf(Byte));
   while n > 0 do begin
      pw^ := pb^;
      Dec(pw);
      Dec(pb);
      Dec(n);
   end;
end;
{$endif}

procedure LoadRawBytesAsScriptStringFromFile(const fileName : TFileName; var result : String);
{$ifdef WINDOWS}
const
   INVALID_FILE_SIZE = DWORD($FFFFFFFF);
var
   hFile : THandle;
   n : Int64;
   nRead : Cardinal;
   pDest : PWord;
begin
   if fileName = '' then Exit;
   hFile := OpenFileForSequentialReadOnly(fileName);
   if hFile = INVALID_HANDLE_VALUE then Exit;
   try
      if not GetFileSizeEx(hFile, n) then
         RaiseLastOSError;
      if n > 0 then begin
         SetLength(Result, n);
         if Length(Result) <> n then
            raise Exception.CreateFmt('File too large (%d)', [ n ]);
         pDest := Pointer(Result);
         ReadFile(hFile, pDest^, n, nRead, nil);
         if nRead <> n then begin
            // file got trimmed while we were reading
            SetLength(Result, nRead);
            n := nRead;
         end;
         if n > 0 then
            BytesToWordsInPlace(pDest, n);
      end;
   finally
      CloseFileHandle(hFile);
   end;
end;
{$else}
var
   buf : RawByteString;
begin
   buf := LoadRawBytesFromFile(fileName);
   if buf <> '' then
      BytesToScriptString(Pointer(buf), Length(buf), Result)
   else Result := '';
end;
{$endif}

// SaveTextToUTF8File
//
procedure SaveTextToUTF8File(const fileName : TFileName; const text : String);
begin
   SaveRawBytesToFile(fileName, StringToUTF8(text));
end;

// AppendTextToUTF8File
//
procedure AppendTextToUTF8File(const fileName : TFileName; const text : UTF8String);
var
   fs : TFileStream;
begin
   if text='' then Exit;
   if FileExists(fileName) then
      fs:=TFileStream.Create(fileName, fmOpenWrite or fmShareDenyNone)
   else fs:=TFileStream.Create(fileName, fmCreate);
   try
      fs.Seek(0, soFromEnd);
      fs.Write(text[1], Length(text));
   finally
      fs.Free;
   end;
end;

// OpenFileForSequentialReadOnly
//
function OpenFileForSequentialReadOnly(
   const fileName : TFileName;
   const options : TOpenFileOptions = [ ]
   ) : THandle;
begin
   {$IFDEF WINDOWS}
   Result:=CreateFile(PChar(fileName), GENERIC_READ, FILE_SHARE_READ+FILE_SHARE_WRITE,
                      nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
   if (Result = INVALID_HANDLE_VALUE) and not (ofoNoRaiseError in options) then begin
      if GetLastError <> ERROR_FILE_NOT_FOUND then
         RaiseLastOSError;
   end;
   {$ELSE}
   Result := System.SysUtils.FileCreate(fileName, fmOpenRead, $007);
   if (Result = INVALID_HANDLE_VALUE) and not (ofoNoRaiseError in options) then
      raise Exception.Create('invalid file handle');
   {$ENDIF}
end;

// OpenFileForSequentialWriteOnly
//
function OpenFileForSequentialWriteOnly(
   const fileName : TFileName;
   const options : TOpenFileOptions = [ ]
   ) : THandle;
begin
   {$ifdef WINDOWS}
   var attributes : Cardinal;
   Result := CreateFile(
      PChar(fileName), GENERIC_WRITE, 0, nil, CREATE_ALWAYS,
      FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN, 0
   );
   if (Result = INVALID_HANDLE_VALUE) and (GetLastError = ERROR_ACCESS_DENIED) then begin
      // could be a file with non-normal attributes, try again after reading attributes
      attributes := GetFileAttributes(PChar(fileName));
      if attributes <> INVALID_FILE_ATTRIBUTES then begin
         Result := CreateFile(
            PChar(fileName), GENERIC_WRITE, 0, nil, CREATE_ALWAYS,
            attributes or FILE_FLAG_SEQUENTIAL_SCAN, 0
         );
      end;
   end;
   {$else}
   Result := System.SysUtils.FileCreate(fileName, fmOpenWrite, $007);
   {$endif}
   if (Result = INVALID_HANDLE_VALUE) and not (ofoNoRaiseError in options) then
      RaiseLastOSError;
end;

// CloseFileHandle
//
function CloseFileHandle(hFile : THandle) : Boolean;
begin
   {$IFDEF WINDOWS}
   Result := Winapi.Windows.CloseHandle(hFile);
   {$else}
   System.SysUtils.FileClose(hFile);
   Result := True; // assume success, as RTL does not say
   {$endif}
end;

// FileClose
//
function FileClose(hFile : THandle) : Boolean;
begin
   Result := CloseFileHandle(hFile);
end;

// FileWrite
//
function FileWrite(hFile : THandle; buffer : Pointer; byteCount : Int64) : Int64;
const
   cOneGigabyte = 1024*1024*1024;
var
   bytesToWrite : Integer;
begin
   Result := 0;
   while byteCount > 0 do begin
      if byteCount > cOneGigabyte then
         bytesToWrite := cOneGigabyte
      else bytesToWrite := byteCount;
      {$ifdef WINDOWS}
      if not WriteFile(hFile, buffer^, bytesToWrite, Cardinal(bytesToWrite), nil) then
         RaiseLastOSError;
      {$else}
      bytesToWrite := System.SysUtils.FileWrite(hFile, buffer^, byteCount);
      if bytesToWrite = -1 then
         raise Exception.Create('file write exception');
      {$endif}
      Assert(bytesToWrite > 0);
      Dec(byteCount, bytesToWrite);
      Inc(IntPtr(buffer), bytesToWrite);
      Inc(Result, bytesToWrite);
   end;
end;

// FileRead
//
function FileRead(hFile : THandle; buffer : Pointer; byteCount : Int64) : Int64;
const
   cOneGigabyte = 1024*1024*1024;
var
   bytesToRead : Integer;
begin
   Result := 0;
   while byteCount > 0 do begin
      {$ifdef WINDOWS}
      if byteCount > cOneGigabyte then
         bytesToRead := cOneGigabyte
      else bytesToRead := byteCount;
      if not ReadFile(hFile, buffer^, bytesToRead, Cardinal(bytesToRead), nil) then
         RaiseLastOSError;
      {$else}
      bytesToRead := System.SysUtils.FileRead(hFile, buffer^, byteCount);
      if bytesToRead = -1 then
         raise Exception.Create('file read exception');
      {$endif}
      if bytesToRead > 0 then begin
         Dec(byteCount, bytesToRead);
         Inc(IntPtr(buffer), bytesToRead);
         Inc(Result, bytesToRead);
      end else Break;
   end;
end;

// FileFlushBuffers
//
{$ifdef WINDOWS}
function FlushFileBuffers(hFile : THandle) : BOOL; stdcall; external 'kernel32.dll';
function FileFlushBuffers(hFile : THandle) : Boolean;
begin
   Result := FlushFileBuffers(hFile);
end;
{$else}
function FileFlushBuffers(hFile : THandle) : Boolean;
begin
   // TODO
end;
{$endif}

// SetEndOfFile
//
function SetEndOfFile(hFile : THandle) : Boolean;
begin
   {$ifdef WINDOWS}
   Result := Winapi.Windows.SetEndOfFile(hFile);
   {$else}
   Result := (ftruncate(hFile,lseek(hfile, 0, SEEK_CUR)) = 0);
   {$endif}
end;

// FileCopy
//
function FileCopy(const existing, new : TFileName; failIfExists : Boolean) : Boolean;
begin
   {$ifdef WINDOWS}
   Result := Winapi.Windows.CopyFileW(PWideChar(existing), PWideChar(new), failIfExists);
   {$else}
   try
      System.IOUtils.TFile.Copy(existing, new, not failIfExists);
      Result := True;
   except
      Result := False;
   end;
   {$endif}
end;

// FileMove
//
function FileMove(const existing, new : TFileName) : Boolean;
begin
   {$ifdef WINDOWS}
   Result := Winapi.Windows.MoveFileW(PWideChar(existing), PWideChar(new));
   {$else}
   try
      System.IOUtils.TFile.Move(existing, new);
      Result := True;
   except
      Result := False;
   end;
   {$endif}
end;

// FileDelete
//
function FileDelete(const fileName : TFileName) : Boolean;
begin
   Result := System.SysUtils.DeleteFile(fileName);
end;

// FileRename
//
function FileRename(const oldName, newName : TFileName) : Boolean;
begin
   Result:=RenameFile(oldName, newName);
end;

// FileSize
//
function FileSize(const name : TFileName) : Int64;
{$ifdef WINDOWS}
var
   info : TWin32FileAttributeData;
begin
   if GetFileAttributesExW(PWideChar(Pointer(name)), GetFileExInfoStandard, @info) then
      Result := info.nFileSizeLow or (Int64(info.nFileSizeHigh) shl 32)
   else Result := -1;
end;
{$else}
var
   searchRec : TSearchRec;
begin
   try
      if System.SysUtils.FindFirst(name, faAnyFile, searchRec) = 0 then
         Result := searchRec.Size
      else Result := 0;
   finally
      System.SysUtils.FindClose(searchRec);
   end;
end;
{$endif}

// FileCreationTime
//
function FileCreationTime(const name : TFileName) : TdwsDateTime; overload;
{$ifdef WINDOWS}
var
   info : TWin32FileAttributeData;
   buf : TdwsDateTime;
begin
   if GetFileAttributesExW(PWideChar(Pointer(name)), GetFileExInfoStandard, @info) then begin
      buf.AsFileTime := info.ftCreationTime;
   end else buf.Clear;
   Result := buf;
end;
{$else}
begin
   Result := FileDateTime(name);
end;
{$endif}

// FileDateTime
//
function FileDateTime(hFile : THandle; lastAccess : Boolean = False) : TdwsDateTime; overload;
{$ifdef WINDOWS}
var
   fileTime : TFileTime;
begin
   fileTime.dwLowDateTime := 0;
   fileTime.dwHighDateTime := 0;
   if lastAccess then
      GetFileTime(hFile, nil, @fileTime, nil)
   else GetFileTime(hFile, nil, nil, @fileTime);
   Result.AsFileTime := fileTime;
end;
{$else}
begin
   Result.AsLocalDateTime := FileDateToDateTime(FileGetDate(hFile));
end;
{$endif}

// FileDateTime
//
function FileDateTime(const name : TFileName; lastAccess : Boolean = False) : TdwsDateTime;
{$ifdef WINDOWS}
var
   info : TWin32FileAttributeData;
   buf : TdwsDateTime;
begin
   if GetFileAttributesExW(PWideChar(Pointer(name)), GetFileExInfoStandard, @info) then begin
      if lastAccess then
         buf.AsFileTime := info.ftLastAccessTime
      else buf.AsFileTime := info.ftLastWriteTime;
   end else buf.Clear;
   Result := buf;
end;
{$else}
var
   searchRec : TSearchRec;
   buf : TdwsDateTime;
begin
   try
      if System.SysUtils.FindFirst(name, faAnyFile, searchRec) = 0 then
         buf.AsLocalDateTime := searchRec.TimeStamp
      else buf.Clear;
   finally
      System.SysUtils.FindClose(searchRec);
   end;
   Result := buf;
end;
{$endif}

// FileSetDateTime
//
procedure FileSetDateTime(hFile : THandle; const aDateTime : TdwsDateTime);
{$IFDEF WINDOWS}
var
   doNotChange, newTimeStamp : TFileTime;
begin
   newTimeStamp := aDateTime.AsFileTime;
   doNotChange.dwLowDateTime  := Cardinal(-1);
   doNotChange.dwHighDateTime := Cardinal(-1);
   SetFileTime(hFile, @doNotChange, @newTimeStamp, @newTimeStamp);
end;
{$ELSE}
begin
  { TODO : Check unix implementation }
  raise Exception.Create('not implemented');
end;
{$ENDIF}

// FileSetDateTime
//
procedure FileSetDateTime(const name : TFileName; const aDateTime : TdwsDateTime);
var
   hFile : THandle;
begin
   hFile := FileOpen(name, fmOpenReadWrite);
   try
      FileSetDateTime(hFile, aDateTime);
   finally
      FileClose(hFile);
   end;
end;

// DeleteDirectory
//
function DeleteDirectory(const path : String; recursive : Boolean = True) : Boolean;
begin
   {$ifdef FPC}
   Result := RemoveDir(path);
   {$else}
   try
      TDirectory.Delete(path, recursive);
   except
      Exit(False);
   end;
   Result := not TDirectory.Exists(path);
   {$endif}
end;

// DirectSet8087CW
//
function DirectSet8087CW(newValue: Word): Word; register;
{$IFNDEF WIN32_ASM}
begin
   Result:=newValue;
{$else}
asm
   push    eax
   push    eax
   fnstcw  [esp]
   fnclex
   pop     eax
   fldcw   [esp]
   pop     edx
{$endif}
end;

// DirectSetMXCSR
//
function DirectSetMXCSR(newValue : Word) : Word; register;
{$ifdef WIN32_ASM}
asm
   and      eax, $FFC0
   push     eax
   push     eax
   stmxcsr  [esp+4]
   ldmxcsr  [esp]
   pop eax
   pop eax
{$else}
begin
   Result:=newValue;
{$endif}
end;

// SwapBytes
//
function SwapBytes(v : Cardinal) : Cardinal;
{$ifdef WIN32_ASM}
asm
   bswap eax
end;
{$else}{$ifdef WIN64_ASM}
asm
   bswap ecx
   mov   eax, ecx
end;
{$else}
type
   TCardinalBytes = array [0..3] of Byte;
begin
   TCardinalBytes(Result)[0] := TCardinalBytes(v)[3];
   TCardinalBytes(Result)[1] := TCardinalBytes(v)[2];
   TCardinalBytes(Result)[2] := TCardinalBytes(v)[1];
   TCardinalBytes(Result)[3] := TCardinalBytes(v)[0];
end;
{$endif}{$endif}

// SwapBytesBlock
//
procedure SwapBytesBlock(src, dest : PByte; nb : Integer);
begin
   Inc(dest, nb-1);
   while nb > 0 do begin
      dest^ := src^;
      Inc(src);
      Dec(dest);
      Dec(nb);
   end;
end;

// SwapBytesInt16
//
procedure SwapBytesInt16(src, dest : PWORD);
{$ifdef WIN64_ASM}
asm
   mov   ax, [rcx]
   rol   ax, 8
   mov   [rdx], ax
end;
{$else}{$ifdef WIN32_ASM}
asm
   mov   ax, word ptr [eax]
   rol   ax, 8
   mov   [ecx], ax
end;
{$else}
begin
   var b0 := PByteArray(src)[0];
   var b1 := PByteArray(src)[1];
   PByteArray(dest)[0] := b1;
   PByteArray(dest)[1] := b0;
end;
{$endif}{$endif}

// SwapBytesInt32
//
procedure SwapBytesInt32(src, dest : PInt32);
{$ifdef WIN64_ASM}
asm
   mov   eax, [rcx]
   bswap eax
   mov   [rdx], eax
end;
{$else}{$ifdef WIN32_ASM}
asm
   mov   ecx, [eax]
   bswap ecx
   mov   [edx], ecx
end;
{$else}
begin
   var b0 := PByteArray(src)[0];
   var b1 := PByteArray(src)[1];
   var b2 := PByteArray(src)[2];
   var b3 := PByteArray(src)[3];
   PByteArray(dest)[0] := b3;
   PByteArray(dest)[1] := b2;
   PByteArray(dest)[2] := b1;
   PByteArray(dest)[3] := b0;
end;
{$endif}{$endif}

// SwapBytesInt64
//
procedure SwapBytesInt64(src, dest : PInt64);
{$ifdef WIN64_ASM}
asm
   mov   rax, [rcx]
   bswap rax
   mov   [rdx], rax
end;
{$else}{$ifdef WIN32_ASM}
asm
   mov   ecx, [eax]
   mov   eax, [eax+4]
   bswap ecx
   bswap eax
   mov   [edx+4], ecx
   mov   [edx], eax
end;
{$else}
begin
   var b0 := PByteArray(src)[0];
   var b1 := PByteArray(src)[1];
   var b2 := PByteArray(src)[2];
   var b3 := PByteArray(src)[3];
   var b4 := PByteArray(src)[4];
   var b5 := PByteArray(src)[5];
   var b6 := PByteArray(src)[6];
   var b7 := PByteArray(src)[7];
   PByteArray(dest)[0] := b7;
   PByteArray(dest)[1] := b6;
   PByteArray(dest)[2] := b5;
   PByteArray(dest)[3] := b4;
   PByteArray(dest)[4] := b3;
   PByteArray(dest)[5] := b2;
   PByteArray(dest)[6] := b1;
   PByteArray(dest)[7] := b0;
end;
{$endif}{$endif}

// RDTSC
//
{$ifdef WINDOWS}
function RDTSC : UInt64;
asm
   RDTSC
   {$ifdef WIN64}
   SHL   RDX, 32
   OR    RAX, RDX
   {$endif}
end;
{$else}
var vFakeRDTSC :  Int64;
function RDTSC : UInt64;
begin
   // TODO : Implement true RDTSC function
   // if asm does not work we use a fake, monotonous, vaguely random ersatz
   Result := Int64(AtomicIncrement(vFakeRDTSC, 1+(GetSystemTimeMilliseconds and $ffff)*7919));
end;
{$endif}

// GetCurrentUserName
//
function GetCurrentUserName : String;
{$ifdef WINDOWS}
var
   len : Cardinal;
begin
   len:=255;
   SetLength(Result, len);
   Winapi.Windows.GetUserNameW(PWideChar(Result), len);
   SetLength(Result, len-1);
end;
{$else}
begin
   Result := Posix.Unistd.getlogin;
end;
{$endif}

{$ifndef FPC}
// Delphi 2009 is not able to cast a generic T instance to TObject or Pointer
function TtoObject(const T): TObject;
begin
// Manually inlining the code would require the IF-defs
//{$IF Compilerversion >= 21}
   Result := TObject(T);
//{$ELSE}
//   Result := PObject(@T)^;
//{$IFEND}
end;

function TtoPointer(const T): Pointer;
begin
// Manually inlining the code would require the IF-defs
//{$IF Compilerversion >= 21}
   Result := Pointer(T);
//{$ELSE}
//   Result := PPointer(@T)^;
//{$IFEND}
end;

procedure GetMemForT(var T; Size: integer); inline;
begin
  GetMem(Pointer(T), Size);
end;
{$endif}

// InitializeWithDefaultFormatSettings
//
procedure InitializeWithDefaultFormatSettings(var fmt : TFormatSettings);
begin
   {$ifdef DELPHI_XE_PLUS}
   fmt := System.SysUtils.FormatSettings;
   {$else}
   fmt := System.SysUtils.TFormatSettings((@CurrencyString{%H-})^);
   {$endif}
end;

// GetAsString
//
function TModuleVersion.GetAsString : String;
begin
   Result := Format('%d.%d.%d.%d', [Major, Minor, Release, Build]);
end;

// SetAsString
//
procedure TModuleVersion.SetAsString(const s : String);
begin
   Self := Default(TModuleVersion);
   var parts := TStringList.Create('"', '.');
   try
      parts.DelimitedText := s;
      if parts.Count > 0 then major := StrToIntDef(parts[0], 0);
      if parts.Count > 1 then minor := StrToIntDef(parts[1], 0);
      if parts.Count > 2 then release := StrToIntDef(parts[2], 0);
      if parts.Count > 3 then build := StrToIntDef(parts[3], 0);
   finally
      parts.Free;
   end;
end;

{$ifdef WINDOWS}
// Adapted from Ian Boyd code published in
// http://stackoverflow.com/questions/10854958/how-to-get-version-of-running-executable
function GetModuleVersion(instance : THandle; var version : TModuleVersion) : Boolean;
var
   fileInformation : PVSFIXEDFILEINFO;
   verlen : Cardinal;
   rs : TResourceStream;
   m : TMemoryStream;
   resource : HRSRC;
begin
   Result:=False;

   // Workaround bug in Delphi if resource doesn't exist
   resource:=FindResource(instance, PChar(1), RT_VERSION);
   if resource=0 then Exit;

   m:=TMemoryStream.Create;
   try
      rs:=TResourceStream.CreateFromID(instance, 1, RT_VERSION);
      try
         m.CopyFrom(rs, rs.Size);
      finally
         rs.Free;
      end;

      m.Position:=0;
      if VerQueryValue(m.Memory, '\', Pointer(fileInformation), verlen) then begin
         version.Major := fileInformation.dwFileVersionMS shr 16;
         version.Minor := fileInformation.dwFileVersionMS and $FFFF;
         version.Release := fileInformation.dwFileVersionLS shr 16;
         version.Build := fileInformation.dwFileVersionLS and $FFFF;
         Result := True;
      end;
   finally
      m.Free;
   end;
end;

// GetApplicationVersion
//
var
   vApplicationVersion : TModuleVersion;
   vApplicationVersionRetrieved : Integer;
function GetApplicationVersion(var version : TModuleVersion) : Boolean;
begin
   if vApplicationVersionRetrieved = 0 then begin
      if GetModuleVersion(HInstance, vApplicationVersion) then
         vApplicationVersionRetrieved := 1
      else vApplicationVersionRetrieved := -1;
   end;
   Result := (vApplicationVersionRetrieved = 1);
   if Result then
      version := vApplicationVersion;
end;
{$else}
function GetModuleVersion(instance : THandle; var version : TModuleVersion) : Boolean;
begin
   Result := False;
end;
function GetApplicationVersion(var version : TModuleVersion) : Boolean;
begin
   Result := False;
end;
{$endif}

// ApplicationVersion
//
function ApplicationVersion(const options : TApplicationVersionOptions = [ avoBitness ]) : String;
begin
   {$ifdef WINDOWS}
      var version : TModuleVersion;
      {$ifdef WIN64}
      if GetApplicationVersion(version) then
         Result := version.AsString
      else Result := '?.?.?.?';
      {$else}
      if GetApplicationVersion(version) then
         Result := version.AsString
      else Result := '?.?.?.?';
      {$endif}
   {$else}
      // No version information available under Linux
      Result := 'linux build';
   {$endif}
   {$ifdef CPUX64}
   if avoBitness in options then
      Result := Result + ' 64bit';
   {$endif}
   {$ifdef CPUX86}
   if avoBitness in options then
      Result := Result + ' 32bit';
   {$endif}
end;

// Win64CPUFeatures
//
{$if Defined(WIN64_ASM)}
function GetCPUID_ECX : Cardinal;
asm
   mov   r10, rbx
   mov   eax, 1
   cpuid
   mov   eax, ecx
   mov   rbx, r10
end;

function GetCPUID7_EBX : Cardinal;
asm
   mov   r10, rbx
   mov   eax, 7
   xor   ecx, ecx
   cpuid
   mov   eax, ebx
   mov   rbx, r10
end;

var
   vCPUFeatures : TCPUFeatures;
function Win64CPUFeatures : TCPUFeatures;
begin
   if not (cpuFeaturesRead in vCPUFeatures) then begin
      var ecx := GetCPUID_ECX;
      if ((ecx shr 19) and 1) <> 0 then Include(vCPUFeatures, cpuSSE41);
      if ((ecx shr 12) and 1) <> 0 then Include(vCPUFeatures, cpuFMA);
      if ((ecx shr 28) and 1) <> 0 then begin
         Include(vCPUFeatures, cpuAVX);
         if (GetCPUID7_EBX shr 5) <> 0 then
            Include(vCPUFeatures, cpuAVX2);
      end;
      Include(vCPUFeatures, cpuFeaturesRead);
   end;
   Result := vCPUFeatures;
end;
{$else}
function Win64CPUFeatures : TCPUFeatures;
begin
   Result := [];
end;
{$endif}

// Win64SSE41Supported
//
function Win64SSE41Supported : Boolean;
begin
   Result := cpuSSE41 in WIN64CPUFeatures;
end;

// Win64FMASupported
//
function Win64FMASupported : Boolean;
begin
   Result := cpuFMA in WIN64CPUFeatures;
end;

// Win64AVX2Supported
//
function Win64AVX2Supported : Boolean;
begin
   Result := cpuAVX2 in WIN64CPUFeatures;
end;

// ------------------
// ------------------ TdwsCriticalSection ------------------
// ------------------

{$ifndef UNIX}
// Create
//
constructor TdwsCriticalSection.Create;
begin
   InitializeCriticalSection(FCS);
end;

// Destroy
//
destructor TdwsCriticalSection.Destroy;
begin
   DeleteCriticalSection(FCS);
end;

// Enter
//
procedure TdwsCriticalSection.Enter;
begin
   EnterCriticalSection(FCS);
end;

// Leave
//
procedure TdwsCriticalSection.Leave;
begin
   LeaveCriticalSection(FCS);
end;

// TryEnter
//
function TdwsCriticalSection.TryEnter : Boolean;
begin
   Result:=TryEnterCriticalSection(FCS);
end;
{$endif}

// TryEnterOrTimeout
//
function TdwsCriticalSection.TryEnterOrTimeout(delayMSec : Integer) : Boolean;

   function EnterWait : Boolean;
   var
      timeout : Int64;
   begin
      timeout := GetSystemMilliseconds + delayMSec;
      repeat
         Sleep(10);
         Result := TryEnter;
      until Result or (GetSystemMilliseconds > timeout);
   end;

begin
   Result := TryEnter;
   if not Result then
      Result := EnterWait;
end;

// ------------------
// ------------------ TPath ------------------
// ------------------

// GetTempPath
//
class function TPath.GetTempPath : String;
{$IFDEF WINDOWS}
var
   tempPath : array [0..MAX_PATH] of WideChar; // Buf sizes are MAX_PATH+1
begin
   if Winapi.Windows.GetTempPath(MAX_PATH, @tempPath[0])=0 then begin
      tempPath[1]:='.'; // Current directory
      tempPath[2]:=#0;
   end;
   Result:=tempPath;
{$ELSE}
begin
   Result := System.IOUTils.TPath.GetTempPath;
{$ENDIF}
end;

// GetTempFileName
//
class function TPath.GetTempFileName : String;
{$IFDEF WINDOWS}
var
   tempPath, tempFileName : array [0..MAX_PATH] of WideChar; // Buf sizes are MAX_PATH+1
begin
   if Winapi.Windows.GetTempPath(MAX_PATH, @tempPath[0])=0 then begin
      tempPath[1]:='.'; // Current directory
      tempPath[2]:=#0;
   end;
   if Winapi.Windows.GetTempFileNameW(@tempPath[0], 'DWS', 0, tempFileName)=0 then
      RaiseLastOSError; // should never happen
   Result:=tempFileName;
{$ELSE}
begin
   Result := System.IOUTils.TPath.GetTempFileName;
{$ENDIF}
end;

// ------------------
// ------------------ TFile ------------------
// ------------------

// ReadAllBytes
//
class function TFile.ReadAllBytes(const filename : String) : TBytes;
begin
   Result := LoadDataFromFile(fileName)
end;

// ------------------
// ------------------ TdwsThread ------------------
// ------------------

{$IFNDEF FPC}
{$IFDEF VER200}

// Start
//
procedure TdwsThread.Start;
begin
   Resume;
end;

{$ENDIF}
{$ENDIF}

// SetTimeCriticalPriority
//
procedure TdwsThread.SetTimeCriticalPriority;
begin
   {$ifdef WINDOWS}
   // only supported in Windows
   Priority := tpTimeCritical;
   {$endif}
end;

// ------------------
// ------------------ TMultiReadSingleWrite ------------------
// ------------------

{$ifndef SRW_FALLBACK}
procedure TMultiReadSingleWrite.BeginRead;
begin
   AcquireSRWLockShared(FSRWLock);
end;

function TMultiReadSingleWrite.TryBeginRead : Boolean;
begin
   Result:=TryAcquireSRWLockShared(FSRWLock);
end;

procedure TMultiReadSingleWrite.EndRead;
begin
   ReleaseSRWLockShared(FSRWLock)
end;

procedure TMultiReadSingleWrite.BeginWrite;
begin
   AcquireSRWLockExclusive(FSRWLock);
end;

function TMultiReadSingleWrite.TryBeginWrite : Boolean;
begin
   Result:=TryAcquireSRWLockExclusive(FSRWLock);
end;

procedure TMultiReadSingleWrite.EndWrite;
begin
   ReleaseSRWLockExclusive(FSRWLock)
end;
{$else} // SRW_FALLBACK
procedure TMultiReadSingleWrite.BeginRead;
begin
   FLock.BeginRead;
end;

function TMultiReadSingleWrite.TryBeginRead : Boolean;
begin
   Result:=FLock.TryBeginRead;
end;

procedure TMultiReadSingleWrite.EndRead;
begin
   FLock.EndRead;
end;

procedure TMultiReadSingleWrite.BeginWrite;
begin
   FLock.BeginWrite;
end;

function TMultiReadSingleWrite.TryBeginWrite : Boolean;
begin
   Result:=FLock.TryBeginWrite;
end;

procedure TMultiReadSingleWrite.EndWrite;
begin
   FLock.EndWrite;
end;
{$endif}

function TMultiReadSingleWrite.State : TMultiReadSingleWriteState;
begin
   // Attempt to guess the state of the lock without making assumptions
   // about implementation details
   // This is only for diagnosing locking issues
   if TryBeginWrite then begin
      EndWrite;
      Result:=mrswUnlocked;
   end else if TryBeginRead then begin
      EndRead;
      Result:=mrswReadLock;
   end else begin
      Result:=mrswWriteLock;
   end;
end;

// ------------------
// ------------------ TTimerTimeout ------------------
// ------------------

{$ifdef WINDOWS}
   {$ifdef FPC}
   type TWaitOrTimerCallback = procedure (Context: Pointer; Success: Boolean); stdcall;
   function CreateTimerQueueTimer(out phNewTimer: THandle;
      TimerQueue: THandle; CallBack: TWaitOrTimerCallback;
      Parameter: Pointer; DueTime: DWORD; Period: DWORD; Flags: ULONG): BOOL; stdcall; external 'kernel32.dll';
   function DeleteTimerQueueTimer(TimerQueue: THandle;
      Timer: THandle; CompletionEvent: THandle): BOOL; stdcall; external 'kernel32.dll';
   const
      WT_EXECUTEDEFAULT       = ULONG($00000000);
      WT_EXECUTEONLYONCE      = ULONG($00000008);
      WT_EXECUTELONGFUNCTION  = ULONG($00000010);
   {$endif}
{$endif}

// TTimerTimeoutCallBack
//
procedure TTimerTimeoutCallBack(Context: Pointer; {%H-}Success: Boolean); stdcall;
{$ifdef WINDOWS}
var
   tt : TTimerTimeout;
   event : TTimerEvent;
begin
   tt := TTimerTimeout(Context);
   tt._AddRef;
   try
      event := tt.FOnTimer;
      if Assigned(event) then
         event();
      DeleteTimerQueueTimer(0, tt.FTimer, 0);
      tt.FTimer := 0;
   finally
      tt._Release;
   end;
end;
{$else}
var
   timer      : TdwsXTimer;
   timerQueue : TdwsXTimerQueue;
begin
   timer := TdwsXTimer(Context);
   if Assigned(timer.Event) then
     timer.Event();

   timerQueue := TdwsXTimerQueue.Create(false);
   try
      timerQueue.Release(timer.Handle);
   finally
      timerQueue.Free;
   end;
end;
{$endif}

// Create
//
class function TTimerTimeout.Create(delayMSec : Cardinal; onTimer : TTimerEvent) : ITimer;
var
   obj : TTimerTimeout;
   {$ifdef UNIX}
   timerQueue : TdwsXTimerQueue;
   {$endif}
begin
   obj := TTimerTimeout(inherited Create);
   Result := obj;
   obj.FOnTimer := onTimer;
   {$ifdef WINDOWS}
   CreateTimerQueueTimer(obj.FTimer, 0, TTimerTimeoutCallBack, obj,
                         delayMSec, 0,
                         WT_EXECUTEDEFAULT or WT_EXECUTELONGFUNCTION or WT_EXECUTEONLYONCE);
   {$else}
   timerQueue := TdwsXTimerQueue.Create(false);
   try
      obj.FTimer := timerQueue.Add(delayMSec, onTimer, TTimerTimeoutCallBack);
   finally
      timerQueue.Free;
   end;
   {$endif}
end;

// Destroy
//
destructor TTimerTimeout.Destroy;
begin
   Cancel;
   inherited;
end;

// Cancel
//
procedure TTimerTimeout.Cancel;
{$ifdef UNIX}
var
   timerQueue : TdwsXTimerQueue;
{$endif}
begin
   FOnTimer := nil;
   if FTimer = 0 then Exit;
   {$ifdef WINDOWS}
   DeleteTimerQueueTimer(0, FTimer, INVALID_HANDLE_VALUE);
   {$else}
   timerQueue := TdwsXTimerQueue.Create(false);
   try
      timerQueue.ReleaseAll;
   finally
      timerQueue.Free;
   end;
   {$endif}
   FTimer:=0;
end;

// ------------------
// ------------------ TdwsDateTime ------------------
// ------------------

// Now
//
class function TdwsDateTime.Now : TdwsDateTime;
{$ifdef WINDOWS}
var
   fileTime : TFileTime;
begin
   GetSystemTimeAsFileTime(fileTime);
   Result.AsFileTime := fileTime;
end;
{$else}
begin
   Result.AsLocalDateTime := System.SysUtils.Now;
end;
{$endif}

// FromLocalDateTime
//
class function TdwsDateTime.FromLocalDateTime(const dt : TDateTime) : TdwsDateTime;
begin
   Result.AsLocalDateTime := dt;
end;

// FromUTCDateTime
//
class function TdwsDateTime.FromUTCDateTime(const dt : TDateTime) : TdwsDateTime;
begin
   Result.AsUTCDateTime := dt;
end;

// Clear
//
procedure TdwsDateTime.Clear;
begin
   FValue := 0;
end;

// IsZero
//
function TdwsDateTime.IsZero : Boolean;
begin
   Result := FValue = 0;
end;

// IsNotZero
//
function TdwsDateTime.IsNotZero : Boolean;
begin
   Result := FValue <> 0;
end;

// Equal
//
class operator TdwsDateTime.Equal(const a, b : TdwsDateTime) : Boolean;
begin
   Result := a.FValue = b.FValue;
end;

// NotEqual
//
class operator TdwsDateTime.NotEqual(const a, b : TdwsDateTime) : Boolean;
begin
   Result := a.FValue <> b.FValue;
end;

// GreaterThan
//
class operator TdwsDateTime.GreaterThan(const a, b : TdwsDateTime) : Boolean;
begin
   Result := a.FValue > b.FValue;
end;

// GreaterThanOrEqual
//
class operator TdwsDateTime.GreaterThanOrEqual(const a, b : TdwsDateTime) : Boolean;
begin
   Result := a.FValue >= b.FValue;
end;

// LessThan
//
class operator TdwsDateTime.LessThan(const a, b : TdwsDateTime) : Boolean;
begin
   Result := a.FValue < b.FValue;
end;

// LessThanOrEqual
//
class operator TdwsDateTime.LessThanOrEqual(const a, b : TdwsDateTime) : Boolean;
begin
   Result := a.FValue <= b.FValue;
end;

// MillisecondsAheadOf
//
function TdwsDateTime.MillisecondsAheadOf(const d : TdwsDateTime) : Int64;
begin
   Result := FValue - d.FValue;
end;

// IncMilliseconds
//
procedure TdwsDateTime.IncMilliseconds(const msec : Int64);
begin
   Inc(FValue, msec);
end;

const
   cFileTime_UnixTimeStart : Int64 = $019DB1DED53E8000; // January 1, 1970 (start of Unix epoch) in "ticks"
   cFileTime_TicksPerMillisecond : Int64 = 10000;       // a tick is 100ns

// SetAsFileTime
//
procedure TdwsDateTime.SetAsFileTime(const val : TFileTime);
var
   temp : TdwsLargeInteger;
begin
   temp.LowPart := val.dwLowDateTime;
   temp.HighPart := val.dwHighDateTime;
   FValue := (temp.QuadPart - cFileTime_UnixTimeStart) div cFileTime_TicksPerMillisecond;
end;

// GetAsDosDateTime
//
function TdwsDateTime.GetAsDosDateTime : Integer;
{$IFDEF WINDOWS}
var
   fileTime : TFileTime;
   dosTime : LongRec;
begin
   fileTime := AsFileTime;
   FileTimeToDosDateTime(fileTime, dosTime.Hi, dosTime.Lo);
   Result := Integer(dosTime);
end;
{$ELSE}
begin
   Result := Integer(System.SysUtils.DateTimeToFileDate(Self.AsLocalDateTime));
end;
{$ENDIF}

// GetAsFileTime
//
function TdwsDateTime.GetAsFileTime : TFileTime;
var
   temp : TdwsLargeInteger;
begin
   temp.QuadPart := (FValue * cFileTime_TicksPerMillisecond) + cFileTime_UnixTimeStart;
   Result.dwLowDateTime := temp.LowPart;
   Result.dwHighDateTime := temp.HighPart;
end;

// GetAsUnixTime
//
function TdwsDateTime.GetAsUnixTime : Int64;
begin
   Result := FValue div 1000;
end;

// SetAsUnixTime
//
procedure TdwsDateTime.SetAsUnixTime(const val : Int64);
begin
   FValue := val * 1000;
end;

// GetAsLocalDateTime
//
function TdwsDateTime.GetAsLocalDateTime : TDateTime;
begin
   Result := UTCDateTimeToLocalDateTime(AsUTCDateTime);
end;

// SetAsLocalDateTime
//
procedure TdwsDateTime.SetAsLocalDateTime(const val : TDateTime);
begin
   AsUTCDateTime := LocalDateTimeToUTCDateTime(val);
end;

// GetAsUTCDateTime
//
function TdwsDateTime.GetAsUTCDateTime : TDateTime;
begin
   Result := FValue / 864e5 + 25569;
end;

// SetAsUTCDateTime
//
procedure TdwsDateTime.SetAsUTCDateTime(const val : TDateTime);
begin
   FValue := Round((val - 25569) * 864e5);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   InitializeGetSystemMilliseconds;

   {$IFDEF UNIX}
      {$IFNDEF FPC}
         {$IFDEF POSIXSYSLOG}
         Posix.Syslog.openlog(nil, LOG_PID or LOG_NDELAY, LOG_DAEMON);
         {$ENDIF}
      {$ENDIF}
   {$ENDIF}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
finalization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   {$IFDEF UNIX}
      {$IFNDEF FPC}
         {$IFDEF POSIXSYSLOG}
         Posix.Syslog.closelog();
         {$ENDIF}
      {$ENDIF}
   {$ENDIF}

end.
