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
   {$DEFINE VER200}
{$ENDIF}

interface

uses
   Classes, SysUtils, Types, Masks,
   {$IFDEF FPC}
      {$IFDEF Windows}
         Windows
      {$ELSE}
         LCLIntf
      {$ENDIF}
   {$ELSE}
      Windows
      {$IFNDEF VER200}, IOUtils{$ENDIF}
   {$ENDIF}
   ;

const
{$IFDEF UNIX}
   cLineTerminator  = #10;
{$ELSE}
   cLineTerminator  = #13#10;
{$ENDIF}

   // following is missing from D2010
   INVALID_HANDLE_VALUE = DWORD(-1);

type
   // see http://delphitools.info/2011/11/30/fixing-tcriticalsection/
   {$HINTS OFF}
   TFixedCriticalSection = class
      private
         FDummy : array [0..95-SizeOf(TRTLCRiticalSection)-2*SizeOf(Pointer)] of Byte;
         FCS : TRTLCriticalSection;

      public
         constructor Create;
         destructor Destroy; override;

         procedure Enter;
         procedure Leave;

         function TryEnter : Boolean;
   end;

   IMultiReadSingleWrite = interface
      procedure BeginRead;
      function  TryBeginRead : Boolean;
      procedure EndRead;

      procedure BeginWrite;
      function  TryBeginWrite : Boolean;
      procedure EndWrite;
   end;

   TMultiReadSingleWriteState = (mrswUnlocked, mrswReadLock, mrswWriteLock);

   TMultiReadSingleWrite = class (TInterfacedObject, IMultiReadSingleWrite)
      private
         FCS : TFixedCriticalSection; // used as fallback
         FSRWLock : Pointer;
         FDummy : array [0..95-4*SizeOf(Pointer)] of Byte; // padding

      public
         constructor Create(forceFallBack : Boolean = False);
         destructor Destroy; override;

         procedure BeginRead;
         function  TryBeginRead : Boolean;
         procedure EndRead;

         procedure BeginWrite;
         function  TryBeginWrite : Boolean;
         procedure EndWrite;

         // use for diagnostic only
         function State : TMultiReadSingleWriteState;
   end;
   {$HINTS ON}

procedure SetDecimalSeparator(c : Char);
function GetDecimalSeparator : Char;

type
   TCollectFileProgressEvent = procedure (const directory : String; var skipScan : Boolean) of object;

procedure CollectFiles(const directory, fileMask : UnicodeString;
                       list : TStrings; recurseSubdirectories: Boolean = False;
                       onProgress : TCollectFileProgressEvent = nil);

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

   RawByteString = UnicodeString;

   PNativeInt = ^NativeInt;
   PUInt64 = ^UInt64;
   {$ENDIF}

   TPath = class
      class function GetTempFileName : UnicodeString; static;
   end;

   TFile = class
      class function ReadAllBytes(const filename : UnicodeString) : TBytes; static;
   end;

   TdwsThread = class (TThread)
      {$IFNDEF FPC}
      {$IFDEF VER200}
      procedure Start;
      {$ENDIF}
      {$ENDIF}
   end;

// 64bit system clock reference in milliseconds since boot
function GetSystemMilliseconds : Int64;
function UTCDateTime : TDateTime;
// UTC = LocalTime + Bias
function LocalTimeBias : TDateTime;
procedure SystemSleep(msec : Integer);

{$ifndef FPC}
function UnicodeFormat(const fmt : UnicodeString; const args : array of const) : UnicodeString;
function UnicodeCompareStr(const S1, S2 : UnicodeString) : Integer; inline;
{$endif}

function AnsiCompareText(const S1, S2 : UnicodeString) : Integer;
function AnsiCompareStr(const S1, S2 : UnicodeString) : Integer;

function UnicodeComparePChars(p1 : PWideChar; n1 : Integer; p2 : PWideChar; n2 : Integer) : Integer; overload;
function UnicodeComparePChars(p1, p2 : PWideChar; n : Integer) : Integer; overload;

function UnicodeLowerCase(const s : UnicodeString) : UnicodeString;
function UnicodeUpperCase(const s : UnicodeString) : UnicodeString;

function ASCIICompareText(const s1, s2 : UnicodeString) : Integer; inline;
function ASCIISameText(const s1, s2 : UnicodeString) : Boolean; inline;

function InterlockedIncrement(var val : Integer) : Integer; overload; {$IFDEF PUREPASCAL} inline; {$endif}
function InterlockedDecrement(var val : Integer) : Integer; {$IFDEF PUREPASCAL} inline; {$endif}

procedure FastInterlockedIncrement(var val : Integer); {$IFDEF PUREPASCAL} inline; {$endif}
procedure FastInterlockedDecrement(var val : Integer); {$IFDEF PUREPASCAL} inline; {$endif}

function InterlockedExchangePointer(var target : Pointer; val : Pointer) : Pointer; {$IFDEF PUREPASCAL} inline; {$endif}

function InterlockedCompareExchangePointer(var destination : Pointer; exchange, comparand : Pointer) : Pointer; {$IFDEF PUREPASCAL} inline; {$endif}

procedure SetThreadName(const threadName : PAnsiChar; threadID : Cardinal = Cardinal(-1));

procedure OutputDebugString(const msg : UnicodeString);

procedure WriteToOSEventLog(const logName, logCaption, logDetails : UnicodeString;
                            const logRawData : RawByteString = ''); overload;

function TryTextToFloat(const s : PWideChar; var value : Extended;
                        const formatSettings : TFormatSettings) : Boolean; {$ifndef FPC} inline; {$endif}

{$ifdef FPC}
procedure VarCopy(out dest : Variant; const src : Variant); inline;
{$else}
function VarToUnicodeStr(const v : Variant) : UnicodeString; inline;
{$endif}

function RawByteStringToBytes(const buf : RawByteString) : TBytes;
function BytesToRawByteString(const buf : TBytes; startIndex : Integer = 0) : RawByteString;

function LoadDataFromFile(const fileName : UnicodeString) : TBytes;
procedure SaveDataToFile(const fileName : UnicodeString; const data : TBytes);

function LoadRawBytesFromFile(const fileName : UnicodeString) : RawByteString;
procedure SaveRawBytesToFile(const fileName : UnicodeString; const data : RawByteString);

function LoadTextFromBuffer(const buf : TBytes) : UnicodeString;
function LoadTextFromRawBytes(const buf : RawByteString) : UnicodeString;
function LoadTextFromStream(aStream : TStream) : UnicodeString;
function LoadTextFromFile(const fileName : UnicodeString) : UnicodeString;
procedure SaveTextToUTF8File(const fileName, text : UnicodeString);
procedure AppendTextToUTF8File(const fileName : String; const text : UTF8String);
function OpenFileForSequentialReadOnly(const fileName : UnicodeString) : THandle;
function OpenFileForSequentialWriteOnly(const fileName : UnicodeString) : THandle;
procedure CloseFileHandle(hFile : THandle);
function FileWrite(hFile : THandle; buffer : Pointer; byteCount : Integer) : Cardinal;
function FileCopy(const existing, new : UnicodeString; failIfExists : Boolean) : Boolean;
function FileMove(const existing, new : UnicodeString) : Boolean;
function FileDelete(const fileName : String) : Boolean;
function FileRename(const oldName, newName : String) : Boolean;
function FileSize(const name : String) : Int64;
function FileDateTime(const name : String) : TDateTime;
function DeleteDirectory(const path : String) : Boolean;

function DirectSet8087CW(newValue : Word) : Word; register;
function DirectSetMXCSR(newValue : Word) : Word; register;

// Generics helper functions to handle Delphi 2009 issues - HV
function TtoObject(const T): TObject; inline;
function TtoPointer(const T): Pointer; inline;
procedure GetMemForT(var T; Size: integer); inline;

procedure InitializeWithDefaultFormatSettings(var fmt : TFormatSettings);

// Functions missing in D2009
{$ifdef FPC}
   {$define NEED_FindDelimiter}
{$else}
   {$IF RTLVersion < 21}{$define NEED_FindDelimiter}{$ifend}
{$endif}
{$ifdef NEED_FindDelimiter}
function FindDelimiter(const Delimiters, S: string; StartIdx: Integer = 1): Integer;
{$endif}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$ifndef FPC}
uses Variants;
{$endif}

{$ifdef FPC}
type
   TFindExInfoLevels = FINDEX_INFO_LEVELS;
{$endif}

// GetSystemTimeMilliseconds
//
function GetSystemTimeMilliseconds : Int64; stdcall;
var
   fileTime : TFileTime;
begin
{$IFDEF WINDOWS}
   GetSystemTimeAsFileTime(fileTime);
   Result:=Round(PInt64(@fileTime)^*1e-4); // 181
{$ELSE}
   Not yet implemented!
{$ENDIF}
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
   Result:=vGetSystemMilliseconds;
{$endif}
end;

// InitializeGetSystemMilliseconds
//
procedure InitializeGetSystemMilliseconds;
var
   h : THandle;
begin
   {$IFDEF WINDOWS}
   h:=LoadLibrary('kernel32.dll');
   vGetSystemMilliseconds:=GetProcAddress(h, 'GetTickCount64');
   {$ENDIF}
   if not Assigned(vGetSystemMilliseconds) then
      vGetSystemMilliseconds:=@GetSystemTimeMilliseconds;
end;

// UTCDateTime
//
function UTCDateTime : TDateTime;
var
   systemTime : TSystemTime;
begin
{$IFDEF Windows}
   FillChar(systemTime, SizeOf(systemTime), 0);
   GetSystemTime(systemTime);
   with systemTime do
      Result:= EncodeDate(wYear, wMonth, wDay)
              +EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
{$ELSE}
   Not yet implemented!
{$ENDIF}
end;

// LocalTimeBias
//
function LocalTimeBias : TDateTime;
{$IFDEF Windows}
var
   tzInfo : TTimeZoneInformation;
begin
   GetTimeZoneInformation(tzInfo);
   Result:=tzInfo.Bias*(1/24/60);
{$ELSE}
begin
   Result:=0; // TODO!
{$ENDIF}
end;

// SystemSleep
//
procedure SystemSleep(msec : Integer);
begin
   if msec>=0 then
      Windows.Sleep(msec);
end;

{$ifndef FPC}

// UnicodeFormat
//
function UnicodeFormat(const fmt : UnicodeString; const args : array of const) : UnicodeString;
begin
   Result:=Format(fmt, args);
end;

// UnicodeCompareStr
//
function UnicodeCompareStr(const S1, S2 : UnicodeString) : Integer;
begin
   Result:=CompareStr(S1, S2);
end;

{$endif} // FPC

// AnsiCompareText
//
function AnsiCompareText(const S1, S2: UnicodeString) : Integer;
begin
   {$ifdef FPC}
   Result:=widestringmanager.CompareTextUnicodeStringProc(s1,s2);
   {$else}
   Result:=SysUtils.AnsiCompareText(S1, S2);
   {$endif}
end;

// AnsiCompareStr
//
function AnsiCompareStr(const S1, S2: UnicodeString) : Integer;
begin
   {$ifdef FPC}
   Result:=widestringmanager.CompareUnicodeStringProc(s1,s2);
   {$else}
   Result:=SysUtils.AnsiCompareStr(S1, S2);
   {$endif}
end;

// UnicodeComparePChars
//
function UnicodeComparePChars(p1 : PWideChar; n1 : Integer; p2 : PWideChar; n2 : Integer) : Integer;
const
   CSTR_EQUAL = 2;
begin
   Result:=CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, p1, n1, p2, n2)-CSTR_EQUAL;
end;

// UnicodeComparePChars
//
function UnicodeComparePChars(p1, p2 : PWideChar; n : Integer) : Integer; overload;
const
   CSTR_EQUAL = 2;
begin
   Result:=CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, p1, n, p2, n)-CSTR_EQUAL;
end;

// UnicodeLowerCase
//
function UnicodeLowerCase(const s : UnicodeString) : UnicodeString;
begin
   if s<>'' then begin
      Result:=s;
      UniqueString(Result);
      Windows.CharLowerBuffW(PWideChar(Pointer(Result)), Length(Result));
   end else Result:=s;
end;

// UnicodeUpperCase
//
function UnicodeUpperCase(const s : UnicodeString) : UnicodeString;
begin
   if s<>'' then begin
      Result:=s;
      UniqueString(Result);
      Windows.CharUpperBuffW(PWideChar(Pointer(Result)), Length(Result));
   end else Result:=s;
end;

// ASCIICompareText
//
function ASCIICompareText(const s1, s2 : UnicodeString) : Integer; inline;
begin
   {$ifdef FPC}
   Result:=CompareText(UTF8Encode(s1), UTF8Encode(s2));
   {$else}
   Result:=CompareText(s1, s2);
   {$endif}
end;

// ASCIISameText
//
function ASCIISameText(const s1, s2 : UnicodeString) : Boolean; inline;
begin
   {$ifdef FPC}
   Result:=(ASCIICompareText(s1, s2)=0);
   {$else}
   Result:=SameText(s1, s2);
   {$endif}
end;

// InterlockedIncrement
//
function InterlockedIncrement(var val : Integer) : Integer;
{$ifndef WIN32_ASM}
begin
   Result:=Windows.InterlockedIncrement(val);
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
   Result:=Windows.InterlockedDecrement(val);
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
   InterlockedIncrement(val);
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
   InterlockedDecrement(val);
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
   Result:=InterlockedExchangePointer(target, val);
   {$else}
   Result:=Windows.InterlockedExchangePointer(target, val);
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
   Result:=Windows.InterlockedCompareExchangePointer(destination, exchange, comparand);
end;

// SetThreadName
//
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

// OutputDebugString
//
procedure OutputDebugString(const msg : UnicodeString);
begin
   Windows.OutputDebugStringW(PWideChar(msg));
end;

// WriteToOSEventLog
//
procedure WriteToOSEventLog(const logName, logCaption, logDetails : UnicodeString;
                            const logRawData : RawByteString = '');
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

// SetDecimalSeparator
//
procedure SetDecimalSeparator(c : Char);
begin
   {$IFDEF FPC}
      FormatSettings.DecimalSeparator:=c;
   {$ELSE}
      {$IF CompilerVersion >= 22.0}
      FormatSettings.DecimalSeparator:=c;
      {$ELSE}
      DecimalSeparator:=c;
      {$IFEND}
   {$ENDIF}
end;

// GetDecimalSeparator
//
function GetDecimalSeparator : Char;
begin
   {$IFDEF FPC}
      Result:=FormatSettings.DecimalSeparator;
   {$ELSE}
      {$IF CompilerVersion >= 22.0}
      Result:=FormatSettings.DecimalSeparator;
      {$ELSE}
      Result:=DecimalSeparator;
      {$IFEND}
   {$ENDIF}
end;

// CollectFiles
//
type
   TFindDataRec = record
      Handle : THandle;
      Data : TWin32FindData;
   end;

// CollectFilesMasked
//
procedure CollectFilesMasked(const directory : UnicodeString;
                             mask : TMask; list : TStrings;
                             recurseSubdirectories: Boolean = False;
                             onProgress : TCollectFileProgressEvent = nil);
const
   // contant defined in Windows.pas is incorrect
   FindExInfoBasic = 1;
var
   searchRec : TFindDataRec;
   infoLevel : TFindexInfoLevels;
   fileName : String;
   skipScan : Boolean;
begin
   // 6.1 required for FindExInfoBasic (Win 2008 R2 or Win 7)
   if ((Win32MajorVersion shl 8) or Win32MinorVersion)>=$601 then
      infoLevel:=TFindexInfoLevels(FindExInfoBasic)
   else infoLevel:=FindExInfoStandard;

   if Assigned(onProgress) then begin
      skipScan:=False;
      onProgress(directory, skipScan);
      if skipScan then exit;
   end;

   fileName:=directory+'*';
   searchRec.Handle:=FindFirstFileEx(PChar(Pointer(fileName)), infoLevel,
                                     @searchRec.Data, FINDEX_SEARCH_OPS.FindExSearchNameMatch,
                                     nil, 0);
   if searchRec.Handle<>INVALID_HANDLE_VALUE then begin
      repeat
         if (searchRec.Data.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)=0 then begin
            // check file against mask
            fileName:=searchRec.Data.cFileName;
            if mask.Matches(fileName) then begin
               fileName:=directory+fileName;
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
            CollectFilesMasked(fileName, mask, list, True, onProgress);
         end;
      until not FindNextFile(searchRec.Handle, searchRec.Data);
      Windows.FindClose(searchRec.Handle);
   end;
end;

// CollectFiles
//
procedure CollectFiles(const directory, fileMask : UnicodeString; list : TStrings;
                       recurseSubdirectories: Boolean = False;
                       onProgress : TCollectFileProgressEvent = nil);
var
   mask : TMask;
begin
   // Windows can match 3 character filters with old DOS filenames
   // Mask confirmation is necessary
   mask:=TMask.Create(fileMask);
   try
      CollectFilesMasked(IncludeTrailingPathDelimiter(directory), mask,
                         list, recurseSubdirectories, onProgress);
   finally
      mask.Free;
   end;
end;

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
function VarToUnicodeStr(const v : Variant) : UnicodeString; inline;
begin
   Result:=VarToStr(v);
end;
{$endif FPC}

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
      System.Move(buf[startIndex], Result[1], n);
   end;
end;

// TryTextToFloat
//
function TryTextToFloat(const s : PWideChar; var value : Extended; const formatSettings : TFormatSettings) : Boolean;
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
      encoding:=nil;
      n:=TEncoding.GetBufferEncoding(buf, encoding);
      if n=0 then
         encoding:=TEncoding.UTF8;
      if encoding=TEncoding.UTF8 then begin
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
   n:=aStream.Size-aStream.Position;
   SetLength(buf, n);
   aStream.Read(buf[0], n);
   Result:=LoadTextFromBuffer(buf);
end;

// LoadTextFromFile
//
function LoadTextFromFile(const fileName : UnicodeString) : UnicodeString;
var
   buf : TBytes;
begin
   buf:=LoadDataFromFile(fileName);
   Result:=LoadTextFromBuffer(buf);
end;

// LoadDataFromFile
//
function LoadDataFromFile(const fileName : UnicodeString) : TBytes;
const
   INVALID_FILE_SIZE = DWORD($FFFFFFFF);
var
   hFile : THandle;
   n, nRead : Cardinal;
begin
   if fileName='' then Exit(nil);
   hFile:=OpenFileForSequentialReadOnly(fileName);
   if hFile=INVALID_HANDLE_VALUE then Exit(nil);
   try
      n:=GetFileSize(hFile, nil);
      if n=INVALID_FILE_SIZE then
         RaiseLastOSError;
      if n>0 then begin
         SetLength(Result, n);
         if not ReadFile(hFile, Result[0], n, nRead, nil) then
            RaiseLastOSError;
      end else Result:=nil;
   finally
      CloseHandle(hFile);
   end;
end;

// SaveDataToFile
//
procedure SaveDataToFile(const fileName : UnicodeString; const data : TBytes);
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
      CloseHandle(hFile);
   end;
end;

// LoadRawBytesFromFile
//
function LoadRawBytesFromFile(const fileName : UnicodeString) : RawByteString;
const
   INVALID_FILE_SIZE = DWORD($FFFFFFFF);
var
   hFile : THandle;
   n, nRead : Cardinal;
begin
   if fileName='' then Exit;
   hFile:=OpenFileForSequentialReadOnly(fileName);
   if hFile=INVALID_HANDLE_VALUE then Exit;
   try
      n:=GetFileSize(hFile, nil);
      if n=INVALID_FILE_SIZE then
         RaiseLastOSError;
      if n>0 then begin
         SetLength(Result, n);
         if not ReadFile(hFile, Result[1], n, nRead, nil) then
            RaiseLastOSError;
      end;
   finally
      CloseHandle(hFile);
   end;
end;

// SaveRawBytesToFile
//
procedure SaveRawBytesToFile(const fileName : UnicodeString; const data : RawByteString);
var
   hFile : THandle;
   nWrite : DWORD;
begin
   hFile:=OpenFileForSequentialWriteOnly(fileName);
   try
      if data<>'' then
         if not WriteFile(hFile, data[1], Length(data), nWrite, nil) then
            RaiseLastOSError;
   finally
      CloseHandle(hFile);
   end;
end;

// SaveTextToUTF8File
//
procedure SaveTextToUTF8File(const fileName, text : UnicodeString);
begin
   SaveRawBytesToFile(fileName, UTF8Encode(text));
end;

// AppendTextToUTF8File
//
procedure AppendTextToUTF8File(const fileName : String; const text : UTF8String);
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
function OpenFileForSequentialReadOnly(const fileName : UnicodeString) : THandle;
begin
   Result:=CreateFileW(PWideChar(fileName), GENERIC_READ, FILE_SHARE_READ+FILE_SHARE_WRITE,
                       nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
   if Result=INVALID_HANDLE_VALUE then begin
      if GetLastError<>ERROR_FILE_NOT_FOUND then
         RaiseLastOSError;
   end;
end;

// OpenFileForSequentialWriteOnly
//
function OpenFileForSequentialWriteOnly(const fileName : UnicodeString) : THandle;
begin
   Result:=CreateFileW(PWideChar(fileName), GENERIC_WRITE, 0, nil, CREATE_ALWAYS,
                       FILE_ATTRIBUTE_NORMAL+FILE_FLAG_SEQUENTIAL_SCAN, 0);
   if Result=INVALID_HANDLE_VALUE then
      RaiseLastOSError;
end;

// CloseFileHandle
//
procedure CloseFileHandle(hFile : THandle);
begin
   CloseHandle(hFile);
end;

// FileWrite
//
function FileWrite(hFile : THandle; buffer : Pointer; byteCount : Integer) : Cardinal;
begin
   if not WriteFile(hFile, buffer^, byteCount, Result, nil) then
      RaiseLastOSError;
end;

// FileCopy
//
function FileCopy(const existing, new : UnicodeString; failIfExists : Boolean) : Boolean;
begin
   Result:=Windows.CopyFileW(PWideChar(existing), PWideChar(new), failIfExists);
end;

// FileMove
//
function FileMove(const existing, new : UnicodeString) : Boolean;
begin
   Result:=Windows.MoveFileW(PWideChar(existing), PWideChar(new));
end;

// FileDelete
//
function FileDelete(const fileName : String) : Boolean;
begin
   Result:=SysUtils.DeleteFile(fileName);
end;

// FileRename
//
function FileRename(const oldName, newName : String) : Boolean;
begin
   Result:=RenameFile(oldName, newName);
end;

// FileSize
//
function FileSize(const name : String) : Int64;
var
   info : TWin32FileAttributeData;
begin
   if GetFileAttributesEx(PChar(Pointer(name)), GetFileExInfoStandard, @info) then
      Result:=info.nFileSizeLow or (Int64(info.nFileSizeHigh) shl 32)
   else Result:=-1;
end;

// FileDateTime
//
function FileDateTime(const name : String) : TDateTime;
var
   info : TWin32FileAttributeData;
   localTime : TFileTime;
   systemTime : TSystemTime;
begin
   if GetFileAttributesEx(PChar(Pointer(name)), GetFileExInfoStandard, @info) then begin
      FileTimeToLocalFileTime(info.ftLastWriteTime, localTime);
      FileTimeToSystemTime(localTime, systemTime);
      Result:=SystemTimeToDateTime(systemTime);
   end else Result:=0;
end;

// DeleteDirectory
//
function DeleteDirectory(const path : String) : Boolean;
begin
   try
      TDirectory.Delete(path, True);
   except
      Exit(False);
   end;
   Result := not TDirectory.Exists(path);
end;

// DirectSet8087CW
//
function DirectSet8087CW(newValue : Word) : Word;
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

// InitializeWithDefaultFormatSettings
//
procedure InitializeWithDefaultFormatSettings(var fmt : TFormatSettings);
begin
   {$ifdef DELPHI_XE_PLUS}
   fmt:=SysUtils.FormatSettings;
   {$else}
   fmt:=SysUtils.TFormatSettings((@CurrencyString)^);
   {$endif}
end;

// FindDelimiter
//
{$ifdef NEED_FindDelimiter}
function FindDelimiter(const Delimiters, S: string; StartIdx: Integer = 1): Integer;
begin
  for Result := StartIdx to Length(S) do
    if IsDelimiter(Delimiters, S, Result) then
      Exit;
  Result := -1;
end;
{$endif}

// ------------------
// ------------------ TFixedCriticalSection ------------------
// ------------------

// Create
//
constructor TFixedCriticalSection.Create;
begin
   InitializeCriticalSection(FCS);
end;

// Destroy
//
destructor TFixedCriticalSection.Destroy;
begin
   DeleteCriticalSection(FCS);
end;

// Enter
//
procedure TFixedCriticalSection.Enter;
begin
   EnterCriticalSection(FCS);
end;

// Leave
//
procedure TFixedCriticalSection.Leave;
begin
   LeaveCriticalSection(FCS);
end;

// TryEnter
//
function TFixedCriticalSection.TryEnter : Boolean;
begin
   Result:=TryEnterCriticalSection(FCS);
end;

// ------------------
// ------------------ TPath ------------------
// ------------------

// GetTempFileName
//
class function TPath.GetTempFileName : UnicodeString;
{$IFDEF VER200} // Delphi 2009
var
   tempPath, tempFileName : array [0..MAX_PATH] of WideChar; // Buf sizes are MAX_PATH+1
begin
   if Windows.GetTempPath(MAX_PATH, @tempPath[0])=0 then begin
      tempPath[1]:='.'; // Current directory
      tempPath[2]:=#0;
   end;
   if Windows.GetTempFileNameW(@tempPath[0], 'DWS', 0, tempFileName)=0 then
      RaiseLastOSError; // should never happen
   Result:=tempFileName;
{$ELSE}
begin
   Result:=IOUTils.TPath.GetTempFileName;
{$ENDIF}
end;

// ------------------
// ------------------ TFile ------------------
// ------------------

// ReadAllBytes
//
class function TFile.ReadAllBytes(const filename : UnicodeString) : TBytes;
{$IFDEF VER200} // Delphi 2009
var
   fileStream : TFileStream;
   n : Integer;
begin
   fileStream:=TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
   try
      n:=fileStream.Size;
      SetLength(Result, n);
      if n>0 then
         fileStream.ReadBuffer(Result[0], n);
   finally
      fileStream.Free;
   end;
{$ELSE}
begin
   Result:=IOUTils.TFile.ReadAllBytes(filename);
{$ENDIF}
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

// ------------------
// ------------------ TMultiReadSingleWrite ------------------
// ------------------

// light-weight SRW is supported on Vista and above
// we detect by feature rather than OS Version
type
   SRWLOCK = Pointer;
var vSupportsSRWChecked : Boolean;

var AcquireSRWLockExclusive : procedure (var SRWLock : SRWLOCK); stdcall;
var TryAcquireSRWLockExclusive : function (var SRWLock : SRWLOCK) : BOOL; stdcall;
var ReleaseSRWLockExclusive : procedure (var SRWLock : SRWLOCK); stdcall;

var AcquireSRWLockShared : procedure(var SRWLock : SRWLOCK); stdcall;
var TryAcquireSRWLockShared : function (var SRWLock : SRWLOCK) : BOOL; stdcall;
var ReleaseSRWLockShared : procedure (var SRWLock : SRWLOCK); stdcall;

function SupportsSRW : Boolean;
var
   h : HMODULE;
begin
   if not vSupportsSRWChecked then begin
      vSupportsSRWChecked:=True;
      h:=GetModuleHandle('kernel32');
      AcquireSRWLockExclusive:=GetProcAddress(h, 'AcquireSRWLockExclusive');
      TryAcquireSRWLockExclusive:=GetProcAddress(h, 'TryAcquireSRWLockExclusive');
      ReleaseSRWLockExclusive:=GetProcAddress(h, 'ReleaseSRWLockExclusive');
      AcquireSRWLockShared:=GetProcAddress(h, 'AcquireSRWLockShared');
      TryAcquireSRWLockShared:=GetProcAddress(h, 'TryAcquireSRWLockShared');
      ReleaseSRWLockShared:=GetProcAddress(h, 'ReleaseSRWLockShared');
   end;
   Result:=Assigned(AcquireSRWLockExclusive);
end;

// Create
//
constructor TMultiReadSingleWrite.Create(forceFallBack : Boolean = False);
begin
   if forceFallBack or not SupportsSRW then
      FCS:=TFixedCriticalSection.Create;
end;

// Destroy
//
destructor TMultiReadSingleWrite.Destroy;
begin
   FCS.Free;
end;

// BeginRead
//
procedure TMultiReadSingleWrite.BeginRead;
begin
   if Assigned(FCS) then
      FCS.Enter
   else AcquireSRWLockShared(FSRWLock);
end;

// TryBeginRead
//
function TMultiReadSingleWrite.TryBeginRead : Boolean;
begin
   if Assigned(FCS) then
      Result:=FCS.TryEnter
   else Result:=TryAcquireSRWLockShared(FSRWLock);
end;

// EndRead
//
procedure TMultiReadSingleWrite.EndRead;
begin
   if Assigned(FCS) then
      FCS.Leave
   else ReleaseSRWLockShared(FSRWLock)
end;

// BeginWrite
//
procedure TMultiReadSingleWrite.BeginWrite;
begin
   if Assigned(FCS) then
      FCS.Enter
   else AcquireSRWLockExclusive(FSRWLock);
end;

// TryBeginWrite
//
function TMultiReadSingleWrite.TryBeginWrite : Boolean;
begin
   if Assigned(FCS) then
      Result:=FCS.TryEnter
   else Result:=TryAcquireSRWLockExclusive(FSRWLock);
end;

// EndWrite
//
procedure TMultiReadSingleWrite.EndWrite;
begin
   if Assigned(FCS) then
      FCS.Leave
   else ReleaseSRWLockExclusive(FSRWLock)
end;

// State
//
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

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   InitializeGetSystemMilliseconds;

end.
