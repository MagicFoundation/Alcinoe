{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclPCRE.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Peter Thornqvist.                                  }
{ Portions created by Peter Thornqvist are Copyright (C) of Peter Thornqvist. All rights reserved. }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Robert Rossmair (rrossmair)                                                                    }
{   Mario R. Carro                                                                                 }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Class wrapper for PCRE (PERL Compatible Regular Expression)                                      }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclPCRE;

{$I jcl.inc}

{$RANGECHECKS OFF}

interface

uses
  pcre,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  {$IFDEF HAS_UNITSCOPE}
  System.Classes, System.SysUtils,
  {$IFDEF PCRE_RTL}
  System.RegularExpressionsAPI,
  {$ENDIF PCRE_RTL}
  {$ELSE ~HAS_UNITSCOPE}
  Classes, SysUtils,
  {$IFDEF PCRE_RTL}
  RegularExpressionsAPI,
  {$ENDIF PCRE_RTL}
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase, JclStringConversions;

const
  JCL_PCRE_CALLOUT_NOERROR      = 0;
  JCL_PCRE_CALLOUT_FAILCONTINUE = 1;

  JCL_PCRE_ERROR_NOJIT          = -996;
  JCL_PCRE_ERROR_NOUTF8         = -997;
  JCL_PCRE_ERROR_NOUTF16        = -997;
  JCL_PCRE_ERROR_CALLOUTERROR   = -998;
  JCL_PCRE_ERROR_STUDYFAILED    = -999;

type
  EPCREError = class(EJclError)
  private
    FErrorCode: Integer;
  public
    constructor CreateRes(ResStringRec: PResStringRec; ErrorCode: Integer);
    property ErrorCode: Integer read FErrorCode;
  end;

  TPCREIntArray = array [0 .. 0] of Integer;
  PPCREIntArray = ^TPCREIntArray;

  TJclRegExOption = (roIgnoreCase, roMultiLine, roDotAll, roExtended,
    roAnchored, roDollarEndOnly, roExtra, roNotBOL, roNotEOL, roUnGreedy,
    roNotEmpty, roUTF8, roNoAutoCapture, roNoUTF8Check, roAutoCallout,
    roPartial, roDfaShortest, roDfaRestart, roFirstLine, roDupNames,
    roNewLineCR, roNewLineLF, roNewLineCRLF, roNewLineAny, roBSRAnyCRLF,
    roBSRUnicode, roJavascriptCompat, roNoStartOptimize, roPartialHard,
    roNotEmptyAtStart, roUCP);
  TJclRegExOptions = set of TJclRegExOption;

const
  roUTF16 = roUTF8;
  roNoUTF16Check = roNoUTF8Check;

type
  TJclRegExBase = class;

  TJclCaptureRange = record
    FirstPos: Integer;
    LastPos: Integer;
  end;

  TJclRegExCallout = procedure (Sender: TJclRegExBase;
    Index, MatchStart, SubjectPos, LastCapture, PatternPos, NextItemLength: Integer;
    var ErrorCode: Integer) of object;
  TPCRECalloutIndex = 0 .. 255;

  TJclRegExBase = class(TObject)
  private
    FOptions: TJclRegExOptions;
    FPattern: string;
    FDfaMode: Boolean;
    FSubject: string;

    FViewChanges: Boolean;
    FChangedCaptures: TList;
    FResultValues: array of string;

    FErrorCode: Integer;
    FErrorMessage: string;
    FErrorOffset: Integer;

    FVector: PPCREIntArray;
    FVectorSize: Integer;
    FCaptureCount: Integer;

    FOnCallout: TJclRegExCallout;

  protected
    function GetResult: string; virtual;
    function GetCapture(Index: Integer): string; virtual; abstract;
    procedure SetCapture(Index: Integer; const Value: string); virtual;
    function GetCaptureRange(Index: Integer): TJclCaptureRange; virtual; abstract;
    function GetNamedCapture(const Name: string): string; virtual; abstract;
    procedure SetNamedCapture(const Name, Value: string); virtual; abstract;
    function GetCaptureNameCount: Integer; virtual; abstract;
    function GetCaptureName(Index: Integer): string; virtual; abstract;
    function GetAPIOptions(RunTime, DFA: Boolean): Integer; virtual;

    function SupportsWideChar: Boolean; virtual;
  public
    destructor Destroy; override;

    property Options: TJclRegExOptions read FOptions write FOptions;
    function Compile(const Pattern: string; Study: Boolean;
      UserLocale: Boolean = False; JITCompile: Boolean = False): Boolean; virtual; abstract;
    property Pattern: string read FPattern;
    property DfaMode: Boolean read FDfaMode write FDfaMode;
    function Match(const Subject: string; StartOffset: Cardinal = 1): Boolean; virtual; abstract;
    property Subject: string read FSubject;
    property Result: string read GetResult;

    property ViewChanges: Boolean read FViewChanges write FViewChanges;
    property CaptureCount: Integer read FCaptureCount write FCaptureCount;
    property Captures[Index: Integer]: string read GetCapture write SetCapture;
    property CaptureRanges[Index: Integer]: TJclCaptureRange read GetCaptureRange;

    property NamedCaptures[const Name: string]: string
      read GetNamedCapture write SetNamedCapture;
    property CaptureNameCount: Integer read GetCaptureNameCount;
    property CaptureNames[Index: Integer]: string read GetCaptureName;
    function IndexOfName(const Name: string): Integer; virtual; abstract;
    function IsNameValid(const Name: string): Boolean; virtual; abstract;

    property ErrorCode: Integer read FErrorCode;
    property ErrorMessage: string read FErrorMessage;
    property ErrorOffset: Integer read FErrorOffset;

    property OnCallout: TJclRegExCallout read FOnCallout write FOnCallout;
  end;

  {$IFDEF PCRE_8}
  TJclAnsiRegEx = class(TJclRegExBase)
  private
    FCode: PPCRE;
    FExtra: PPCREExtra;
  protected
    function CalloutHandler(var CalloutBlock: pcre_callout_block): Integer;
    function GetAPIOptions(RunTime, DFA: Boolean): Integer; override;
    function GetCapture(Index: Integer): string; override;
    function GetCaptureName(Index: Integer): string; override;
    function GetCaptureNameCount: Integer; override;
    function GetCaptureRange(Index: Integer): TJclCaptureRange; override;
    function GetNamedCapture(const Name: string): string; override;
    procedure SetNamedCapture(const Name, Value: string); override;
  public
    destructor Destroy; override;
    function Compile(const Pattern: string; Study: Boolean;
      UserLocale: Boolean = False; JITCompile: Boolean = False): Boolean; override;
    function Match(const Subject: string; StartOffset: Cardinal = 1): Boolean; override;
    function IndexOfName(const Name: string): Integer; override;
    function IsNameValid(const Name: string): Boolean; override;
  end;

  TJclAnsiRegExOption = TJclRegExOption;
  TJclAnsiRegExOptions = TJclRegExOptions;
  TJclAnsiCaptureRange = TJclCaptureRange;
  TJclAnsiRegExCallout = TJclRegExCallout;
  {$ENDIF PCRE_8}

  {$IFDEF PCRE_16}
  TJclWideRegEx = class(TJclRegExBase)
  private
    FCode: PPCRE16;
    FExtra: PPCRE16Extra;
  protected
    function CalloutHandler(var CalloutBlock: pcre16_callout_block): Integer;
    function GetAPIOptions(RunTime, DFA: Boolean): Integer; override;
    function GetCapture(Index: Integer): string; override;
    function GetCaptureName(Index: Integer): string; override;
    function GetCaptureNameCount: Integer; override;
    function GetCaptureRange(Index: Integer): TJclCaptureRange; override;
    function GetNamedCapture(const Name: string): string; override;
    procedure SetNamedCapture(const Name, Value: string); override;
    function SupportsWideChar: Boolean; override;
  public
    destructor Destroy; override;
    function Compile(const Pattern: string; Study: Boolean;
      UserLocale: Boolean = False; JITCompile: Boolean = False): Boolean; override;
    function Match(const Subject: string; StartOffset: Cardinal = 1): Boolean; override;
    function IndexOfName(const Name: string): Integer; override;
    function IsNameValid(const Name: string): Boolean; override;
  end;

  TJclWideRegExOption = TJclRegExOption;
  TJclWideRegExOptions = TJclRegExOptions;
  TJclWideCaptureRange = TJclCaptureRange;
  TJclWideRegExCallout = TJclRegExCallout;
  {$ENDIF PCRE_16}

  {$IFDEF JCL_PCRE_8}
  TJclRegEx = TJclAnsiRegEx;
  {$ENDIF JCL_PCRE_8}
  {$IFDEF JCL_PCRE_16}
  TJclRegEx = TJclWideRegEx;
  {$ENDIF JCL_PCRE_16}

{$IFDEF PCRE_8}
procedure InitializeLocaleSupport;
procedure TerminateLocaleSupport;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
procedure InitializeLocaleSupport16;
procedure TerminateLocaleSupport16;
{$ENDIF PCRE_16}

{$IFDEF JCL_PCRE}
// Args is an array of pairs (CaptureIndex, Value) or (CaptureName, Value).
// For example: NewIp := StrReplaceRegEx(DirIP, '(\d+)\.(\d+)\.(\d+)\.(\d+)', [3, '128', 4, '254']);
function StrReplaceRegEx(const Subject, Pattern: string; Args: array of const): string;
{$ENDIF JCL_PCRE}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  System.SysConst,
  {$ELSE ~HAS_UNITSCOPE}
  SysConst,
  {$ENDIF ~HAS_UNITSCOPE}
  JclResources;

function EncodeAnsiString(const S: string; ToUTF8: Boolean): AnsiString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
begin
  if ToUTF8 then
    Result := StringToUTF8(S)
  else
    Result := AnsiString(S);
end;

function EncodeWideString(const S: string; ToUTF16: Boolean): WideString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
begin
  if ToUTF16 then
    Result := StringToUTF16(S)
  else
    Result := WideString(S);
end;

function DecodeAnsiString(const S: AnsiString; IsUTF8: Boolean): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
begin
  if IsUTF8 then
    Result := UTF8ToString(S)
  else
    Result := string(S);
end;

function DecodeWideString(const S: WideString; IsUTF16: Boolean): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
begin
  if IsUTF16 then
    Result := UTF16ToString(S)
  else
    Result := string(S);
end;

function TranslateAnsiIndex(const S: string; ToUTF8: Boolean; Index: SizeInt): SizeInt;
var
  UTF8Buffer: TUTF8String;
  UTF8Pos, StrPos, StrLen: SizeInt;
  Ch: UCS4;
begin
  if ToUTF8 then
  begin
    SetLength(UTF8Buffer, 6);
    StrPos := 1;
    StrLen := Length(S);
    while (StrPos > 0) and (StrPos <= StrLen) and (Index > 1) do
    begin
      UTF8Pos := 1;
      Ch := StringGetNextChar(S, StrPos);
      if (StrPos > 0) and UTF8SetNextChar(UTF8Buffer, UTF8Pos, Ch) and (UTF8Pos > 0) then
        Dec(Index, UTF8Pos - 1);
    end;
    if StrPos <= 0 then
      raise EJclUnexpectedEOSequenceError.Create
    else
    if StrPos > StrLen then
      Result := StrLen + 1
    else
      Result := StrPos;
  end
  else
    Result := Index;
end;

function TranslateWideIndex(const S: string; ToUTF16: Boolean; Index: SizeInt): SizeInt;
var
  UTF16Buffer: TUTF16String;
  UTF16Pos, StrPos, StrLen: SizeInt;
  Ch: UCS4;
begin
  if ToUTF16 then
  begin
    SetLength(UTF16Buffer, 2);
    StrPos := 1;
    StrLen := Length(S);
    while (StrPos > 0) and (StrPos <= StrLen) and (Index > 1) do
    begin
      UTF16Pos := 1;
      Ch := StringGetNextChar(S, StrPos);
      if (StrPos > 0) and UTF16SetNextChar(UTF16Buffer, UTF16Pos, Ch) and (UTF16Pos > 0) then
        Dec(Index, UTF16Pos - 1);
    end;
    if StrPos <= 0 then
      raise EJclUnexpectedEOSequenceError.Create
    else
    if StrPos > StrLen then
      Result := StrLen + 1
    else
      Result := StrPos;
  end
  else
    Result := Index;
end;

{$IFDEF JCL_PCRE}
var
  {$IFDEF PCRE_8}
  GTables: PAnsiChar;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  GTables16: PAnsiChar;
  {$ENDIF PCRE_16}
{$ENDIF JCL_PCRE}

{$IFDEF RTL230_UP}
  {$IFDEF PCRE_RTL}
    {$DEFINE PCRE_EXPORT_CDECL}
  {$ENDIF PCRE_RTL}
{$ENDIF RTL230_UP}

type
  {$IFDEF PCRE_RTL}
  {$IFDEF BDS10_UP}
  TPCREGetMemInteger = NativeUInt;
  {$ELSE ~BDS10_UP}
  TPCREGetMemInteger = Integer;
  {$ENDIF ~BDS10_UP}
  {$ELSE ~PCRE_RTL}
  TPCREGetMemInteger = SizeInt;
  {$ENDIF ~PCRE_RTL}

function JclPCREGetMem(Size: TPCREGetMemInteger): Pointer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
  GetMem(Result, Size);
end;

function JclPCRE16GetMem(Size: SizeInt): Pointer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
  GetMem(Result, Size);
end;

procedure JclPCREFreeMem(P: Pointer); {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
  FreeMem(P);
end;

procedure JclPCRE16FreeMem(P: Pointer); {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
  FreeMem(P);
end;

{$IFDEF PCRE_8}
function JclPCRECallout(var callout_block: pcre_callout_block): Integer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
   Result := TJclAnsiRegEx(callout_block.callout_data).CalloutHandler(callout_block);
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
function JclPCRE16Callout(var callout_block: pcre16_callout_block): Integer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
   Result := TJclWideRegEx(callout_block.callout_data).CalloutHandler(callout_block);
end;
{$ENDIF PCRE_16}

function PCRECheck(Value: Integer; Wide: Boolean): Boolean;
var
  PErr: PResStringRec;
begin
  Result := Value >= 0;
  if Result then Exit;

  case Value of
    PCRE_ERROR_NOMATCH:
      PErr := @RsErrNoMatch;
    PCRE_ERROR_NULL:
      PErr := @RsErrNull;
    PCRE_ERROR_BADOPTION:
      PErr := @RsErrBadOption;
    PCRE_ERROR_BADMAGIC:
      PErr := @RsErrBadMagic;
    PCRE_ERROR_UNKNOWN_NODE:
      PErr := @RsErrUnknownNode;
    PCRE_ERROR_NOMEMORY:
      PErr := @RsErrNoMemory;
    PCRE_ERROR_NOSUBSTRING:
      PErr := @RsErrNoSubString;
    PCRE_ERROR_MATCHLIMIT:
      PErr := @RsErrMatchLimit;
    PCRE_ERROR_CALLOUT:
      PErr := @RsErrCallout;
    PCRE_ERROR_BADUTF8:
      if Wide then
        PErr := @RsErrBadUTF16
      else
        PErr := @RsErrBadUTF8;
    PCRE_ERROR_BADUTF8_OFFSET:
      if Wide then
        PErr := @RsErrBadUTF16Offset
      else
        PErr := @RsErrBadUTF8Offset;
    PCRE_ERROR_PARTIAL:
      PErr := @RsErrPartial;
    PCRE_ERROR_BADPARTIAL:
      PErr := @RsErrBadPartial;
    PCRE_ERROR_INTERNAL:
      PErr := @RsErrInternal;
    PCRE_ERROR_BADCOUNT:
      PErr := @RsErrBadCount;
    PCRE_ERROR_DFA_UITEM:
      PErr := @RsErrDfaUItem;
    PCRE_ERROR_DFA_UCOND:
      PErr := @RsErrDfaUCond;
    PCRE_ERROR_DFA_UMLIMIT:
      PErr := @RsErrDfaUMLimit;
    PCRE_ERROR_DFA_WSSIZE:
      PErr := @RsErrDfaWSSize;
    PCRE_ERROR_DFA_RECURSE:
      PErr := @RsErrDfaRecurse;
    PCRE_ERROR_RECURSIONLIMIT:
      PErr := @RsErrRecursionLimit;
    PCRE_ERROR_NULLWSLIMIT:
      PErr := @RsErrNullWsLimit;
    PCRE_ERROR_BADNEWLINE:
      PErr := @RsErrBadNewLine;
    {$IFNDEF PCRE_RTL}
    PCRE_ERROR_BADOFFSET:
      PErr := @RsErrBadOffset;
    PCRE_ERROR_SHORTUTF8:
      if Wide then
        PErr := @RsErrShortUTF16
      else
        PErr := @RsErrShortUTF8;
    PCRE_ERROR_RECURSELOOP:
      PErr := @RsErrRecurseLoop;
    PCRE_ERROR_JITSTACKLIMIT:
      PErr := @RsErrJITStackLimit;
    PCRE_ERROR_BADMODE:
      PErr := @RsErrBadMode;
    PCRE_ERROR_BADENDIANNESS:
      PErr := @RsErrBadEndianness;
    PCRE_ERROR_DFA_BADRESTART:
      PErr := @RsErrBadRestart;
    {$ENDIF ~PCRE_RTL}
    JCL_PCRE_ERROR_STUDYFAILED:
      PErr := @RsErrStudyFailed;
    JCL_PCRE_ERROR_CALLOUTERROR:
      PErr := @RsErrCalloutError;
    JCL_PCRE_ERROR_NOUTF8:
      if Wide then
        PErr := @RsErrNoUTF16Support
      else
        PErr := @RsErrNoUTF8Support;
    JCL_PCRE_ERROR_NOJIT:
      PErr := @RsErrNoJITSupport;
  else
    PErr := @RsErrUnknownError;
  end;

  raise EPCREError.CreateRes(PErr, Value);
end;

//=== { TJclRegEx } ===========================================================

destructor TJclRegExBase.Destroy;
begin
  if Assigned(FVector) then
    FreeMem(FVector);
  if Assigned(FChangedCaptures) then
    FChangedCaptures.Free;

  inherited Destroy;
end;

function TJclRegExBase.GetAPIOptions(RunTime, DFA: Boolean): Integer;
const
  {$IFDEF PCRE_RTL}
  PCRE_PARTIAL_HARD     = $08000000;
  PCRE_NOTEMPTY_ATSTART = $10000000;
  PCRE_UCP              = $20000000;
  {$ENDIF PCRE_RTL}
  { roIgnoreCase, roMultiLine, roDotAll, roExtended,
    roAnchored, roDollarEndOnly, roExtra, roNotBOL, roNotEOL, roUnGreedy,
    roNotEmpty, roUTF8, roNoAutoCapture, roNoUTF8Check, roAutoCallout,
    roPartial, roDfaShortest, roDfaRestart, roFirstLine, roDupNames,
    roNewLineCR, roNewLineLF, roNewLineCRLF, roNewLineAny, roBSRAnyCRLF,
    roBSRUnicode, roJavascriptCompat, roNoStartOptimize, roPartialHard,
    roNotEmptyAtStart, roUCP }
  cDesignOptions: array [TJclRegExOption] of Integer =
   (PCRE_CASELESS, PCRE_MULTILINE, PCRE_DOTALL, PCRE_EXTENDED, PCRE_ANCHORED,
    PCRE_DOLLAR_ENDONLY, PCRE_EXTRA, 0, 0, PCRE_UNGREEDY, 0, PCRE_UTF8,
    PCRE_NO_AUTO_CAPTURE, PCRE_NO_UTF8_CHECK, PCRE_AUTO_CALLOUT, 0, 0, 0,
    PCRE_FIRSTLINE, PCRE_DUPNAMES, PCRE_NEWLINE_CR, PCRE_NEWLINE_LF,
    PCRE_NEWLINE_CRLF, PCRE_NEWLINE_ANY, PCRE_BSR_ANYCRLF, PCRE_BSR_UNICODE,
    PCRE_JAVASCRIPT_COMPAT, PCRE_NO_START_OPTIMIZE, 0, 0, PCRE_UCP);
  cRunOptions: array [TJclRegExOption] of Integer =
   (0, 0, 0, 0, PCRE_ANCHORED, 0, 0, PCRE_NOTBOL, PCRE_NOTEOL,
    0, PCRE_NOTEMPTY, 0, 0, PCRE_NO_UTF8_CHECK, 0, PCRE_PARTIAL, 0, 0,
    0, 0, PCRE_NEWLINE_CR, PCRE_NEWLINE_LF, PCRE_NEWLINE_CRLF,
    PCRE_NEWLINE_ANY, PCRE_BSR_ANYCRLF, PCRE_BSR_UNICODE,
    0, PCRE_NO_START_OPTIMIZE, PCRE_PARTIAL_HARD,
    PCRE_NOTEMPTY_ATSTART, 0);
  cDFARunOptions: array [TJclRegExOption] of Integer =
   (0, 0, 0, 0, PCRE_ANCHORED, 0, 0, PCRE_NOTBOL, PCRE_NOTEOL,
    0, PCRE_NOTEMPTY, 0, 0, PCRE_NO_UTF8_CHECK, 0, PCRE_PARTIAL,
    PCRE_DFA_SHORTEST, PCRE_DFA_RESTART, 0, 0, PCRE_NEWLINE_CR,
    PCRE_NEWLINE_LF, PCRE_NEWLINE_CRLF, PCRE_NEWLINE_ANY, PCRE_BSR_ANYCRLF,
    PCRE_BSR_UNICODE, 0, PCRE_NO_START_OPTIMIZE, PCRE_PARTIAL_HARD,
    PCRE_NOTEMPTY_ATSTART, 0);
var
  I: TJclRegExOption;
begin
  Result := 0;
  if RunTime and DFA then
  begin
    for I := Low(TJclRegExOption) to High(TJclRegExOption) do
      if I in Options then
        Result := Result or cDFARunOptions[I];
  end
  else
  if RunTime then
  begin
    for I := Low(TJclRegExOption) to High(TJclRegExOption) do
      if I in Options then
        Result := Result or cRunOptions[I];
  end
  else
  begin
    for I := Low(TJclRegExOption) to High(TJclRegExOption) do
      if I in Options then
        Result := Result or cDesignOptions[I];
  end;
end;

function TJclRegExBase.GetResult: string;
var
  Index, CaptureIndex: Integer;
  Pos: Integer;
  Range: TJclCaptureRange;
begin
  if Assigned(FChangedCaptures) and (FChangedCaptures.Count > 0) then
  begin
    Pos := 1;
    Result := '';
    for Index := 0 to FChangedCaptures.Count - 1 do
    begin
      CaptureIndex := SizeInt(FChangedCaptures[Index]);
      Range := GetCaptureRange(CaptureIndex);

      Result := Result +
        Copy(FSubject, Pos, Range.FirstPos - Pos) +
        FResultValues[CaptureIndex];

      Pos := Range.LastPos + 1;
    end;
    if Pos <= Length(FSubject) then
      Result := Result + Copy(FSubject, Pos, Length(FSubject) - Pos + 1);
  end
  else
    Result := FSubject;
end;

procedure TJclRegExBase.SetCapture(Index: Integer; const Value: string);
begin
  if (Index < 0) or (Index >= FCaptureCount) then
    PCRECheck(PCRE_ERROR_NOSUBSTRING, SupportsWideChar)
  else
  begin
    if (not Assigned(FChangedCaptures)) or (FChangedCaptures.Count = 0) then
    begin
      if not Assigned(FChangedCaptures) then
        FChangedCaptures := TList.Create;

      // Always resize to the max length to avoid repeated allocations.
      FChangedCaptures.Capacity := FCaptureCount;
      SetLength(FResultValues, FCaptureCount);
    end;

    if FChangedCaptures.IndexOf(Pointer(SizeInt(Index))) < 0 then
      FChangedCaptures.Add(Pointer(SizeInt(Index)));
    FResultValues[Index] := Value;
  end;
end;

function TJclRegExBase.SupportsWideChar: Boolean;
begin
  Result := False;
end;

{$IFDEF PCRE_8}
procedure InitializeLocaleSupport;
begin
  if not Assigned(GTables) then
    GTables := pcre_maketables;
end;

procedure TerminateLocaleSupport;
begin
  if Assigned(GTables) then
  begin
    CallPCREFree(GTables);
    GTables := nil;
  end;
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
procedure InitializeLocaleSupport16;
begin
  if not Assigned(GTables16) then
    GTables16 := pcre16_maketables;
end;

procedure TerminateLocaleSupport16;
begin
  if Assigned(GTables16) then
  begin
    CallPCRE16Free(GTables16);
    GTables16 := nil;
  end;
end;
{$ENDIF PCRE_16}

{$IFDEF JCL_PCRE}
// TODO: Better/specific error messages, show index when available.
function StrReplaceRegEx(const Subject, Pattern: string; Args: array of const): string;

  function ArgToString(Index: Integer): string;
  begin
    // TODO: Any other type?
    case TVarRec(Args[Index]).VType of
      vtPChar:
        Result := string(AnsiString(TVarRec(Args[Index]).VPChar));
      vtPWideChar:
        Result := string(WideString(TVarRec(Args[Index]).VPWideChar));
      vtString:
        Result := string(TVarRec(Args[Index]).VString^);
      vtAnsiString:
        Result := string(AnsiString(TVarRec(Args[Index]).VAnsiString));
      vtWideString:
        Result := string(WideString(TVarRec(Args[Index]).VWideString));
      {$IFDEF SUPPORTS_UNICODE_STRING}
      vtUnicodeString:
        Result := string(UnicodeString(TVarRec(Args[Index]).VUnicodeString));
      {$ENDIF SUPPORTS_UNICODE_STRING}
      vtChar:
        Result := string(AnsiString(TVarRec(Args[Index]).VChar));
      vtWideChar:
        Result := string(WideString(TVarRec(Args[Index]).VWideChar));
    else
      raise EConvertError.Create(SInvalidFormat);
    end;
  end;

var
  Re: TJclRegExBase;
  Index, ArgIndex: Integer;
  Value: string;
begin
  if Odd(Length(Args)) then
    raise EConvertError.Create(SArgumentMissing)
  else
  begin
    Re := TJclRegEx.Create;
    try
      if Re.Compile(Pattern, False) and Re.Match(Subject) then
      begin
        for Index := 0 to Length(Args) div 2 - 1 do
        begin
          ArgIndex := Index * 2;
          Value := ArgToString(ArgIndex + 1);

          if TVarRec(Args[ArgIndex]).VType = vtInteger then
            Re.Captures[TVarRec(Args[ArgIndex]).VInteger] := Value
          else
            Re.NamedCaptures[ArgToString(ArgIndex)] := Value;
        end;

        Result := Re.Result;
      end
      else
        raise EConvertError.Create(SInvalidFormat);
    finally
      Re.Free;
    end;
  end;
end;
{$ENDIF JCL_PCRE}

//=== { EPCREError } =========================================================

constructor EPCREError.CreateRes(ResStringRec: PResStringRec; ErrorCode: Integer);
begin
  FErrorCode := ErrorCode;
  inherited CreateRes(ResStringRec);
end;

procedure LibNotLoadedHandler; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
  raise EPCREError.CreateRes(@RsErrLibNotLoaded, 0);
end;

{$IFDEF PCRE_8}

//=== { TJclAnsiRegEx } ======================================================

destructor TJclAnsiRegEx.Destroy;
begin
  if Assigned(FCode) then
    CallPCREFree(FCode);
  if Assigned(FExtra) then
    {$IFDEF PCRE_RTL}
    CallPCREFree(FExtra);
    {$ELSE ~PCRE_RTL}
    pcre_free_study(FExtra);
    {$ENDIF ~PCRE_RTL}
  inherited Destroy;
end;

function TJclAnsiRegEx.CalloutHandler(var CalloutBlock: pcre_callout_block): Integer;
begin
  try
    Result := JCL_PCRE_CALLOUT_NOERROR;
    if Assigned(FOnCallout) then
    begin
      with CalloutBlock do
      begin
        FCaptureCount := capture_top;
        FOnCallout(Self, callout_number, start_match + 1, current_position + 1,
          capture_last, pattern_position + 1, next_item_length, Result);
      end;
    end;
  except
    on E: Exception do
    begin
      FErrorMessage := E.Message;
      Result := JCL_PCRE_ERROR_CALLOUTERROR;
    end;
  end;
end;

function TJclAnsiRegEx.Compile(const Pattern: string; Study, UserLocale, JITCompile: Boolean): Boolean;
var
  ErrMsgPtr: PAnsiChar;
  Tables: PAnsiChar;
  StudyOptions: Integer;
  {$IFNDEF PCRE_RTL}
  ConfigJIT: Integer;
  {$ENDIF ~PCRE_RTL}
begin
  if UserLocale then
  begin
    InitializeLocaleSupport;
    Tables := GTables;
  end
  else
    Tables := nil;

  FPattern := Pattern;
  if FPattern = '' then
    PCRECheck(PCRE_ERROR_NULL, SupportsWideChar);

  if Assigned(FCode) then
  begin
    CallPCREFree(FCode);
    FCode := nil;
  end;
  FCode := pcre_compile2(PAnsiChar(EncodeAnsiString(FPattern, roUTF8 in Options)), GetAPIOptions(False, DfaMode),
    @FErrorCode, @ErrMsgPtr, @FErrorOffset, Tables);
  Inc(FErrorOffset);
  FErrorMessage := string(AnsiString(ErrMsgPtr));
  Result := Assigned(FCode);
  if Result then
  begin
    if Study then
    begin
      {$IFDEF PCRE_RTL}
      if Assigned(FExtra) then
        CallPCREFree(FExtra);
      if JITCompile then
        raise EPCREError.CreateRes(@RsErrNoJITSupport, 0);
      StudyOptions := 0;
      {$ELSE ~PCRE_RTL}
      if Assigned(FExtra) then
        pcre_free_study(FExtra);
      if JITCompile then
      begin
        PCRECheck(pcre_config(PCRE_CONFIG_JIT, @ConfigJIT), SupportsWideChar);
        if ConfigJIT = 0 then
          raise EPCREError.CreateRes(@RsErrNoJITSupport, 0);
        StudyOptions := PCRE_STUDY_JIT_COMPILE;
      end
      else
        StudyOptions := 0;
      {$ENDIF ~PCRE_RTL}
      FExtra := pcre_study(FCode, StudyOptions, @ErrMsgPtr);
      Result := Assigned(FExtra) or (not Assigned(ErrMsgPtr));
      if not Result then
      begin
        FErrorCode := JCL_PCRE_ERROR_STUDYFAILED;
        FErrorMessage := string(AnsiString(ErrMsgPtr));
      end;
    end;

    if FDfaMode then
      FVectorSize := FCaptureCount
    else
    begin
      PCRECheck(pcre_fullinfo(FCode, FExtra, PCRE_INFO_CAPTURECOUNT, @FCaptureCount), SupportsWideChar);
      FVectorSize := (FCaptureCount + 1) * 3;
    end;
    ReAllocMem(FVector, FVectorSize * SizeOf(FVector[0]));
  end;
end;

function TJclAnsiRegEx.GetAPIOptions(RunTime, DFA: Boolean): Integer;
var
  ConfigUTF8: Integer;
begin
  PCRECheck(pcre_config(PCRE_CONFIG_UTF8, @ConfigUTF8), SupportsWideChar);
  if (roUTF8 in Options) and (ConfigUTF8 = 0) then
    PCRECheck(JCL_PCRE_ERROR_NOUTF8, SupportsWideChar);

  Result := inherited GetAPIOptions(RunTime, DFA);
end;

function TJclAnsiRegEx.GetCapture(Index: Integer): string;
var
  FromPos, ToPos: SizeInt;
begin
  if (Index < 0) or (Index >= FCaptureCount) then
    PCRECheck(PCRE_ERROR_NOSUBSTRING, SupportsWideChar)
  else
  begin
    if FViewChanges and (FChangedCaptures.IndexOf(Pointer(SizeInt(Index))) >= 0) then
    begin
      Result := FResultValues[Index];
      Exit;
    end;

    Index := Index * 2;
    FromPos := TranslateAnsiIndex(FSubject, roUTF8 in Options, FVector^[Index] + 1);
    ToPos := TranslateAnsiIndex(FSubject, roUTF8 in Options, FVector^[Index + 1] + 1) - 1;
    Result := Copy(FSubject, FromPos, ToPos - FromPos + 1);
  end;
end;

function TJclAnsiRegEx.GetCaptureNameCount: Integer;
begin
  PCRECheck(pcre_fullinfo(FCode, FExtra, PCRE_INFO_NAMECOUNT, @Result), SupportsWideChar);
end;

function TJclAnsiRegEx.GetCaptureName(Index: Integer): string;
var
  NameTable: PAnsiChar;
  EntrySize: Integer;
begin
  PCRECheck(pcre_fullinfo(FCode, FExtra, PCRE_INFO_NAMETABLE, @NameTable), SupportsWideChar);
  PCRECheck(pcre_fullinfo(FCode, FExtra, PCRE_INFO_NAMEENTRYSIZE, @EntrySize), SupportsWideChar);

  NameTable := NameTable + EntrySize * Index + 2;
  Result := DecodeAnsiString(AnsiString(NameTable), roUTF8 in Options);
end;

function TJclAnsiRegEx.GetCaptureRange(Index: Integer): TJclCaptureRange;
begin
  if (Index < 0) or (Index >= FCaptureCount) then
    PCRECheck(PCRE_ERROR_NOSUBSTRING, SupportsWideChar)
  else
  begin
    Index := Index * 2;
    Result.FirstPos := TranslateAnsiIndex(FSubject, roUTF8 in Options, FVector^[Index] + 1);
    Result.LastPos := TranslateAnsiIndex(FSubject, roUTF8 in Options, FVector^[Index + 1] + 1) - 1;
  end;
end;

function TJclAnsiRegEx.GetNamedCapture(const Name: string): string;
var
  Index: Integer;
begin
  Index := pcre_get_stringnumber(FCode, PAnsiChar(EncodeAnsiString(Name, roUTF8 in Options)));
  PCRECheck(Index, SupportsWideChar);

  Result := GetCapture(Index);
end;

function TJclAnsiRegEx.IndexOfName(const Name: string): Integer;
begin
  Result := pcre_get_stringnumber(FCode, PAnsiChar(EncodeAnsiString(Name, roUTF8 in Options)));
end;

function TJclAnsiRegEx.IsNameValid(const Name: string): Boolean;
begin
  Result := pcre_get_stringnumber(FCode, PAnsiChar(EncodeAnsiString(Name, roUTF8 in Options))) >= 0;
end;

function TJclAnsiRegEx.Match(const Subject: string; StartOffset: Cardinal): Boolean;
var
  LocalExtra: real_pcre_extra;
  Extra: Pointer;
  WorkSpace: array [0 .. 19] of Integer;
  ExecRslt: Integer;
  EncodedSubject: AnsiString;
begin
  if Assigned(FOnCallout) then
  begin
    if Assigned(FExtra) then
    begin
      LocalExtra.flags := PCRE_EXTRA_STUDY_DATA or PCRE_EXTRA_CALLOUT_DATA;
      LocalExtra.study_data := FExtra;
    end
    else
      LocalExtra.flags := PCRE_EXTRA_CALLOUT_DATA;
    LocalExtra.callout_data := Self;
    Extra := @LocalExtra;
    SetPCRECalloutCallback(JclPCRECallout);
  end
  else
  begin
    Extra := FExtra;
    SetPCRECalloutCallback(nil);
  end;

  FSubject := Subject;
  if Assigned(FChangedCaptures) then
    FChangedCaptures.Clear;
  EncodedSubject := EncodeAnsiString(FSubject, roUTF8 in Options);

  // convert index
  if roUTF8 in Options then
    StartOffset := Length(EncodeAnsiString(Copy(FSubject, 1, StartOffset - 1), True)) + 1;

  if FDfaMode then
  begin
    ExecRslt := pcre_dfa_exec(FCode, Extra, PAnsiChar(EncodedSubject), Length(EncodedSubject),
      StartOffset - 1, GetAPIOptions(True, DfaMode), PInteger(FVector), FVectorSize, @Workspace, 20);
  end
  else
  begin
    ExecRslt := pcre_exec(FCode, Extra, PAnsiChar(EncodedSubject), Length(EncodedSubject),
      StartOffset - 1, GetAPIOptions(True, DfaMode), PInteger(FVector), FVectorSize);
  end;
  Result := ExecRslt >= 0;
  if Result then
  begin
    FCaptureCount := ExecRslt;
    FErrorCode := 0;
  end
  else
  begin
    FErrorCode := ExecRslt;
    if FErrorCode <> PCRE_ERROR_NOMATCH then
      PCRECheck(FErrorCode, SupportsWideChar);
  end;
end;

procedure TJclAnsiRegEx.SetNamedCapture(const Name, Value: string);
var
  Index: Integer;
begin
  Index := pcre_get_stringnumber(FCode, PAnsiChar(EncodeAnsiString(Name, roUTF8 in Options)));
  PCRECheck(Index, SupportsWideChar);

  SetCapture(Index, Value);
end;

{$ENDIF PCRE_8}

{$IFDEF PCRE_16}

//=== { TJclWideRegEx } ======================================================

destructor TJclWideRegEx.Destroy;
begin
  if Assigned(FCode) then
    CallPCRE16Free(FCode);
  if Assigned(FExtra) then
    {$IFDEF PCRE_RTL}
    CallPCRE16Free(FExtra);
    {$ELSE ~PCRE_RTL}
    pcre16_free_study(FExtra);
    {$ENDIF ~PCRE_RTL}
  inherited Destroy;
end;

function TJclWideRegEx.CalloutHandler(var CalloutBlock: pcre16_callout_block): Integer;
begin
  try
    Result := JCL_PCRE_CALLOUT_NOERROR;
    if Assigned(FOnCallout) then
    begin
      with CalloutBlock do
      begin
        FCaptureCount := capture_top;
        FOnCallout(Self, callout_number, start_match + 1, current_position + 1,
          capture_last, pattern_position + 1, next_item_length, Result);
      end;
    end;
  except
    on E: Exception do
    begin
      FErrorMessage := E.Message;
      Result := JCL_PCRE_ERROR_CALLOUTERROR;
    end;
  end;
end;

function TJclWideRegEx.Compile(const Pattern: string; Study, UserLocale, JITCompile: Boolean): Boolean;
var
  ErrMsgPtr: PAnsiChar;
  Tables: PAnsiChar;
  StudyOptions: Integer;
  {$IFNDEF PCRE_RTL}
  ConfigJIT: Integer;
  {$ENDIF ~PCRE_RTL}
begin
  if UserLocale then
  begin
    InitializeLocaleSupport16;
    Tables := GTables16;
  end
  else
    Tables := nil;

  FPattern := Pattern;
  if FPattern = '' then
    PCRECheck(PCRE_ERROR_NULL, SupportsWideChar);

  if Assigned(FCode) then
  begin
    CallPCRE16Free(FCode);
    FCode := nil;
  end;
  FCode := pcre16_compile2(PWideChar(EncodeWideString(FPattern, roUTF16 in Options)), GetAPIOptions(False, DfaMode),
    @FErrorCode, @ErrMsgPtr, @FErrorOffset, Tables);
  Inc(FErrorOffset);
  FErrorMessage := string(AnsiString(ErrMsgPtr));
  Result := Assigned(FCode);
  if Result then
  begin
    if Study then
    begin
      {$IFDEF PCRE_RTL}
      if Assigned(FExtra) then
        CallPCRE16Free(FExtra);
      if JITCompile then
        raise EPCREError.CreateRes(@RsErrNoJITSupport, 0);
      StudyOptions := 0;
      {$ELSE ~PCRE_RTL}
      if Assigned(FExtra) then
        pcre16_free_study(FExtra);
      if JITCompile then
      begin
        PCRECheck(pcre16_config(PCRE_CONFIG_JIT, @ConfigJIT), SupportsWideChar);
        if ConfigJIT = 0 then
          raise EPCREError.CreateRes(@RsErrNoJITSupport, 0);
        StudyOptions := PCRE_STUDY_JIT_COMPILE;
      end
      else
        StudyOptions := 0;
      {$ENDIF ~PCRE_RTL}
      FExtra := pcre16_study(FCode, StudyOptions, @ErrMsgPtr);
      Result := Assigned(FExtra) or (not Assigned(ErrMsgPtr));
      if not Result then
      begin
        FErrorCode := JCL_PCRE_ERROR_STUDYFAILED;
        FErrorMessage := string(AnsiString(ErrMsgPtr));
      end;
    end;

    if FDfaMode then
      FVectorSize := FCaptureCount
    else
    begin
      PCRECheck(pcre16_fullinfo(FCode, FExtra, PCRE_INFO_CAPTURECOUNT, @FCaptureCount), SupportsWideChar);
      FVectorSize := (FCaptureCount + 1) * 3;
    end;
    ReAllocMem(FVector, FVectorSize * SizeOf(FVector[0]));
  end;
end;

function TJclWideRegEx.GetAPIOptions(RunTime, DFA: Boolean): Integer;
var
  ConfigUTF16: Integer;
begin
  PCRECheck(pcre16_config(PCRE_CONFIG_UTF16, @ConfigUTF16), SupportsWideChar);
  if (roUTF16 in Options) and (ConfigUTF16 = 0) then
    PCRECheck(JCL_PCRE_ERROR_NOUTF16, SupportsWideChar);

  Result := inherited GetAPIOptions(RunTime, DFA);
end;

function TJclWideRegEx.GetCapture(Index: Integer): string;
var
  FromPos, ToPos: SizeInt;
begin
  if (Index < 0) or (Index >= FCaptureCount) then
    PCRECheck(PCRE_ERROR_NOSUBSTRING, SupportsWideChar)
  else
  begin
    if FViewChanges and (FChangedCaptures.IndexOf(Pointer(SizeInt(Index))) >= 0) then
    begin
      Result := FResultValues[Index];
      Exit;
    end;

    Index := Index * 2;
    FromPos := TranslateWideIndex(FSubject, roUTF16 in Options, FVector^[Index] + 1);
    ToPos := TranslateWideIndex(FSubject, roUTF16 in Options, FVector^[Index + 1] + 1) - 1;
    Result := Copy(FSubject, FromPos, ToPos - FromPos + 1);
  end;
end;

function TJclWideRegEx.GetCaptureName(Index: Integer): string;
var
  NameTable: PWideChar;
  EntrySize: Integer;
begin
  PCRECheck(pcre16_fullinfo(FCode, FExtra, PCRE_INFO_NAMETABLE, @NameTable), SupportsWideChar);
  PCRECheck(pcre16_fullinfo(FCode, FExtra, PCRE_INFO_NAMEENTRYSIZE, @EntrySize), SupportsWideChar);

  NameTable := NameTable + EntrySize * Index + 1;
  Result := DecodeWideString(WideString(NameTable), roUTF16 in Options);
end;

function TJclWideRegEx.GetCaptureNameCount: Integer;
begin
  PCRECheck(pcre16_fullinfo(FCode, FExtra, PCRE_INFO_NAMECOUNT, @Result), SupportsWideChar);
end;

function TJclWideRegEx.GetCaptureRange(Index: Integer): TJclCaptureRange;
begin
  if (Index < 0) or (Index >= FCaptureCount) then
    PCRECheck(PCRE_ERROR_NOSUBSTRING, SupportsWideChar)
  else
  begin
    Index := Index * 2;
    Result.FirstPos := TranslateWideIndex(FSubject, roUTF16 in Options, FVector^[Index] + 1);
    Result.LastPos := TranslateWideIndex(FSubject, roUTF16 in Options, FVector^[Index + 1] + 1) - 1;
  end;
end;

function TJclWideRegEx.GetNamedCapture(const Name: string): string;
var
  Index: Integer;
begin
  Index := pcre16_get_stringnumber(FCode, PWideChar(EncodeWideString(Name, roUTF16 in Options)));
  PCRECheck(Index, SupportsWideChar);

  Result := GetCapture(Index);
end;

function TJclWideRegEx.IndexOfName(const Name: string): Integer;
begin
  Result := pcre16_get_stringnumber(FCode, PWideChar(EncodeWideString(Name, roUTF16 in Options)));
end;

function TJclWideRegEx.IsNameValid(const Name: string): Boolean;
begin
  Result := pcre16_get_stringnumber(FCode, PWideChar(EncodeWideString(Name, roUTF16 in Options))) >= 0;
end;

function TJclWideRegEx.Match(const Subject: string; StartOffset: Cardinal): Boolean;
var
  LocalExtra: real_pcre16_extra;
  Extra: Pointer;
  WorkSpace: array [0 .. 19] of Integer;
  ExecRslt: Integer;
  EncodedSubject: WideString;
begin
  if Assigned(FOnCallout) then
  begin
    if Assigned(FExtra) then
    begin
      LocalExtra.flags := PCRE_EXTRA_STUDY_DATA or PCRE_EXTRA_CALLOUT_DATA;
      LocalExtra.study_data := FExtra;
    end
    else
      LocalExtra.flags := PCRE_EXTRA_CALLOUT_DATA;
    LocalExtra.callout_data := Self;
    Extra := @LocalExtra;
    SetPCRE16CalloutCallback(JclPCRE16Callout);
  end
  else
  begin
    Extra := FExtra;
    SetPCRE16CalloutCallback(nil);
  end;

  FSubject := Subject;
  if Assigned(FChangedCaptures) then
    FChangedCaptures.Clear;
  EncodedSubject := EncodeWideString(FSubject, roUTF16 in Options);

  // convert index
  if roUTF16 in Options then
    StartOffset := Length(EncodeWideString(Copy(FSubject, 1, StartOffset - 1), True)) + 1;

  if FDfaMode then
  begin
    ExecRslt := pcre16_dfa_exec(FCode, Extra, PWideChar(EncodedSubject), Length(EncodedSubject),
      StartOffset - 1, GetAPIOptions(True, DfaMode), PInteger(FVector), FVectorSize, @Workspace, 20);
  end
  else
  begin
    ExecRslt := pcre16_exec(FCode, Extra, PWideChar(EncodedSubject), Length(EncodedSubject),
      StartOffset - 1, GetAPIOptions(True, DfaMode), PInteger(FVector), FVectorSize);
  end;
  Result := ExecRslt >= 0;
  if Result then
  begin
    FCaptureCount := ExecRslt;
    FErrorCode := 0;
  end
  else
  begin
    FErrorCode := ExecRslt;
    if FErrorCode <> PCRE_ERROR_NOMATCH then
      PCRECheck(FErrorCode, SupportsWideChar);
  end;
end;

procedure TJclWideRegEx.SetNamedCapture(const Name, Value: string);
var
  Index: Integer;
begin
  Index := pcre16_get_stringnumber(FCode, PWideChar(EncodeWideString(Name, roUTF16 in Options)));
  PCRECheck(Index, SupportsWideChar);

  SetCapture(Index, Value);
end;

function TJclWideRegEx.SupportsWideChar: Boolean;
begin
  Result := True;
end;
{$ENDIF PCRE_16}

initialization
  {$IFNDEF PCRE_RTL}
  pcre.LibNotLoadedHandler := LibNotLoadedHandler;
  {$ENDIF ~PCRE_RTL}
  if LoadPCRE then
  begin
    {$IFDEF PCRE_8}
    SetPCREMallocCallback(JclPCREGetMem);
    SetPCREFreeCallback(JclPCREFreeMem);
    {$ENDIF PCRE_8}
    {$IFDEF PCRE_16}
    SetPCRE16MallocCallback(JclPCRE16GetMem);
    SetPCRE16FreeCallback(JclPCRE16FreeMem);
    {$ENDIF PCRE_16}
  end;
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF PCRE_8}
  TerminateLocaleSupport;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  TerminateLocaleSupport16;
  {$ENDIF PCRE_16}
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  UnloadPCRE;

end.

