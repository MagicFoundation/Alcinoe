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
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Class wrapper for PCRE (PERL Compatible Regular Expression)                                      }
{                                                                                                  }
{ Unit owner: Peter Thörnqvist                                                                     }
{ Last modified: April 30, 2004                                                                    }
{                                                                                                  }
{**************************************************************************************************}
//$Id$

unit JclPCRE;

interface
uses
  Windows, Classes, SysUtils;

type
  EPCREError = class(Exception)
  private
    FErrorCode: integer;
  public
    constructor Create(const Msg: AnsiString; ErrorCode: integer);
    property ErrorCode: integer read FErrorCode;
  end;

  TPCREIntArray = array[0..2999] of integer; // 1000 subpatterns should be enough...
  PPCREIntArray = ^TPCREIntArray;

  TJclAnsiRegExOption = (roIgnoreCase, roMultiLine, roDotAll, roExtended,
    roAnchored, roDollarEndOnly, roExtra, roNotBOL, roNotEOL, roUnGreedy, roNotEmpty, roUTF8);
  TJclAnsiRegExOptions = set of TJclAnsiRegExOption;
  TJclAnsiCaptureOffset = record
    FirstPos, LastPos: integer;
  end;

  TJclAnsiRegEx = class
  private
    FCode: Pointer;
    FExtra: Pointer;
    FOptions: TJclAnsiRegExOptions;
    FSubject, FErrorMessage: AnsiString;
    FErrorOffset: integer;
    FVector: TPCREIntArray;
    FStringCount, FVectorSize: integer;
    FTables: PChar;
    function GetCaptureCount: integer;
    function GetCaptures(Index: integer): AnsiString;
    function GetAPIOptions(RunTime: boolean): integer;
    function GetCapturesOffset(Index: integer): TJclAnsiCaptureOffset;
  public
    constructor Create;
    destructor Destroy; override;
    function Compile(const Pattern: AnsiString; Study, UserLocale: boolean): boolean;
    function Match(const Subject: AnsiString; StartOffset: Cardinal = 1): boolean;
    property Options: TJclAnsiRegExOptions read FOptions write FOptions;
    property CaptureCount: integer read GetCaptureCount;
    property Captures[Index: integer]: AnsiString read GetCaptures;
    property CaptureOffset[Index: integer]: TJclAnsiCaptureOffset read GetCapturesOffset;
    property ErrorMessage: AnsiString read FErrorMessage;
    property ErrorOffset: integer read FErrorOffset;
  end;

implementation
uses
  pcre;

resourcestring
  SErrNoMatch = 'No match';
  SErrNull = 'Required value is null';
  SErrBadOption = 'Bad option';
  SErrBadMagic = 'Bad magic';
  SErrUnknownNode = 'Unknown node';
  SErrNoMemory = 'Out of memory';
  SErrNoSubString = 'No substring';

function PCRECheck(Value: integer): boolean;
var
  S: AnsiString;
begin
  Result := false;
  case Value of
    PCRE_ERROR_NOMATCH: S := SErrNoMatch;
    PCRE_ERROR_NULL: S := SErrNull;
    PCRE_ERROR_BADOPTION: S := SErrBadOption;
    PCRE_ERROR_BADMAGIC: S := SErrBadMagic;
    PCRE_ERROR_UNKNOWN_NODE: S := SErrUnknownNode;
    PCRE_ERROR_NOMEMORY: S := SErrNoMemory;
    PCRE_ERROR_NOSUBSTRING: S := SErrNoSubString;
  else
    Result := true;
  end;
  if not Result then
    raise EPCREError.Create(S, Value);
end;

{ TJclAnsiRegEx }

function TJclAnsiRegEx.Compile(const Pattern: AnsiString; Study, UserLocale: boolean): boolean;
var
  errptr: PChar;
  erroffset: Integer;
begin
  if UserLocale then
    FTables := pcre_maketables
  else
    FTables := nil;
  if Pattern = '' then
    raise EPCREError.Create(SErrNull, PCRE_ERROR_NULL);
  FCode := pcre_compile(PChar(Pattern), GetAPIOptions(false), @errptr, @erroffset, FTables);
  FErrorMessage := errptr;
  FErrorOffset := erroffset;
  Result := (FCode <> nil);
  if Result and Study then
    FExtra := pcre_study(FCode, 0, @errptr);
end;

constructor TJclAnsiRegEx.Create;
begin
  inherited Create;
  FVectorSize := sizeof(FVector) div sizeof(integer);
end;

destructor TJclAnsiRegEx.Destroy;
begin
  (*
    if FCode <> nil then
      pcre_free(FCode);
    if FExtra <> nil then
      pcre_free(FExtra);
  *)
  inherited;
end;

function TJclAnsiRegEx.GetAPIOptions(RunTime: boolean): integer;
const
  cDesignOptions: array[TJclAnsiRegExOption] of integer =
  (PCRE_CASELESS, PCRE_MULTILINE, PCRE_DOTALL, PCRE_EXTENDED, PCRE_ANCHORED, PCRE_DOLLAR_ENDONLY,
    PCRE_EXTRA, 0, 0, PCRE_UNGREEDY, 0, PCRE_UTF8);
  cRunOptions: array[TJclAnsiRegExOption] of integer =
  (0, 0, 0, 0, 0, 0,
    0, PCRE_NOTBOL, PCRE_NOTEOL, 0, PCRE_NOTEMPTY, 0);
var
  i: TJclAnsiRegExOption;
begin
  Result := 0;
  if RunTime then
  begin
    for i := Low(TJclAnsiRegExOption) to High(TJclAnsiRegExOption) do
      if (i in Options) then
        Result := Result or cRunOptions[i];
  end
  else
  begin
    for i := Low(TJclAnsiRegExOption) to High(TJclAnsiRegExOption) do
      if (i in Options) then
        Result := Result or cDesignOptions[i];
  end;
end;

function TJclAnsiRegEx.GetCaptureCount: integer;
begin
  Result := FStringCount;
  //  PCRECheck(pcre_fullinfo(FCode, FExtra, PCRE_INFO_CAPTURECOUNT, @Result));
end;

function TJclAnsiRegEx.GetCaptures(Index: integer): AnsiString;
var
  buffer: array[0..1024] of char;
begin
  PCRECheck(pcre_copy_substring(PChar(FSubject), @FVector, FStringCount, Index, buffer, sizeof(buffer)));
  Result := AnsiString(buffer);
end;

function TJclAnsiRegEx.GetCapturesOffset(Index: integer): TJclAnsiCaptureOffset;
begin
  if (Index < 0) or (Index >= FStringCount) then
  begin
    Result.FirstPos := -1;
    Result.LastPos := -1;
  end;
  Result.FirstPos := FVector[Index * 2];
  Result.LastPos := FVector[Index * 2 + 1];
end;

function TJclAnsiRegEx.Match(const Subject: AnsiString; StartOffset: Cardinal = 1): boolean;
begin
  if (FCode = nil) or (Subject = '') then
  begin
    Result := False;
    Exit;
  end;
  if StartOffset < 1 then
    StartOffset := 1;
  FSubject := Subject;
  FStringCount := pcre_exec(FCode, FExtra, PChar(FSubject), Length(FSubject),
    StartOffset - 1, GetAPIOptions(true), @FVector, FVectorSize);
  Result := FStringCount > 0;
end;

{ EPCREError }

constructor EPCREError.Create(const Msg: AnsiString; ErrorCode: integer);
begin
  FErrorCode := ErrorCode;
  inherited Create(Msg);
end;

initialization
  LoadPCRE;
finalization
  UnloadPCRE;

end.

