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
{ The Original Code is JclPRCE.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{ Portions created by University of Cambridge are                                                  }
{ Copyright (C) 1997-2001 by University of Cambridge.                                              }
{                                                                                                  }
{The latest release of PCRE is always available from                                               }
{ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre-xxx.tar.gz                             }
{**************************************************************************************************}
{                                                                                                  }
{ Header conversion of pcre.h                                                                      }
{                                                                                                  }
{ Unit owner: Peter Thornqvist                                                                     }
{ Last modified: April 30, 2004                                                                    }
{                                                                                                  }
{**************************************************************************************************}
//$Id$

unit pcre;

interface
(*************************************************
*       Perl-Compatible Regular Expressions      *
*************************************************)

{$WEAKPACKAGEUNIT ON}

// (p3) this is the switch to change between static and dynamic linking.
// It is set to dynamic by default. To disable simply insert a '.' before the '$'
//
// NOTE: if you enable static linking of DLL, this means that the pcre.dll *must*
// be in the users path or an AV will occur at startup

{$DEFINE PCRE_LINKONREQUEST}
(*$HPPEMIT '#include "pcre.h"'*)

const
  MAX_PATTERN_LENGTH = $10003;
{$EXTERNALSYM MAX_PATTERN_LENGTH}
  MAX_QUANTIFY_REPEAT = $10000;
{$EXTERNALSYM MAX_QUANTIFY_REPEAT}
  MAX_CAPTURE_COUNT = $FFFF;
{$EXTERNALSYM MAX_CAPTURE_COUNT}
  MAX_NESTING_DEPTH = 200;
{$EXTERNALSYM MAX_NESTING_DEPTH}

  (* Options *)
const
  PCRE_CASELESS = $0001;
{$EXTERNALSYM PCRE_CASELESS}
  PCRE_MULTILINE = $0002;
{$EXTERNALSYM PCRE_MULTILINE}
  PCRE_DOTALL = $0004;
{$EXTERNALSYM PCRE_DOTALL}
  PCRE_EXTENDED = $0008;
{$EXTERNALSYM PCRE_EXTENDED}
  PCRE_ANCHORED = $0010;
{$EXTERNALSYM PCRE_ANCHORED}
  PCRE_DOLLAR_ENDONLY = $0020;
{$EXTERNALSYM PCRE_DOLLAR_ENDONLY}
  PCRE_EXTRA = $0040;
{$EXTERNALSYM PCRE_EXTRA}
  PCRE_NOTBOL = $0080;
{$EXTERNALSYM PCRE_NOTBOL}
  PCRE_NOTEOL = $0100;
{$EXTERNALSYM PCRE_NOTEOL}
  PCRE_UNGREEDY = $0200;
{$EXTERNALSYM PCRE_UNGREEDY}
  PCRE_NOTEMPTY = $0400;
{$EXTERNALSYM PCRE_NOTEMPTY}
  PCRE_UTF8 = $0800;
{$EXTERNALSYM PCRE_UTF8}

  (* Exec-time and get-time error codes *)

  PCRE_ERROR_NOMATCH = -1;
{$EXTERNALSYM PCRE_ERROR_NOMATCH}
  PCRE_ERROR_NULL = -2;
{$EXTERNALSYM PCRE_ERROR_NULL}
  PCRE_ERROR_BADOPTION = -3;
{$EXTERNALSYM PCRE_ERROR_BADOPTION}
  PCRE_ERROR_BADMAGIC = -4;
{$EXTERNALSYM PCRE_ERROR_BADMAGIC}
  PCRE_ERROR_UNKNOWN_NODE = -5;
{$EXTERNALSYM PCRE_ERROR_UNKNOWN_NODE}
  PCRE_ERROR_NOMEMORY = -6;
{$EXTERNALSYM PCRE_ERROR_NOMEMORY}
  PCRE_ERROR_NOSUBSTRING = -7;
{$EXTERNALSYM PCRE_ERROR_NOSUBSTRING}

  (* Request types for pcre_fullinfo() *)

  PCRE_INFO_OPTIONS = 0;
{$EXTERNALSYM PCRE_INFO_OPTIONS}
  PCRE_INFO_SIZE = 1;
{$EXTERNALSYM PCRE_INFO_SIZE}
  PCRE_INFO_CAPTURECOUNT = 2;
{$EXTERNALSYM PCRE_INFO_CAPTURECOUNT}
  PCRE_INFO_BACKREFMAX = 3;
{$EXTERNALSYM PCRE_INFO_BACKREFMAX}
  PCRE_INFO_FIRSTCHAR = 4;
{$EXTERNALSYM PCRE_INFO_FIRSTCHAR}
  PCRE_INFO_FIRSTTABLE = 5;
{$EXTERNALSYM PCRE_INFO_FIRSTTABLE}
  PCRE_INFO_LASTLITERAL = 6;
{$EXTERNALSYM PCRE_INFO_LASTLITERAL}

  (* Types *)
type
  PPChar = ^PChar;
  PPPChar = ^PPChar;
  PInteger = ^integer;
  PPointer = ^Pointer;

  real_pcre = record
    magic_number: longword;
    size: integer;
    tables: PChar;
    options: longword;
    top_bracket: word;
    top_backref: word;
    first_char: PChar;
    req_char: PChar;
    code: array[0..0] of char;
  end;
{$EXTERNALSYM real_pcre}
  TPCRE = real_pcre;
  PPCRE = ^TPCRE;

  real_pcre_extra = record
    options: PChar;
    start_bits: array[0..31] of char;
  end;
{$EXTERNALSYM real_pcre_extra}
  TPCREExtra = real_pcre_extra;
  PPCREExtra = ^TPCREExtra;

  (* Functions *)
{$IFNDEF PCRE_LINKONREQUEST}
function pcre_compile(const pattern: PChar; options: integer;
  const errptr: PPChar; erroffset: PInteger; const tableptr: PChar): PPCRE; cdecl;
{$EXTERNALSYM pcre_compile}
function pcre_copy_substring(const subject: PChar; ovector: PInteger; stringcount, stringnumber: integer;
  buffer: PChar; buffersize: integer): integer; cdecl;
{$EXTERNALSYM pcre_copy_substring}
function pcre_exec(const code: PPCRE; const extra: PPCREExtra; const subject: PChar;
{$EXTERNALSYM pcre_exec}
  length, startoffset, options: integer; ovector: PInteger; ovecsize: integer): integer; cdecl;
function pcre_study(const code: PPCRE; options: integer; const errptr: PPChar): PPCREExtra; cdecl;
{$EXTERNALSYM pcre_study}
function pcre_get_substring(const subject: PChar; ovector: PInteger;
{$EXTERNALSYM pcre_get_substring}
  stringcount, stringnumber: integer; const stringptr: PPChar): integer; cdecl;
function pcre_get_substring_list(const subject: PChar; ovector: PInteger;
  stringcount: integer; listptr: PPPChar): integer; cdecl;
{$EXTERNALSYM pcre_get_substring_list}
procedure pcre_free_substring(var stringptr: PChar); cdecl;
{$EXTERNALSYM pcre_free_substring}
procedure pcre_free_substring_list(var stringptr: PChar); cdecl;
{$EXTERNALSYM pcre_free_substring_list}
function pcre_maketables: PChar; cdecl;
{$EXTERNALSYM pcre_maketables}
function pcre_fullinfo(const code: PPCRE; const extra: PPCREExtra;
  what: integer; where: Pointer): integer; cdecl;
{$EXTERNALSYM pcre_fullinfo}
function pcre_info(const code: PPCRE; optptr, firstcharptr: PInteger): integer; cdecl;
{$EXTERNALSYM pcre_info}
function pcre_version: PChar; cdecl;
{$EXTERNALSYM pcre_version}

// Don't use! These does *not* work!!!
function pcre_malloc(Size: integer): Pointer; cdecl;
{$EXTERNALSYM pcre_malloc}
procedure pcre_free(P: Pointer); cdecl;
{$EXTERNALSYM pcre_free}

{$ELSE}
  // dynamic linking
type
  pcre_compile_func = function(const pattern: PChar; options: integer;
    const errptr: PPChar; erroffset: PInteger; const tableptr: PChar): PPCRE; cdecl;
  pcre_copy_substring_func = function(const subject: PChar; ovector: PInteger; stringcount, stringnumber: integer;
    buffer: PChar; buffersize: integer): integer; cdecl;
  pcre_exec_func = function(const code: PPCRE; const extra: PPCREExtra; const subject: PChar;
    length, startoffset, options: integer; ovector: PInteger; ovecsize: integer): integer; cdecl;
  pcre_study_func = function(const code: PPCRE; options: integer; const errptr: PPChar): PPCREExtra; cdecl;
  pcre_get_substring_func = function(const subject: PChar; ovector: PInteger;
    stringcount, stringnumber: integer; const stringptr: PPChar): integer; cdecl;
  pcre_get_substring_list_func = function(const subject: PChar; ovector: PInteger;
    stringcount: integer; listptr: PPPChar): integer; cdecl;
  pcre_free_substring_func = procedure(var stringptr: PChar); cdecl;
  pcre_free_substring_list_func = procedure(var stringptr: PChar); cdecl;
  pcre_maketables_func = function: PChar; cdecl;
  pcre_fullinfo_func = function(const code: PPCRE; const extra: PPCREExtra;
    what: integer; where: Pointer): integer; cdecl;
  pcre_info_func = function(const code: PPCRE; optptr, firstcharptr: PInteger): integer; cdecl;
  pcre_version_func = function: PChar; cdecl;

  pcre_malloc_func = function(Size: integer): Pointer; cdecl;
  pcre_free_func = procedure(P: Pointer); cdecl;
var
  pcre_compile: pcre_compile_func = nil;
{$EXTERNALSYM pcre_compile}
  pcre_copy_substring: pcre_copy_substring_func = nil;
{$EXTERNALSYM pcre_copy_substring}
  pcre_exec: pcre_exec_func = nil;
{$EXTERNALSYM pcre_exec}
  pcre_study: pcre_study_func = nil;
{$EXTERNALSYM pcre_study}
  pcre_get_substring: pcre_get_substring_func = nil;
{$EXTERNALSYM pcre_get_substring}
  pcre_get_substring_list: pcre_get_substring_list_func = nil;
{$EXTERNALSYM pcre_get_substring_list}
  pcre_free_substring: pcre_free_substring_func = nil;
{$EXTERNALSYM pcre_free_substring}
  pcre_free_substring_list: pcre_free_substring_list_func = nil;
{$EXTERNALSYM pcre_free_substring_list}
  pcre_maketables: pcre_maketables_func = nil;
{$EXTERNALSYM pcre_maketables}
  pcre_fullinfo: pcre_fullinfo_func = nil;
{$EXTERNALSYM pcre_fullinfo}
  pcre_info: pcre_info_func = nil;
{$EXTERNALSYM pcre_info}
  pcre_version: pcre_version_func = nil;
{$EXTERNALSYM pcre_version}

  // Don't use! These doesn't work!!!
  pcre_malloc: pcre_malloc_func = nil;
{$EXTERNALSYM pcre_malloc}
  pcre_free: pcre_free_func = nil;
{$EXTERNALSYM pcre_free}

{$ENDIF}

function IsPCRELoaded: Boolean;
function LoadPCRE: Boolean;
procedure UnloadPCRE;

implementation
uses
  Windows;
const
  pcreDLL = 'pcre.dll';

{$IFDEF PCRE_LINKONREQUEST}
var
  PCRELib: THandle = INVALID_HANDLE_VALUE;
{$ENDIF}

function IsPCRELoaded: Boolean;
begin
{$IFDEF PCRE_LINKONREQUEST}
  Result := PCRELib <> INVALID_HANDLE_VALUE;
{$ELSE}
  Result := True;
{$ENDIF}
end;

function LoadPCRE: Boolean;
begin
{$IFDEF PCRE_LINKONREQUEST}
  if PCRELib = INVALID_HANDLE_VALUE then
    PCRELib := LoadLibrary(pcreDLL);
  Result := PCRELib <> INVALID_HANDLE_VALUE;
  if Result then
  begin
    @pcre_compile := GetProcAddress(PCRELib, 'pcre_compile');
    @pcre_copy_substring := GetProcAddress(PCRELib, 'pcre_copy_substring');
    @pcre_exec := GetProcAddress(PCRELib, 'pcre_exec');
    @pcre_study := GetProcAddress(PCRELib, 'pcre_study');
    @pcre_get_substring := GetProcAddress(PCRELib, 'pcre_get_substring');
    @pcre_get_substring_list := GetProcAddress(PCRELib, 'pcre_get_substring_list');
    @pcre_free_substring := GetProcAddress(PCRELib, 'pcre_free_substring');
    @pcre_free_substring_list := GetProcAddress(PCRELib, 'pcre_free_substring_list');
    @pcre_maketables := GetProcAddress(PCRELib, 'pcre_maketables');
    @pcre_fullinfo := GetProcAddress(PCRELib, 'pcre_fullinfo');
    @pcre_info := GetProcAddress(PCRELib, 'pcre_info');
    @pcre_version := GetProcAddress(PCRELib, 'pcre_version');

    @pcre_malloc := GetProcAddress(PCRELib, 'pcre_malloc');
    @pcre_free := GetProcAddress(PCRELib, 'pcre_free');
  end
  else
    UnloadPCRE;
{$ELSE}
  Result := True;
{$ENDIF}
end;

procedure UnloadPCRE;
begin
{$IFDEF PCRE_LINKONREQUEST}
  if PCRELib <> INVALID_HANDLE_VALUE then
    FreeLibrary(PCRELib);
  PCRELib := INVALID_HANDLE_VALUE;
  @pcre_compile := nil;
  @pcre_copy_substring := nil;
  @pcre_exec := nil;
  @pcre_study := nil;
  @pcre_get_substring := nil;
  @pcre_get_substring_list := nil;
  @pcre_free_substring := nil;
  @pcre_free_substring_list := nil;
  @pcre_maketables := nil;
  @pcre_fullinfo := nil;
  @pcre_info := nil;
  @pcre_version := nil;

  @pcre_malloc := nil;
  @pcre_free := nil;
{$ENDIF}
end;

{$IFNDEF PCRE_LINKONREQUEST}
function pcre_compile; external pcreDLL name 'pcre_compile';
function pcre_copy_substring; external pcreDLL name 'pcre_copy_substring';
function pcre_exec; external pcreDLL name 'pcre_exec';
function pcre_study; external pcreDLL name 'pcre_study';
function pcre_get_substring; external pcreDLL name 'pcre_get_substring';
function pcre_get_substring_list; external pcreDLL name 'pcre_get_substring_list';
procedure pcre_free_substring; external pcreDLL name 'pcre_free_substring';
procedure pcre_free_substring_list; external pcreDLL name 'pcre_free_substring_list';
function pcre_maketables; external pcreDLL name 'pcre_maketables';
function pcre_fullinfo; external pcreDLL name 'pcre_fullinfo';
function pcre_info; external pcreDLL name 'pcre_info';
function pcre_version; external pcreDLL name 'pcre_version';
function pcre_malloc; external pcreDLL name 'pcre_malloc';
procedure pcre_free; external pcreDLL name 'pcre_free';
{$ENDIF}

end.

