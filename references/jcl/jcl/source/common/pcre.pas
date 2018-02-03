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
{ The Initial Developer of the Original Code is Peter Thornqvist.                                  }
{ Portions created by Peter Thornqvist are Copyright (C) of Peter Thornqvist. All rights reserved. }
{ Portions created by University of Cambridge are                                                  }
{ Copyright (C) 1997-2001 by University of Cambridge.                                              }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Robert Rossmair (rrossmair)                                                                    }
{   Mario R. Carro                                                                                 }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{ The latest release of PCRE is always available from                                              }
{ ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre-xxx.tar.gz                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Header conversion of pcre.h                                                                      }
{                                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit pcre;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase,
  JclSysUtils;

//DOM-IGNORE-BEGIN

{$IFNDEF PCRE_RTL}

(*************************************************
*       Perl-Compatible Regular Expressions      *
*************************************************)

{$IFDEF SUPPORTS_WEAKPACKAGEUNIT}
  {$IFDEF UNITVERSIONING}
    {$WEAKPACKAGEUNIT OFF}
  {$ELSE ~UNITVERSIONING}
    // d6 and d7 consider initialized variables to be initialization parts which goes against the "weakpackageunit" requirement
    {$IF Defined(PCRE_LINKONREQUEST) and not Defined(COMPILER8_UP)}
      {$WEAKPACKAGEUNIT OFF}
    {$ELSE}
      {$WEAKPACKAGEUNIT ON}
    {$IFEND}
  {$ENDIF ~UNITVERSIONING}
{$ENDIF SUPPORTS_WEAKPACKAGEUNIT}

// (p3) this is the switch to change between static and dynamic linking.
// It is set to dynamic by default. To disable simply insert a '.' before the '$'
//
// NOTE: if you enable static linking of DLL, this means that the pcre.dll *must*
// be in the users path or an AV will occur at startup

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

const
  (* Options *)
  PCRE_CASELESS = $00000001;
  {$EXTERNALSYM PCRE_CASELESS}
  PCRE_MULTILINE = $00000002;
  {$EXTERNALSYM PCRE_MULTILINE}
  PCRE_DOTALL = $00000004;
  {$EXTERNALSYM PCRE_DOTALL}
  PCRE_EXTENDED = $00000008;
  {$EXTERNALSYM PCRE_EXTENDED}
  PCRE_ANCHORED = $00000010;
  {$EXTERNALSYM PCRE_ANCHORED}
  PCRE_DOLLAR_ENDONLY = $00000020;
  {$EXTERNALSYM PCRE_DOLLAR_ENDONLY}
  PCRE_EXTRA = $00000040;
  {$EXTERNALSYM PCRE_EXTRA}
  PCRE_NOTBOL = $00000080;
  {$EXTERNALSYM PCRE_NOTBOL}
  PCRE_NOTEOL = $00000100;
  {$EXTERNALSYM PCRE_NOTEOL}
  PCRE_UNGREEDY = $00000200;
  {$EXTERNALSYM PCRE_UNGREEDY}
  PCRE_NOTEMPTY = $00000400;
  {$EXTERNALSYM PCRE_NOTEMPTY}
  PCRE_UTF8 = $00000800;
  {$EXTERNALSYM PCRE_UTF8}
  PCRE_UTF16 = $00000800;
  {$EXTERNALSYM PCRE_UTF16}
  PCRE_NO_AUTO_CAPTURE = $00001000;
  {$EXTERNALSYM PCRE_NO_AUTO_CAPTURE}
  PCRE_NO_UTF8_CHECK = $00002000;
  {$EXTERNALSYM PCRE_NO_UTF8_CHECK}
  PCRE_NO_UTF16_CHECK = $00002000;
  {$EXTERNALSYM PCRE_NO_UTF16_CHECK}
  PCRE_AUTO_CALLOUT = $00004000;
  {$EXTERNALSYM PCRE_AUTO_CALLOUT}
  PCRE_PARTIAL_SOFT = $00008000;
  {$EXTERNALSYM PCRE_PARTIAL_SOFT}
  PCRE_PARTIAL = PCRE_PARTIAL_SOFT; // Backwards compatible synonym
  {$EXTERNALSYM PCRE_PARTIAL}
  PCRE_DFA_SHORTEST = $00010000;
  {$EXTERNALSYM PCRE_DFA_SHORTEST}
  PCRE_DFA_RESTART = $00020000;
  {$EXTERNALSYM PCRE_DFA_RESTART}
  PCRE_FIRSTLINE = $00040000;
  {$EXTERNALSYM PCRE_FIRSTLINE}
  PCRE_DUPNAMES = $00080000;
  {$EXTERNALSYM PCRE_DUPNAMES}
  PCRE_NEWLINE_CR = $00100000;
  {$EXTERNALSYM PCRE_NEWLINE_CR}
  PCRE_NEWLINE_LF = $00200000;
  {$EXTERNALSYM PCRE_NEWLINE_LF}
  PCRE_NEWLINE_CRLF = $00300000;
  {$EXTERNALSYM PCRE_NEWLINE_CRLF}
  PCRE_NEWLINE_ANY = $00400000;
  {$EXTERNALSYM PCRE_NEWLINE_ANY}
  PCRE_NEWLINE_ANYCRLF = $00500000;
  {$EXTERNALSYM PCRE_NEWLINE_ANYCRLF}
  PCRE_BSR_ANYCRLF = $00800000;
  {$EXTERNALSYM PCRE_BSR_ANYCRLF}
  PCRE_BSR_UNICODE = $01000000;
  {$EXTERNALSYM PCRE_BSR_UNICODE}
  PCRE_JAVASCRIPT_COMPAT = $02000000;
  {$EXTERNALSYM PCRE_JAVASCRIPT_COMPAT}
  PCRE_NO_START_OPTIMIZE = $04000000;
  {$EXTERNALSYM PCRE_NO_START_OPTIMIZE}
  PCRE_NO_START_OPTIMISE = $04000000;
  {$EXTERNALSYM PCRE_NO_START_OPTIMISE}
  PCRE_PARTIAL_HARD = $08000000;
  {$EXTERNALSYM PCRE_PARTIAL_HARD}
  PCRE_NOTEMPTY_ATSTART = $10000000;
  {$EXTERNALSYM PCRE_NOTEMPTY_ATSTART}
  PCRE_UCP = $20000000;
  {$EXTERNALSYM PCRE_UCP}

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
  PCRE_ERROR_MATCHLIMIT = -8;
  {$EXTERNALSYM PCRE_ERROR_MATCHLIMIT}
  PCRE_ERROR_CALLOUT = -9;  (* Never used by PCRE itself *)
  {$EXTERNALSYM PCRE_ERROR_CALLOUT}
  PCRE_ERROR_BADUTF8 = -10;
  {$EXTERNALSYM PCRE_ERROR_BADUTF8}
  PCRE_ERROR_BADUTF16 = -10;
  {$EXTERNALSYM PCRE_ERROR_BADUTF16}
  PCRE_ERROR_BADUTF8_OFFSET = -11;
  {$EXTERNALSYM PCRE_ERROR_BADUTF8_OFFSET}
  PCRE_ERROR_BADUTF16_OFFSET = -11;
  {$EXTERNALSYM PCRE_ERROR_BADUTF16_OFFSET}
  PCRE_ERROR_PARTIAL = -12;
  {$EXTERNALSYM PCRE_ERROR_PARTIAL}
  PCRE_ERROR_BADPARTIAL = -13;
  {$EXTERNALSYM PCRE_ERROR_BADPARTIAL}
  PCRE_ERROR_INTERNAL = -14;
  {$EXTERNALSYM PCRE_ERROR_INTERNAL}
  PCRE_ERROR_BADCOUNT = -15;
  {$EXTERNALSYM PCRE_ERROR_BADCOUNT}
  PCRE_ERROR_DFA_UITEM = -16;
  {$EXTERNALSYM PCRE_ERROR_DFA_UITEM}
  PCRE_ERROR_DFA_UCOND = -17;
  {$EXTERNALSYM PCRE_ERROR_DFA_UCOND}
  PCRE_ERROR_DFA_UMLIMIT = -18;
  {$EXTERNALSYM PCRE_ERROR_DFA_UMLIMIT}
  PCRE_ERROR_DFA_WSSIZE = -19;
  {$EXTERNALSYM PCRE_ERROR_DFA_WSSIZE}
  PCRE_ERROR_DFA_RECURSE = -20;
  {$EXTERNALSYM PCRE_ERROR_DFA_RECURSE}
  PCRE_ERROR_RECURSIONLIMIT = -21;
  {$EXTERNALSYM PCRE_ERROR_RECURSIONLIMIT}
  PCRE_ERROR_NULLWSLIMIT = -22;  (* No longer actually used *)
  {$EXTERNALSYM PCRE_ERROR_NULLWSLIMIT}
  PCRE_ERROR_BADNEWLINE = -23;
  {$EXTERNALSYM PCRE_ERROR_BADNEWLINE}
  PCRE_ERROR_BADOFFSET = -24;
  {$EXTERNALSYM PCRE_ERROR_BADOFFSET}
  PCRE_ERROR_SHORTUTF8 = -25;
  {$EXTERNALSYM PCRE_ERROR_SHORTUTF8}
  PCRE_ERROR_SHORTUTF16 = -25;
  {$EXTERNALSYM PCRE_ERROR_SHORTUTF16}
  PCRE_ERROR_RECURSELOOP = -26;
  {$EXTERNALSYM PCRE_ERROR_RECURSELOOP}
  PCRE_ERROR_JITSTACKLIMIT = -27;
  {$EXTERNALSYM PCRE_ERROR_JITSTACKLIMIT}
  PCRE_ERROR_BADMODE = -28;
  {$EXTERNALSYM PCRE_ERROR_BADMODE}
  PCRE_ERROR_BADENDIANNESS = -29;
  {$EXTERNALSYM PCRE_ERROR_BADENDIANNESS}
  PCRE_ERROR_DFA_BADRESTART = -30;
  {$EXTERNALSYM PCRE_ERROR_DFA_BADRESTART}

  (* Specific error codes for UTF-8 validity checks *)

  PCRE_UTF8_ERR0   =  0;
  PCRE_UTF8_ERR1   =  1;
  PCRE_UTF8_ERR2   =  2;
  PCRE_UTF8_ERR3   =  3;
  PCRE_UTF8_ERR4   =  4;
  PCRE_UTF8_ERR5   =  5;
  PCRE_UTF8_ERR6   =  6;
  PCRE_UTF8_ERR7   =  7;
  PCRE_UTF8_ERR8   =  8;
  PCRE_UTF8_ERR9   =  9;
  PCRE_UTF8_ERR10  = 10;
  PCRE_UTF8_ERR11  = 11;
  PCRE_UTF8_ERR12  = 12;
  PCRE_UTF8_ERR13  = 13;
  PCRE_UTF8_ERR14  = 14;
  PCRE_UTF8_ERR15  = 15;
  PCRE_UTF8_ERR16  = 16;
  PCRE_UTF8_ERR17  = 17;
  PCRE_UTF8_ERR18  = 18;
  PCRE_UTF8_ERR19  = 19;
  PCRE_UTF8_ERR20  = 20;
  PCRE_UTF8_ERR21  = 21;

  (* Specific error codes for UTF-16 validity checks *)

  PCRE_UTF16_ERR0 = 0;
  PCRE_UTF16_ERR1 = 1;
  PCRE_UTF16_ERR2 = 2;
  PCRE_UTF16_ERR3 = 3;
  PCRE_UTF16_ERR4 = 4;

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
  PCRE_INFO_NAMEENTRYSIZE = 7;
  {$EXTERNALSYM PCRE_INFO_NAMEENTRYSIZE}
  PCRE_INFO_NAMECOUNT = 8;
  {$EXTERNALSYM PCRE_INFO_NAMECOUNT}
  PCRE_INFO_NAMETABLE = 9;
  {$EXTERNALSYM PCRE_INFO_NAMETABLE}
  PCRE_INFO_STUDYSIZE = 10;
  {$EXTERNALSYM PCRE_INFO_STUDYSIZE}
  PCRE_INFO_DEFAULT_TABLES = 11;
  {$EXTERNALSYM PCRE_INFO_DEFAULT_TABLES}
  PCRE_INFO_OKPARTIAL = 12;
  {$EXTERNALSYM PCRE_INFO_OKPARTIAL}
  PCRE_INFO_JCHANGED = 13;
  {$EXTERNALSYM PCRE_INFO_JCHANGED}
  PCRE_INFO_HASCRORLF = 14;
  {$EXTERNALSYM PCRE_INFO_HASCRORLF}
  PCRE_INFO_MINLENGTH = 15;
  {$EXTERNALSYM PCRE_INFO_MINLENGTH}
  PCRE_INFO_JIT = 16;
  {$EXTERNALSYM PCRE_INFO_JIT}
  PCRE_INFO_JITSIZE = 17;
  {$EXTERNALSYM PCRE_INFO_JITSIZE}
  PCRE_INFO_MAXLOOKBEHIND = 18;
  {$EXTERNALSYM PCRE_INFO_MAXLOOKBEHIND}

  (* Request types for pcre_config() *)
  PCRE_CONFIG_UTF8 = 0;
  {$EXTERNALSYM PCRE_CONFIG_UTF8}
  PCRE_CONFIG_NEWLINE = 1;
  {$EXTERNALSYM PCRE_CONFIG_NEWLINE}
  PCRE_CONFIG_LINK_SIZE = 2;
  {$EXTERNALSYM PCRE_CONFIG_LINK_SIZE}
  PCRE_CONFIG_POSIX_MALLOC_THRESHOLD = 3;
  {$EXTERNALSYM PCRE_CONFIG_POSIX_MALLOC_THRESHOLD}
  PCRE_CONFIG_MATCH_LIMIT = 4;
  {$EXTERNALSYM PCRE_CONFIG_MATCH_LIMIT}
  PCRE_CONFIG_STACKRECURSE = 5;
  {$EXTERNALSYM PCRE_CONFIG_STACKRECURSE}
  PCRE_CONFIG_UNICODE_PROPERTIES = 6;
  {$EXTERNALSYM PCRE_CONFIG_UNICODE_PROPERTIES}
  PCRE_CONFIG_MATCH_LIMIT_RECURSION = 7;
  {$EXTERNALSYM PCRE_CONFIG_MATCH_LIMIT_RECURSION}
  PCRE_CONFIG_BSR = 8;
  {$EXTERNALSYM PCRE_CONFIG_BSR}
  PCRE_CONFIG_JIT = 9;
  {$EXTERNALSYM PCRE_CONFIG_JIT}
  PCRE_CONFIG_UTF16 = 10;
  {$EXTERNALSYM PCRE_CONFIG_UTF16}
  PCRE_CONFIG_JITTARGET = 11;
  {$EXTERNALSYM PCRE_CONFIG_JITTARGET}

  (* Request types for pcre_study() *)

  PCRE_STUDY_JIT_COMPILE = $0001;
  PCRE_STUDY_JIT_PARTIAL_SOFT_COMPILE = $0002;
  PCRE_STUDY_JIT_PARTIAL_HARD_COMPILE = $0004;

  (* Bit flags for the pcre_extra structure *)

  PCRE_EXTRA_STUDY_DATA = $0001;
  {$EXTERNALSYM PCRE_EXTRA_STUDY_DATA}
  PCRE_EXTRA_MATCH_LIMIT = $0002;
  {$EXTERNALSYM PCRE_EXTRA_MATCH_LIMIT}
  PCRE_EXTRA_CALLOUT_DATA = $0004;
  {$EXTERNALSYM PCRE_EXTRA_CALLOUT_DATA}
  PCRE_EXTRA_TABLES = $0008;
  {$EXTERNALSYM PCRE_EXTRA_TABLES}
  PCRE_EXTRA_MATCH_LIMIT_RECURSION = $0010;
  {$EXTERNALSYM PCRE_EXTRA_MATCH_LIMIT_RECURSION}
  PCRE_EXTRA_MARK = $0020;
  {$EXTERNALSYM PCRE_EXTRA_MARK}
  PCRE_EXTRA_EXECUTABLE_JIT = $0040;
  {$EXTERNALSYM PCRE_EXTRA_EXECUTABLE_JIT}

type
  {$IFDEF PCRE_8}
  real_pcre = packed record
    {magic_number: Longword;
    size: Integer;
    tables: PAnsiChar;
    options: Longword;
    top_bracket: Word;
    top_backref: word;
    first_char: PAnsiChar;
    req_char: PAnsiChar;
    code: array [0..0] of AnsiChar;}
  end;
  TPCRE = real_pcre;
  PPCRE = ^TPCRE;
  {$ENDIF PCRE_8}

  {$IFDEF PCRE_16}
  real_pcre16 = packed record
  end;
  TPCRE16 = real_pcre16;
  PPCRE16 = ^TPCRE16;
  {$ENDIF PCRE_16}

  {$IFDEF PCRE_8}
  real_pcre_jit_stack = packed record
  end;
  TPCREJITStack = real_pcre_jit_stack;
  PPCREJITStack = ^TPCREJITStack;
  {$ENDIF PCRE_8}

  {$IFDEF PCRE_16}
  real_pcre16_jit_stack = packed record
  end;
  TPCRE16JITStack = real_pcre16_jit_stack;
  PPCRE16JITStack = ^TPCRE16JITStack;
  {$ENDIF PCRE_16}

  {$IFDEF PCRE_8}
  real_pcre_extra = packed record
    flags: Cardinal;        (* Bits for which fields are set *)
    study_data: Pointer;    (* Opaque data from pcre_study() *)
    match_limit: Cardinal;  (* Maximum number of calls to match() *)
    callout_data: Pointer;  (* Data passed back in callouts *)
    tables: PAnsiChar;      (* Pointer to character tables *)
    match_limit_recursion: Cardinal; (* Max recursive calls to match() *)
    mark: PPAnsiChar;       (* For passing back a mark pointer *)
    executable_jit: Pointer; (* Contains a pointer to a compiled jit code *)
  end;
  TPCREExtra = real_pcre_extra;
  PPCREExtra = ^TPCREExtra;
  {$ENDIF PCRE_8}

  {$IFDEF PCRE_16}
  real_pcre16_extra = packed record
    flags: Cardinal;        (* Bits for which fields are set *)
    study_data: Pointer;    (* Opaque data from pcre_study() *)
    match_limit: Cardinal;  (* Maximum number of calls to match() *)
    callout_data: Pointer;  (* Data passed back in callouts *)
    tables: PAnsiChar;      (* Pointer to character tables *)
    match_limit_recursion: Cardinal; (* Max recursive calls to match() *)
    mark: PPWideChar;       (* For passing back a mark pointer *)
    executable_jit: Pointer; (* Contains a pointer to a compiled jit code *)
  end;
  TPCRE16Extra = real_pcre16_extra;
  PPCRE16Extra = ^TPCRE16Extra;
  {$ENDIF PCRE_16}

  {$IFDEF PCRE_8}
  pcre_callout_block = packed record
    version: Integer;           (* Identifies version of block *)
  (* ------------------------ Version 0 ------------------------------- *)
    callout_number: Integer;    (* Number compiled into pattern *)
    offset_vector: PInteger;    (* The offset vector *)
    subject: PAnsiChar;         (* The subject being matched *)
    subject_length: Integer;    (* The length of the subject *)
    start_match: Integer;       (* Offset to start of this match attempt *)
    current_position: Integer;  (* Where we currently are in the subject *)
    capture_top: Integer;       (* Max current capture *)
    capture_last: Integer;      (* Most recently closed capture *)
    callout_data: Pointer;      (* Data passed in with the call *)
  (* ------------------- Added for Version 1 -------------------------- *)
    pattern_position: Integer;  (* Offset to next item in the pattern *)
    next_item_length: Integer;  (* Length of next item in the pattern *)
  (* ------------------- Added for Version 2 -------------------------- *)
    Mark: PCardinal;            (* Pointer to current mark or NULL *)
  (* ------------------------------------------------------------------ *)
  end;
  {$ENDIF PCRE_8}

  {$IFDEF PCRE_16}
  pcre16_callout_block = packed record
    version: Integer;           (* Identifies version of block *)
  (* ------------------------ Version 0 ------------------------------- *)
    callout_number: Integer;    (* Number compiled into pattern *)
    offset_vector: PInteger;    (* The offset vector *)
    subject: PWideChar;         (* The subject being matched *)
    subject_length: Integer;    (* The length of the subject *)
    start_match: Integer;       (* Offset to start of this match attempt *)
    current_position: Integer;  (* Where we currently are in the subject *)
    capture_top: Integer;       (* Max current capture *)
    capture_last: Integer;      (* Most recently closed capture *)
    callout_data: Pointer;      (* Data passed in with the call *)
  (* ------------------- Added for Version 1 -------------------------- *)
    pattern_position: Integer;  (* Offset to next item in the pattern *)
    next_item_length: Integer;  (* Length of next item in the pattern *)
  (* ------------------- Added for Version 2 -------------------------- *)
    Mark: PCardinal;            (* Pointer to current mark or NULL *)
  (* ------------------------------------------------------------------ *)
  end;
  {$ENDIF PCRE_16}

  {$IFDEF PCRE_8}
  pcre_malloc_callback = function(Size: SizeInt): Pointer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_malloc_callback}
  pcre_free_callback = procedure(P: Pointer); {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_free_callback}
  pcre_stack_malloc_callback = function(Size: SizeInt): Pointer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_stack_malloc_callback}
  pcre_stack_free_callback = procedure(P: Pointer); {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_stack_free_callback}
  pcre_callout_callback = function(var callout_block: pcre_callout_block): Integer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_callout_callback}
  pcre_jit_callback = function (P: Pointer): PPCREJITStack; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_jit_callback}
  {$ENDIF PCRE_8}

  {$IFDEF PCRE_16}
  pcre16_malloc_callback = function(Size: SizeInt): Pointer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_malloc_callback}
  pcre16_free_callback = procedure(P: Pointer); {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_free_callback}
  pcre16_stack_malloc_callback = function(Size: SizeInt): Pointer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_stack_malloc_callback}
  pcre16_stack_free_callback = procedure(P: Pointer); {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_stack_free_callback}
  pcre16_callout_callback = function(var callout_block: pcre16_callout_block): Integer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_callout_callback}
  pcre16_jit_callback = function (P: Pointer): PPCRE16JITStack; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_jit_callback}
  {$ENDIF PCRE_16}

var
  {$IFDEF PCRE_8}
  // renamed from "pcre_X" to "pcre_X_func" to allow functions with name "pcre_X" to be
  // declared in implementation when static linked
  pcre_malloc_func: ^pcre_malloc_callback = nil;
  {$EXTERNALSYM pcre_malloc_func}
  pcre_free_func: ^pcre_free_callback = nil;
  {$EXTERNALSYM pcre_free_func}
  pcre_stack_malloc_func: ^pcre_stack_malloc_callback = nil;
  {$EXTERNALSYM pcre_stack_malloc_func}
  pcre_stack_free_func: ^pcre_stack_free_callback = nil;
  {$EXTERNALSYM pcre_stack_free_func}
  pcre_callout_func: ^pcre_callout_callback = nil;
  {$EXTERNALSYM pcre_callout_func}
  {$ENDIF PCRE_8}

  {$IFDEF PCRE_16}
  pcre16_malloc_func: ^pcre16_malloc_callback = nil;
  {$EXTERNALSYM pcre16_malloc_func}
  pcre16_free_func: ^pcre16_free_callback = nil;
  {$EXTERNALSYM pcre16_free_func}
  pcre16_stack_malloc_func: ^pcre16_stack_malloc_callback = nil;
  {$EXTERNALSYM pcre16_stack_malloc_func}
  pcre16_stack_free_func: ^pcre16_stack_free_callback = nil;
  {$EXTERNALSYM pcre16_stack_free_func}
  pcre16_callout_func: ^pcre16_callout_callback = nil;
  {$EXTERNALSYM pcre16_callout_func}
  {$ENDIF PCRE_16}

{$IFDEF PCRE_8}

procedure SetPCREMallocCallback(const Value: pcre_malloc_callback);
{$EXTERNALSYM SetPCREMallocCallback}
function GetPCREMallocCallback: pcre_malloc_callback;
{$EXTERNALSYM GetPCREMallocCallback}
function CallPCREMalloc(Size: SizeInt): Pointer;
{$EXTERNALSYM CallPCREMalloc}

procedure SetPCREFreeCallback(const Value: pcre_free_callback);
{$EXTERNALSYM SetPCREFreeCallback}
function GetPCREFreeCallback: pcre_free_callback;
{$EXTERNALSYM GetPCREFreeCallback}
procedure CallPCREFree(P: Pointer);
{$EXTERNALSYM CallPCREFree}

procedure SetPCREStackMallocCallback(const Value: pcre_stack_malloc_callback);
{$EXTERNALSYM SetPCREStackMallocCallback}
function GetPCREStackMallocCallback: pcre_stack_malloc_callback;
{$EXTERNALSYM GetPCREStackMallocCallback}
function CallPCREStackMalloc(Size: SizeInt): Pointer;
{$EXTERNALSYM CallPCREStackMalloc}

procedure SetPCREStackFreeCallback(const Value: pcre_stack_free_callback);
{$EXTERNALSYM SetPCREStackFreeCallback}
function GetPCREStackFreeCallback: pcre_stack_free_callback;
{$EXTERNALSYM GetPCREStackFreeCallback}
procedure CallPCREStackFree(P: Pointer);
{$EXTERNALSYM CallPCREStackFree}

procedure SetPCRECalloutCallback(const Value: pcre_callout_callback);
{$EXTERNALSYM SetPCRECalloutCallback}
function GetPCRECalloutCallback: pcre_callout_callback;
{$EXTERNALSYM GetPCRECalloutCallback}
function CallPCRECallout(var callout_block: pcre_callout_block): Integer;
{$EXTERNALSYM CallPCRECallout}

{$ENDIF PCRE_8}

{$IFDEF PCRE_16}

procedure SetPCRE16MallocCallback(const Value: pcre16_malloc_callback);
{$EXTERNALSYM SetPCRE16MallocCallback}
function GetPCRE16MallocCallback: pcre16_malloc_callback;
{$EXTERNALSYM GetPCRE16MallocCallback}
function CallPCRE16Malloc(Size: SizeInt): Pointer;
{$EXTERNALSYM CallPCRE16Malloc}

procedure SetPCRE16FreeCallback(const Value: pcre16_free_callback);
{$EXTERNALSYM SetPCRE16FreeCallback}
function GetPCRE16FreeCallback: pcre16_free_callback;
{$EXTERNALSYM GetPCRE16FreeCallback}
procedure CallPCRE16Free(P: Pointer);
{$EXTERNALSYM CallPCRE16Free}

procedure SetPCRE16StackMallocCallback(const Value: pcre16_stack_malloc_callback);
{$EXTERNALSYM SetPCRE16StackMallocCallback}
function GetPCRE16StackMallocCallback: pcre16_stack_malloc_callback;
{$EXTERNALSYM GetPCRE16StackMallocCallback}
function CallPCRE16StackMalloc(Size: SizeInt): Pointer;
{$EXTERNALSYM CallPCRE16StackMalloc}

procedure SetPCRE16StackFreeCallback(const Value: pcre16_stack_free_callback);
{$EXTERNALSYM SetPCRE16StackFreeCallback}
function GetPCRE16StackFreeCallback: pcre16_stack_free_callback;
{$EXTERNALSYM GetPCRE16StackFreeCallback}
procedure CallPCRE16StackFree(P: Pointer);
{$EXTERNALSYM CallPCRE16StackFree}

procedure SetPCRE16CalloutCallback(const Value: pcre16_callout_callback);
{$EXTERNALSYM SetPCRE16CalloutCallback}
function GetPCRE16CalloutCallback: pcre16_callout_callback;
{$EXTERNALSYM GetPCRE16CalloutCallback}
function CallPCRE16Callout(var callout_block: pcre16_callout_block): Integer;
{$EXTERNALSYM CallPCRE16Callout}

{$ENDIF PCRE_16}

type
  TPCRELibNotLoadedHandler = procedure; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  PPPWideChar = ^PPWideChar;

var
  // Value to initialize function pointers below with, in case LoadPCRE fails
  // or UnloadPCRE is called.  Typically the handler will raise an exception.
  LibNotLoadedHandler: TPCRELibNotLoadedHandler = nil;

(* Functions *)

{$IFNDEF PCRE_LINKONREQUEST}
// static link and static dll import
{$IFDEF PCRE_8}
function pcre_compile(const pattern: PAnsiChar; options: Integer;
  const errptr: PPAnsiChar; erroffset: PInteger; const tableptr: PAnsiChar): PPCRE;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_compile}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
function pcre16_compile(const pattern: PWideChar; options: Integer;
  const errptr: PPAnsiChar; erroffset: PInteger; const tableptr: PAnsiChar): PPCRE16;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_compile}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
function pcre_compile2(const pattern: PAnsiChar; options: Integer;
  const errorcodeptr: PInteger; const errorptr: PPAnsiChar; erroroffset: PInteger;
  const tables: PAnsiChar): PPCRE;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_compile2}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
function pcre16_compile2(const pattern: PWideChar; options: Integer;
  const errorcodeptr: PInteger; const errorptr: PPAnsiChar; erroroffset: PInteger;
  const tables: PAnsiChar): PPCRE16;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_compile2}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
function pcre_config(what: Integer; where: Pointer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_config}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
function pcre16_config(what: Integer; where: Pointer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_config}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
function pcre_copy_named_substring(const code: PPCRE; const subject: PAnsiChar;
  ovector: PInteger; stringcount: Integer; const stringname: PAnsiChar;
  buffer: PAnsiChar; size: Integer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_copy_named_substring}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
function pcre16_copy_named_substring(const code: PPCRE16; const subject: PWideChar;
  ovector: PInteger; stringcount: Integer; const stringname: PWideChar;
  buffer: PWideChar; size: Integer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_copy_named_substring}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
function pcre_copy_substring(const subject: PAnsiChar; ovector: PInteger;
  stringcount, stringnumber: Integer; buffer: PAnsiChar; buffersize: Integer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_copy_substring}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
function pcre16_copy_substring(const subject: PWideChar; ovector: PInteger;
  stringcount, stringnumber: Integer; buffer: PWideChar; buffersize: Integer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_copy_substring}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
function pcre_dfa_exec(const argument_re: PPCRE; const extra_data: PPCREExtra;
  const subject: PAnsiChar; length: Integer; start_offset: Integer;
  options: Integer; offsets: PInteger; offsetcount: Integer; workspace: PInteger;
  wscount: Integer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_dfa_exec}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
function pcre16_dfa_exec(const argument_re: PPCRE16; const extra_data: PPCRE16Extra;
  const subject: PWideChar; length: Integer; start_offset: Integer;
  options: Integer; offsets: PInteger; offsetcount: Integer; workspace: PInteger;
  wscount: Integer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_dfa_exec}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
function pcre_exec(const code: PPCRE; const extra: PPCREExtra; const subject: PAnsiChar;
  length, startoffset, options: Integer; ovector: PInteger; ovecsize: Integer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_exec}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
function pcre16_exec(const code: PPCRE16; const extra: PPCRE16Extra; const subject: PWideChar;
  length, startoffset, options: Integer; ovector: PInteger; ovecsize: Integer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_exec}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
procedure pcre_free_substring(stringptr: PAnsiChar);
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_free_substring}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
procedure pcre16_free_substring(stringptr: PWideChar);
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_free_substring}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
procedure pcre_free_substring_list(stringlistptr: PPAnsiChar);
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_free_substring_list}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
procedure pcre16_free_substring_list(stringlistptr: PPWideChar);
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_free_substring_list}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
function pcre_fullinfo(const code: PPCRE; const extra: PPCREExtra;
  what: Integer; where: Pointer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_fullinfo}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
function pcre16_fullinfo(const code: PPCRE16; const extra: PPCRE16Extra;
  what: Integer; where: Pointer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_fullinfo}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
function pcre_get_named_substring(const code: PPCRE; const subject: PAnsiChar;
  ovector: PInteger; stringcount: Integer; const stringname: PAnsiChar;
  const stringptr: PPAnsiChar): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_get_named_substring}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
function pcre16_get_named_substring(const code: PPCRE16; const subject: PWideChar;
  ovector: PInteger; stringcount: Integer; const stringname: PWideChar;
  const stringptr: PPWideChar): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_get_named_substring}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
function pcre_get_stringnumber(const code: PPCRE; const stringname: PAnsiChar): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_get_stringnumber}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
function pcre16_get_stringnumber(const code: PPCRE16; const stringname: PWideChar): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_get_stringnumber}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
function pcre_get_stringtable_entries(const code: PPCRE; const stringname: PAnsiChar;
  firstptr: PPAnsiChar; lastptr: PPAnsiChar): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_get_stringtable_entries}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
function pcre16_get_stringtable_entries(const code: PPCRE16; const stringname: PWideChar;
  firstptr: PPWideChar; lastptr: PPWideChar): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_get_stringtable_entries}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
function pcre_get_substring(const subject: PAnsiChar; ovector: PInteger;
  stringcount, stringnumber: Integer; const stringptr: PPAnsiChar): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_get_substring}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
function pcre16_get_substring(const subject: PWideChar; ovector: PInteger;
  stringcount, stringnumber: Integer; const stringptr: PPWideChar): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_get_substring}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
function pcre_get_substring_list(const subject: PAnsiChar; ovector: PInteger;
  stringcount: Integer; listptr: PPPAnsiChar): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_get_substring_list}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
function pcre16_get_substring_list(const subject: PWideChar; ovector: PInteger;
  stringcount: Integer; listptr: PPPWideChar): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_get_substring_list}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
function pcre_maketables: PAnsiChar;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_maketables}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
function pcre16_maketables: PAnsiChar;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_maketables}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
function pcre_refcount(argument_re: PPCRE; adjust: Integer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_refcount}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
function pcre16_refcount(argument_re: PPCRE16; adjust: Integer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_refcount}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
function pcre_study(const code: PPCRE; options: Integer; const errptr: PPAnsiChar): PPCREExtra;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_study}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
function pcre16_study(const code: PPCRE16; options: Integer; const errptr: PPAnsiChar): PPCRE16Extra;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_study}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
procedure pcre_free_study(const extra: PPCREExtra);
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_free_study}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
procedure pcre16_free_study(const extra: PPCRE16Extra);
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_free_study}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
function pcre_version: PAnsiChar; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_version}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
function pcre16_version: PAnsiChar; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_version}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
function pcre_jit_stack_alloc(startsize, maxsize: Integer): PPCREJITStack; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_jit_stack_alloc}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
function pcre16_jit_stack_alloc(startsize, maxsize: Integer): PPCRE16JITStack; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_jit_stack_alloc}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
procedure pcre_jit_stack_free(stack: PPCREJITStack); {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_jit_stack_free}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
procedure pcre16_jit_stack_free(stack: PPCRE16JITStack); {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_jit_stack_free}
{$ENDIF PCRE_16}
{$IFDEF PCRE_8}
procedure pcre_assign_jit_stack(extra: PPCREExtra; callback: pcre_jit_callback; userdata: Pointer); {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_assign_jit_stack}
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
procedure pcre16_assign_jit_stack(extra: PPCRE16Extra; callback: pcre16_jit_callback; userdata: Pointer); {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre16_assign_jit_stack}
{$ENDIF PCRE_16}

{$ELSE PCRE_LINKONREQUEST}

// dynamic dll import
type
  {$IFDEF PCRE_8}
  pcre_compile_func = function(const pattern: PAnsiChar; options: Integer;
    const errptr: PPAnsiChar; erroffset: PInteger; const tableptr: PAnsiChar): PPCRE;
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_compile_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_compile_func = function(const pattern: PWideChar; options: Integer;
    const errptr: PPAnsiChar; erroffset: PInteger; const tableptr: PAnsiChar): PPCRE16;
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_compile_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_compile2_func = function(const pattern: PAnsiChar; options: Integer;
    const errorcodeptr: PInteger; const errorptr: PPAnsiChar; erroroffset: PInteger;
    const tables: PAnsiChar): PPCRE; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_compile2_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_compile2_func = function(const pattern: PWideChar; options: Integer;
    const errorcodeptr: PInteger; const errorptr: PPAnsiChar; erroroffset: PInteger;
    const tables: PAnsiChar): PPCRE16; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_compile2_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_config_func = function(what: Integer; where: Pointer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_config_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_config_func = function(what: Integer; where: Pointer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_config_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_copy_named_substring_func = function(const code: PPCRE; const subject: PAnsiChar;
    ovector: PInteger; stringcount: Integer; const stringname: PAnsiChar;
    buffer: PAnsiChar; size: Integer): Integer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_copy_named_substring_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_copy_named_substring_func = function(const code: PPCRE16; const subject: PWideChar;
    ovector: PInteger; stringcount: Integer; const stringname: PWideChar;
    buffer: PWideChar; size: Integer): Integer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_copy_named_substring_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_copy_substring_func = function(const subject: PAnsiChar; ovector: PInteger;
    stringcount, stringnumber: Integer; buffer: PAnsiChar; buffersize: Integer): Integer;
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_copy_substring_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_copy_substring_func = function(const subject: PWideChar; ovector: PInteger;
    stringcount, stringnumber: Integer; buffer: PWideChar; buffersize: Integer): Integer;
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_copy_substring_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_dfa_exec_func = function(const argument_re: PPCRE; const extra_data: PPCREExtra;
    const subject: PAnsiChar; length: Integer; start_offset: Integer;
    options: Integer; offsets: PInteger; offsetcount: Integer; workspace: PInteger;
    wscount: Integer): Integer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_dfa_exec_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_dfa_exec_func = function(const argument_re: PPCRE16; const extra_data: PPCRE16Extra;
    const subject: PWideChar; length: Integer; start_offset: Integer;
    options: Integer; offsets: PInteger; offsetcount: Integer; workspace: PInteger;
    wscount: Integer): Integer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_dfa_exec_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_exec_func = function(const code: PPCRE; const extra: PPCREExtra; const subject: PAnsiChar;
    length, startoffset, options: Integer; ovector: PInteger; ovecsize: Integer): Integer;
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_exec_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_exec_func = function(const code: PPCRE16; const extra: PPCRE16Extra; const subject: PWideChar;
    length, startoffset, options: Integer; ovector: PInteger; ovecsize: Integer): Integer;
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_exec_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_free_substring_func = procedure(stringptr: PAnsiChar);
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_free_substring_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_free_substring_func = procedure(stringptr: PWideChar);
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_free_substring_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_free_substring_list_func = procedure(stringptr: PPAnsiChar);
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_free_substring_list_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_free_substring_list_func = procedure(stringptr: PPWideChar);
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_free_substring_list_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_fullinfo_func = function(const code: PPCRE; const extra: PPCREExtra;
    what: Integer; where: Pointer): Integer;
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_fullinfo_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_fullinfo_func = function(const code: PPCRE16; const extra: PPCRE16Extra;
    what: Integer; where: Pointer): Integer;
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_fullinfo_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_get_named_substring_func = function(const code: PPCRE; const subject: PAnsiChar;
    ovector: PInteger; stringcount: Integer; const stringname: PAnsiChar;
    const stringptr: PPAnsiChar): Integer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_get_named_substring_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_get_named_substring_func = function(const code: PPCRE16; const subject: PWideChar;
    ovector: PInteger; stringcount: Integer; const stringname: PWideChar;
    const stringptr: PPWideChar): Integer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_get_named_substring_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_get_stringnumber_func = function(const code: PPCRE;
    const stringname: PAnsiChar): Integer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_get_stringnumber_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_get_stringnumber_func = function(const code: PPCRE16;
    const stringname: PWideChar): Integer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_get_stringnumber_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_get_stringtable_entries_func = function(const code: PPCRE; const stringname: PAnsiChar;
    firstptr: PPAnsiChar; lastptr: PPAnsiChar): Integer;
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_get_stringtable_entries_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_get_stringtable_entries_func = function(const code: PPCRE16; const stringname: PWideChar;
    firstptr: PPWideChar; lastptr: PPWideChar): Integer;
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_get_stringtable_entries_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_get_substring_func = function(const subject: PAnsiChar; ovector: PInteger;
    stringcount, stringnumber: Integer; const stringptr: PPAnsiChar): Integer;
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_get_substring_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_get_substring_func = function(const subject: PWideChar; ovector: PInteger;
    stringcount, stringnumber: Integer; const stringptr: PPWideChar): Integer;
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_get_substring_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_get_substring_list_func = function(const subject: PAnsiChar; ovector: PInteger;
    stringcount: Integer; listptr: PPPAnsiChar): Integer;
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_get_substring_list_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_get_substring_list_func = function(const subject: PWideChar; ovector: PInteger;
    stringcount: Integer; listptr: PPPWideChar): Integer;
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_get_substring_list_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_maketables_func = function: PAnsiChar; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_maketables_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_maketables_func = function: PAnsiChar; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_maketables_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_refcount_func = function(argument_re: PPCRE; adjust: Integer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_refcount_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_refcount_func = function(argument_re: PPCRE16; adjust: Integer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_refcount_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_study_func = function(const code: PPCRE; options: Integer; const errptr: PPAnsiChar): PPCREExtra;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_study_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_study_func = function(const code: PPCRE16; options: Integer; const errptr: PPAnsiChar): PPCRE16Extra;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_study_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_free_study_func = procedure (const extra: PPCREExtra);
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_free_study_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_free_study_func = procedure (const extra: PPCRE16Extra);
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_free_study_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_version_func = function: PAnsiChar; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_version_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_version_func = function: PAnsiChar; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_version_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_jit_stack_alloc_func = function (startsize, maxsize: Integer): PPCREJITStack;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_jit_stack_alloc_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_jit_stack_alloc_func = function (startsize, maxsize: Integer): PPCRE16JITStack;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_jit_stack_alloc_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_jit_stack_free_func = procedure (stack: PPCREJITStack);
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_jit_stack_free_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_jit_stack_free_func = procedure (stack: PPCRE16JITStack);
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_jit_stack_free_func}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_assign_jit_stack_func = procedure (extra: PPCREExtra; callback: pcre_jit_callback; userdata: Pointer);
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_assign_jit_stack_func}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_assign_jit_stack_func = procedure (extra: PPCRE16Extra; callback: pcre16_jit_callback; userdata: Pointer);
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre16_assign_jit_stack_func}
  {$ENDIF PCRE_16}

var
  {$IFDEF PCRE_8}
  pcre_compile: pcre_compile_func = nil;
  {$EXTERNALSYM pcre_compile}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_compile: pcre16_compile_func = nil;
  {$EXTERNALSYM pcre16_compile}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_compile2: pcre_compile2_func = nil;
  {$EXTERNALSYM pcre_compile2}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_compile2: pcre16_compile2_func = nil;
  {$EXTERNALSYM pcre16_compile2}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_config: pcre_config_func = nil;
  {$EXTERNALSYM pcre_config}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_config: pcre16_config_func = nil;
  {$EXTERNALSYM pcre16_config}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_copy_named_substring: pcre_copy_named_substring_func = nil;
  {$EXTERNALSYM pcre_copy_named_substring}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_copy_named_substring: pcre16_copy_named_substring_func = nil;
  {$EXTERNALSYM pcre16_copy_named_substring}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_copy_substring: pcre_copy_substring_func = nil;
  {$EXTERNALSYM pcre_copy_substring}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_copy_substring: pcre16_copy_substring_func = nil;
  {$EXTERNALSYM pcre16_copy_substring}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_dfa_exec: pcre_dfa_exec_func = nil;
  {$EXTERNALSYM pcre_dfa_exec}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_dfa_exec: pcre16_dfa_exec_func = nil;
  {$EXTERNALSYM pcre16_dfa_exec}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_exec: pcre_exec_func = nil;
  {$EXTERNALSYM pcre_exec}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_exec: pcre16_exec_func = nil;
  {$EXTERNALSYM pcre16_exec}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_free_substring: pcre_free_substring_func = nil;
  {$EXTERNALSYM pcre_free_substring}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_free_substring: pcre16_free_substring_func = nil;
  {$EXTERNALSYM pcre16_free_substring}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_free_substring_list: pcre_free_substring_list_func = nil;
  {$EXTERNALSYM pcre_free_substring_list}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_free_substring_list: pcre16_free_substring_list_func = nil;
  {$EXTERNALSYM pcre16_free_substring_list}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_fullinfo: pcre_fullinfo_func = nil;
  {$EXTERNALSYM pcre_fullinfo}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_fullinfo: pcre16_fullinfo_func = nil;
  {$EXTERNALSYM pcre16_fullinfo}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_get_named_substring: pcre_get_named_substring_func = nil;
  {$EXTERNALSYM pcre_get_named_substring}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_get_named_substring: pcre16_get_named_substring_func = nil;
  {$EXTERNALSYM pcre16_get_named_substring}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_get_stringnumber: pcre_get_stringnumber_func = nil;
  {$EXTERNALSYM pcre_get_stringnumber}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_get_stringnumber: pcre16_get_stringnumber_func = nil;
  {$EXTERNALSYM pcre16_get_stringnumber}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_get_stringtable_entries: pcre_get_stringtable_entries_func = nil;
  {$EXTERNALSYM pcre_get_stringtable_entries}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_get_stringtable_entries: pcre16_get_stringtable_entries_func = nil;
  {$EXTERNALSYM pcre16_get_stringtable_entries}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_get_substring: pcre_get_substring_func = nil;
  {$EXTERNALSYM pcre_get_substring}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_get_substring: pcre16_get_substring_func = nil;
  {$EXTERNALSYM pcre16_get_substring}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_get_substring_list: pcre_get_substring_list_func = nil;
  {$EXTERNALSYM pcre_get_substring_list}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_get_substring_list: pcre16_get_substring_list_func = nil;
  {$EXTERNALSYM pcre16_get_substring_list}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_maketables: pcre_maketables_func = nil;
  {$EXTERNALSYM pcre_maketables}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_maketables: pcre16_maketables_func = nil;
  {$EXTERNALSYM pcre16_maketables}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_refcount: pcre_refcount_func = nil;
  {$EXTERNALSYM pcre_refcount}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_refcount: pcre16_refcount_func = nil;
  {$EXTERNALSYM pcre16_refcount}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_study: pcre_study_func = nil;
  {$EXTERNALSYM pcre_study}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_study: pcre16_study_func = nil;
  {$EXTERNALSYM pcre16_study}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_free_study: pcre_free_study_func = nil;
  {$EXTERNALSYM pcre_free_study}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_free_study: pcre16_free_study_func = nil;
  {$EXTERNALSYM pcre16_free_study}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_version: pcre_version_func = nil;
  {$EXTERNALSYM pcre_version}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_version: pcre16_version_func = nil;
  {$EXTERNALSYM pcre16_version}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_jit_stack_alloc: pcre_jit_stack_alloc_func = nil;
  {$EXTERNALSYM pcre_jit_stack_alloc}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_jit_stack_alloc: pcre16_jit_stack_alloc_func = nil;
  {$EXTERNALSYM pcre16_jit_stack_alloc}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_jit_stack_free: pcre_jit_stack_free_func = nil;
  {$EXTERNALSYM pcre_jit_stack_free}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_jit_stack_free: pcre16_jit_stack_free_func = nil;
  {$EXTERNALSYM pcre16_jit_stack_free}
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  pcre_assign_jit_stack: pcre_assign_jit_stack_func = nil;
  {$EXTERNALSYM pcre_assign_jit_stack}
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_assign_jit_stack: pcre16_assign_jit_stack_func = nil;
  {$EXTERNALSYM pcre16_assign_jit_stack}
  {$ENDIF PCRE_16}

{$ENDIF PCRE_LINKONREQUEST}

{$ENDIF ~PCRE_RTL}
//DOM-IGNORE-END

const
  {$IFDEF MSWINDOWS}
  PCREDefaultLibraryName = 'pcre3.dll';
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  PCREDefaultLibraryName = 'libpcre.so.0';
  {$ENDIF UNIX}
  {$IFDEF PCRE_8}
  PCRECompileDefaultExportName = 'pcre_compile';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16CompileDefaultExportName = 'pcre16_compile';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCRECompile2DefaultExportName = 'pcre_compile2';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16Compile2DefaultExportName = 'pcre16_compile2';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREConfigDefaultExportName = 'pcre_config';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16ConfigDefaultExportName = 'pcre16_config';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCRECopyNamedSubstringDefaultExportName = 'pcre_copy_named_substring';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16CopyNamedSubstringDefaultExportName = 'pcre16_copy_named_substring';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCRECopySubStringDefaultExportName = 'pcre_copy_substring';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16CopySubStringDefaultExportName = 'pcre16_copy_substring';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREDfaExecDefaultExportName = 'pcre_dfa_exec';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16DfaExecDefaultExportName = 'pcre16_dfa_exec';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREExecDefaultExportName = 'pcre_exec';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16ExecDefaultExportName = 'pcre16_exec';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREFreeSubStringDefaultExportName = 'pcre_free_substring';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16FreeSubStringDefaultExportName = 'pcre16_free_substring';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREFreeSubStringListDefaultExportName = 'pcre_free_substring_list';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16FreeSubStringListDefaultExportName = 'pcre16_free_substring_list';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREFullInfoDefaultExportName = 'pcre_fullinfo';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16FullInfoDefaultExportName = 'pcre16_fullinfo';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREGetNamedSubstringDefaultExportName = 'pcre_get_named_substring';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16GetNamedSubstringDefaultExportName = 'pcre16_get_named_substring';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREGetStringNumberDefaultExportName = 'pcre_get_stringnumber';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16GetStringNumberDefaultExportName = 'pcre16_get_stringnumber';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREGetStringTableEntriesDefaultExportName = 'pcre_get_stringtable_entries';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16GetStringTableEntriesDefaultExportName = 'pcre16_get_stringtable_entries';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREGetSubStringDefaultExportName = 'pcre_get_substring';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16GetSubStringDefaultExportName = 'pcre16_get_substring';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREGetSubStringListDefaultExportName = 'pcre_get_substring_list';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16GetSubStringListDefaultExportName = 'pcre16_get_substring_list';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREMakeTablesDefaultExportName = 'pcre_maketables';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16MakeTablesDefaultExportName = 'pcre16_maketables';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCRERefCountDefaultExportName = 'pcre_refcount';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16RefCountDefaultExportName = 'pcre16_refcount';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREStudyDefaultExportName = 'pcre_study';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16StudyDefaultExportName = 'pcre16_study';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREFreeStudyDefaultExportName = 'pcre_free_study';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16FreeStudyDefaultExportName = 'pcre16_free_study';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREVersionDefaultExportName = 'pcre_version';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16VersionDefaultExportName = 'pcre16_version';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREJITStackAllocDefaultExportName = 'pcre_jit_stack_alloc';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16JITStackAllocDefaultExportName = 'pcre16_jit_stack_alloc';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREJITStackFreeDefaultExportName = 'pcre_jit_stack_free';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16JITStackFreeDefaultExportName = 'pcre16_jit_stack_free';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREAssignJITStackDefaultExportName = 'pcre_assign_jit_stack';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16AssignJITStackDefaultExportName = 'pcre16_assign_jit_stack';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREMallocDefaultExportName = 'pcre_malloc';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16MallocDefaultExportName = 'pcre16_malloc';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREFreeDefaultExportName = 'pcre_free';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16FreeDefaultExportName = 'pcre16_free';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREStackMallocDefaultExportName = 'pcre_stack_malloc';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16StackMallocDefaultExportName = 'pcre16_stack_malloc';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREStackFreeDefaultExportName = 'pcre_stack_free';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16StackFreeDefaultExportName = 'pcre16_stack_free';
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCRECalloutDefaultExportName = 'pcre_callout';
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16CalloutDefaultExportName = 'pcre16_callout';
  {$ENDIF PCRE_16}

{$IFDEF PCRE_LINKONREQUEST}
var
  PCRELibraryName: string = PCREDefaultLibraryName;

  {$IFDEF PCRE_8}
  PCRECompileExportName: string = PCRECompileDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16CompileExportName: string = PCRE16CompileDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCRECompile2ExportName: string = PCRECompile2DefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16Compile2ExportName: string = PCRE16Compile2DefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREConfigExportName: string = PCREConfigDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16ConfigExportName: string = PCRE16ConfigDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCRECopyNamedSubstringExportName: string = PCRECopyNamedSubstringDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16CopyNamedSubstringExportName: string = PCRE16CopyNamedSubstringDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCRECopySubStringExportName: string = PCRECopySubStringDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16CopySubStringExportName: string = PCRE16CopySubStringDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREDfaExecExportName: string = PCREDfaExecDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16DfaExecExportName: string = PCRE16DfaExecDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREExecExportName: string = PCREExecDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16ExecExportName: string = PCRE16ExecDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREFreeSubStringExportName: string = PCREFreeSubStringDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16FreeSubStringExportName: string = PCRE16FreeSubStringDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREFreeSubStringListExportName: string = PCREFreeSubStringListDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16FreeSubStringListExportName: string = PCRE16FreeSubStringListDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREFullInfoExportName: string = PCREFullInfoDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16FullInfoExportName: string = PCRE16FullInfoDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREGetNamedSubstringExportName: string = PCREGetNamedSubstringDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16GetNamedSubstringExportName: string = PCRE16GetNamedSubstringDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREGetStringNumberExportName: string = PCREGetStringNumberDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16GetStringNumberExportName: string = PCRE16GetStringNumberDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREGetStringTableEntriesExportName: string = PCREGetStringTableEntriesDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16GetStringTableEntriesExportName: string = PCRE16GetStringTableEntriesDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREGetSubStringExportName: string = PCREGetSubStringDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16GetSubStringExportName: string = PCRE16GetSubStringDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREGetSubStringListExportName: string = PCREGetSubStringListDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16GetSubStringListExportName: string = PCRE16GetSubStringListDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREMakeTablesExportName: string = PCREMakeTablesDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16MakeTablesExportName: string = PCRE16MakeTablesDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCRERefCountExportName: string = PCRERefCountDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16RefCountExportName: string = PCRE16RefCountDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREStudyExportName: string = PCREStudyDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16StudyExportName: string = PCRE16StudyDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREFreeStudyExportName: string = PCREFreeStudyDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16FreeStudyExportName: string = PCRE16FreeStudyDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREVersionExportName: string = PCREVersionDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16VersionExportName: string = PCRE16VersionDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREJITStackAllocExportName: string = PCREJITStackAllocDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16JITStackAllocExportName: string = PCRE16JITStackAllocDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREJITStackFreeExportName: string = PCREJITStackFreeDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16JITStackFreeExportName: string = PCRE16JITStackFreeDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREAssignJITStackExportName: string = PCREAssignJITStackDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16AssignJITStackExportName: string = PCRE16AssignJITStackDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREMallocExportName: string = PCREMallocDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16MallocExportName: string = PCRE16MallocDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREFreeExportName: string = PCREFreeDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16FreeExportName: string = PCRE16FreeDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREStackMallocExportName: string = PCREStackMallocDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16StackMallocExportName: string = PCRE16StackMallocDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCREStackFreeExportName: string = PCREStackFreeDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16StackFreeExportName: string = PCRE16StackFreeDefaultExportName;
  {$ENDIF PCRE_16}
  {$IFDEF PCRE_8}
  PCRECalloutExportName: string = PCRECalloutDefaultExportName;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  PCRE16CalloutExportName: string = PCRE16CalloutDefaultExportName;
  {$ENDIF PCRE_16}
{$ENDIF PCRE_LINKONREQUEST}

var
  PCRELib: TModuleHandle = INVALID_MODULEHANDLE_VALUE;

function IsPCRELoaded: Boolean;
function LoadPCRE: Boolean;
procedure UnloadPCRE;

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
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  {$IFDEF HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF MSWINDOWS}
  System.Types, System.SysUtils;
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Types, SysUtils;
  {$ENDIF ~HAS_UNITSCOPE}

{$IFNDEF PCRE_RTL}
{$IFDEF PCRE_STATICLINK}

{$IFDEF PCRE_8}
// make the linker happy with PCRE 8.00
procedure _pcre_find_bracket; external;
// make the linker happy with PCRE 8.21
procedure _pcre_jit_compile; external;
procedure _pcre_jit_free; external;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
// make the linker happy with PCRE 8.30
procedure _pcre16_find_bracket; external;
procedure _pcre16_jit_compile; external;
procedure _pcre16_jit_free; external;
{$ENDIF PCRE_16}

{$IFDEF CPU32}

{$IFDEF PCRE_8}

{$LINK ..\windows\obj\pcre\win32\pcre_compile.obj}
{$LINK ..\windows\obj\pcre\win32\pcre_config.obj}
{$LINK ..\windows\obj\pcre\win32\pcre_dfa_exec.obj}
{$LINK ..\windows\obj\pcre\win32\pcre_exec.obj}
{$LINK ..\windows\obj\pcre\win32\pcre_fullinfo.obj}
{$LINK ..\windows\obj\pcre\win32\pcre_get.obj}
{$LINK ..\windows\obj\pcre\win32\pcre_jit_compile.obj}
{$LINK ..\windows\obj\pcre\win32\pcre_maketables.obj}
{$LINK ..\windows\obj\pcre\win32\pcre_newline.obj}
{$LINK ..\windows\obj\pcre\win32\pcre_ord2utf8.obj}
{$LINK ..\windows\obj\pcre\win32\pcre_refcount.obj}
{$LINK ..\windows\obj\pcre\win32\pcre_study.obj}
{$LINK ..\windows\obj\pcre\win32\pcre_tables.obj}
{$LINK ..\windows\obj\pcre\win32\pcre_ucd.obj}
{$LINK ..\windows\obj\pcre\win32\pcre_valid_utf8.obj}
{$LINK ..\windows\obj\pcre\win32\pcre_version.obj}
{$LINK ..\windows\obj\pcre\win32\pcre_xclass.obj}
{$LINK ..\windows\obj\pcre\win32\pcre_chartables.obj}

{$ENDIF PCRE_8}

{$IFDEF PCRE_16}

{$LINK ..\windows\obj\pcre\win32\pcre16_compile.obj}
{$LINK ..\windows\obj\pcre\win32\pcre16_config.obj}
{$LINK ..\windows\obj\pcre\win32\pcre16_dfa_exec.obj}
{$LINK ..\windows\obj\pcre\win32\pcre16_exec.obj}
{$LINK ..\windows\obj\pcre\win32\pcre16_fullinfo.obj}
{$LINK ..\windows\obj\pcre\win32\pcre16_get.obj}
{$LINK ..\windows\obj\pcre\win32\pcre16_jit_compile.obj}
{$LINK ..\windows\obj\pcre\win32\pcre16_maketables.obj}
{$LINK ..\windows\obj\pcre\win32\pcre16_newline.obj}
{$LINK ..\windows\obj\pcre\win32\pcre16_ord2utf16.obj}
{$LINK ..\windows\obj\pcre\win32\pcre16_refcount.obj}
{$LINK ..\windows\obj\pcre\win32\pcre16_study.obj}
{$LINK ..\windows\obj\pcre\win32\pcre16_tables.obj}
{$LINK ..\windows\obj\pcre\win32\pcre16_ucd.obj}
{$LINK ..\windows\obj\pcre\win32\pcre16_valid_utf16.obj}
{$LINK ..\windows\obj\pcre\win32\pcre16_version.obj}
{$LINK ..\windows\obj\pcre\win32\pcre16_xclass.obj}
{$LINK ..\windows\obj\pcre\win32\pcre16_chartables.obj}
{$LINK ..\windows\obj\pcre\win32\pcre16_string_utils.obj}
{$ENDIF PCRE_16}

{$ENDIF CPU32}
{$IFDEF CPU64}

{$IFDEF PCRE_8}

{$LINK ..\windows\obj\pcre\win64\pcre_compile.obj}
{$LINK ..\windows\obj\pcre\win64\pcre_config.obj}
{$LINK ..\windows\obj\pcre\win64\pcre_dfa_exec.obj}
{$LINK ..\windows\obj\pcre\win64\pcre_exec.obj}
{$LINK ..\windows\obj\pcre\win64\pcre_fullinfo.obj}
{$LINK ..\windows\obj\pcre\win64\pcre_get.obj}
{$LINK ..\windows\obj\pcre\win64\pcre_jit_compile.obj}
{$LINK ..\windows\obj\pcre\win64\pcre_maketables.obj}
{$LINK ..\windows\obj\pcre\win64\pcre_newline.obj}
{$LINK ..\windows\obj\pcre\win64\pcre_ord2utf8.obj}
{$LINK ..\windows\obj\pcre\win64\pcre_refcount.obj}
{$LINK ..\windows\obj\pcre\win64\pcre_study.obj}
{$LINK ..\windows\obj\pcre\win64\pcre_tables.obj}
{$LINK ..\windows\obj\pcre\win64\pcre_ucd.obj}
{$LINK ..\windows\obj\pcre\win64\pcre_valid_utf8.obj}
{$LINK ..\windows\obj\pcre\win64\pcre_version.obj}
{$LINK ..\windows\obj\pcre\win64\pcre_xclass.obj}
{$LINK ..\windows\obj\pcre\win64\pcre_chartables.obj}

{$ENDIF PCRE_8}

{$IFDEF PCRE_16}

{$LINK ..\windows\obj\pcre\win64\pcre16_compile.obj}
{$LINK ..\windows\obj\pcre\win64\pcre16_config.obj}
{$LINK ..\windows\obj\pcre\win64\pcre16_dfa_exec.obj}
{$LINK ..\windows\obj\pcre\win64\pcre16_exec.obj}
{$LINK ..\windows\obj\pcre\win64\pcre16_fullinfo.obj}
{$LINK ..\windows\obj\pcre\win64\pcre16_get.obj}
{$LINK ..\windows\obj\pcre\win64\pcre16_jit_compile.obj}
{$LINK ..\windows\obj\pcre\win64\pcre16_maketables.obj}
{$LINK ..\windows\obj\pcre\win64\pcre16_newline.obj}
{$LINK ..\windows\obj\pcre\win64\pcre16_ord2utf16.obj}
{$LINK ..\windows\obj\pcre\win64\pcre16_refcount.obj}
{$LINK ..\windows\obj\pcre\win64\pcre16_study.obj}
{$LINK ..\windows\obj\pcre\win64\pcre16_tables.obj}
{$LINK ..\windows\obj\pcre\win64\pcre16_ucd.obj}
{$LINK ..\windows\obj\pcre\win64\pcre16_valid_utf16.obj}
{$LINK ..\windows\obj\pcre\win64\pcre16_version.obj}
{$LINK ..\windows\obj\pcre\win64\pcre16_xclass.obj}
{$LINK ..\windows\obj\pcre\win64\pcre16_chartables.obj}
{$LINK ..\windows\obj\pcre\win64\pcre16_string_utils.obj}

{$ENDIF PCRE_16}

{$ENDIF CPU64}

// user's defined callbacks
var
  {$IFDEF PCRE_8}
  pcre_malloc_user: pcre_malloc_callback;
  pcre_free_user: pcre_free_callback;
  pcre_stack_malloc_user: pcre_stack_malloc_callback;
  pcre_stack_free_user: pcre_stack_free_callback;
  pcre_callout_user: pcre_callout_callback;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_malloc_user: pcre16_malloc_callback;
  pcre16_free_user: pcre16_free_callback;
  pcre16_stack_malloc_user: pcre16_stack_malloc_callback;
  pcre16_stack_free_user: pcre16_stack_free_callback;
  pcre16_callout_user: pcre16_callout_callback;
  {$ENDIF PCRE_16}

{$IFDEF PCRE_8}
function pcre_compile; external;
function pcre_compile2; external;
function pcre_config; external;
function pcre_copy_named_substring; external;
function pcre_copy_substring; external;
function pcre_dfa_exec; external;
function pcre_exec; external;
procedure pcre_free_substring; external;
procedure pcre_free_substring_list; external;
function pcre_fullinfo; external;
function pcre_get_named_substring; external;
function pcre_get_stringnumber; external;
function pcre_get_stringtable_entries; external;
function pcre_get_substring; external;
function pcre_get_substring_list; external;
function pcre_maketables; external;
function pcre_refcount; external;
function pcre_study; external;
procedure pcre_free_study; external;
function pcre_version; external;
function pcre_jit_stack_alloc; external;
procedure pcre_jit_stack_free; external;
procedure pcre_assign_jit_stack; external;
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
function pcre16_compile; external;
function pcre16_compile2; external;
function pcre16_config; external;
function pcre16_copy_named_substring; external;
function pcre16_copy_substring; external;
function pcre16_dfa_exec; external;
function pcre16_exec; external;
procedure pcre16_free_substring; external;
procedure pcre16_free_substring_list; external;
function pcre16_fullinfo; external;
function pcre16_get_named_substring; external;
function pcre16_get_stringnumber; external;
function pcre16_get_stringtable_entries; external;
function pcre16_get_substring; external;
function pcre16_get_substring_list; external;
function pcre16_maketables; external;
function pcre16_refcount; external;
function pcre16_study; external;
procedure pcre16_free_study; external;
function pcre16_version; external;
function pcre16_jit_stack_alloc; external;
procedure pcre16_jit_stack_free; external;
procedure pcre16_assign_jit_stack; external;
{$ENDIF PCRE_16}

type
  size_t = Longint;

const
  szMSVCRT = 'MSVCRT.DLL';

function malloc(size: size_t): Pointer; cdecl; external szMSVCRT name 'malloc';

{$IFDEF CPU32}
function _memcpy(dest, src: Pointer; count: size_t): Pointer; cdecl; external szMSVCRT name 'memcpy';
function _memmove(dest, src: Pointer; count: size_t): Pointer; cdecl; external szMSVCRT name 'memmove';
function _memset(dest: Pointer; val: Integer; count: size_t): Pointer; cdecl; external szMSVCRT name 'memset';
function _strncmp(s1: PAnsiChar; s2: PAnsiChar; n: size_t): Integer; cdecl; external szMSVCRT name 'strncmp';
function _memcmp(s1: Pointer; s2: Pointer; n: size_t): Integer; cdecl; external szMSVCRT name 'memcmp';
function _strlen(s: PAnsiChar): size_t; cdecl; external szMSVCRT name 'strlen';
function __ltolower(__ch: Integer): Integer; cdecl; external szMSVCRT name 'tolower';
function __ltoupper(__ch: Integer): Integer; cdecl; external szMSVCRT name 'toupper';
function _isalnum(__ch: Integer): Integer; cdecl; external szMSVCRT name 'isalnum';
function _isalpha(__ch: Integer): Integer; cdecl; external szMSVCRT name 'isalpha';
function _iscntrl(__ch: Integer): Integer; cdecl; external szMSVCRT name 'iscntrl';
function _isdigit(__ch: Integer): Integer; cdecl; external szMSVCRT name 'isdigit';
function _isgraph(__ch: Integer): Integer; cdecl; external szMSVCRT name 'isgraph';
function _islower(__ch: Integer): Integer; cdecl; external szMSVCRT name 'islower';
function _isprint(__ch: Integer): Integer; cdecl; external szMSVCRT name 'isprint';
function _ispunct(__ch: Integer): Integer; cdecl; external szMSVCRT name 'ispunct';
function _isspace(__ch: Integer): Integer; cdecl; external szMSVCRT name 'isspace';
function _isupper(__ch: Integer): Integer; cdecl; external szMSVCRT name 'isupper';
function _isxdigit(__ch: Integer): Integer; cdecl; external szMSVCRT name 'isxdigit';
function _strchr(__s: PAnsiChar; __c: Integer): PAnsiChar; cdecl; external szMSVCRT name 'strchr';

function ___alloca_helper(size: size_t): Pointer; cdecl;
begin
  Result := malloc(size);
end;

procedure __llmul;
asm
  JMP System.__llmul
end;
{$ENDIF CPU32}
{$IFDEF CPU64}
function memcpy(dest, src: Pointer; count: size_t): Pointer; external szMSVCRT name 'memcpy';
function memmove(dest, src: Pointer; count: size_t): Pointer; external szMSVCRT name 'memmove';
function memset(dest: Pointer; val: Integer; count: size_t): Pointer; external szMSVCRT name 'memset';
function strncmp(s1: PAnsiChar; s2: PAnsiChar; n: size_t): Integer; external szMSVCRT name 'strncmp';
function strcmp(s1: PAnsiChar; s2: PAnsiChar; n: size_t): Integer; external szMSVCRT name 'strcmp';
function memcmp(s1: Pointer; s2: Pointer; n: size_t): Integer; external szMSVCRT name 'memcmp';
function strlen(s: PAnsiChar): size_t; external szMSVCRT name 'strlen';
function tolower(__ch: Integer): Integer; external szMSVCRT name 'tolower';
function toupper(__ch: Integer): Integer; external szMSVCRT name 'toupper';
function isalnum(__ch: Integer): Integer; external szMSVCRT name 'isalnum';
function isalpha(__ch: Integer): Integer; external szMSVCRT name 'isalpha';
function iscntrl(__ch: Integer): Integer; external szMSVCRT name 'iscntrl';
function isdigit(__ch: Integer): Integer; external szMSVCRT name 'isdigit';
function isgraph(__ch: Integer): Integer; external szMSVCRT name 'isgraph';
function islower(__ch: Integer): Integer; external szMSVCRT name 'islower';
function isprint(__ch: Integer): Integer; external szMSVCRT name 'isprint';
function ispunct(__ch: Integer): Integer; external szMSVCRT name 'ispunct';
function isspace(__ch: Integer): Integer; external szMSVCRT name 'isspace';
function isupper(__ch: Integer): Integer; external szMSVCRT name 'isupper';
function isxdigit(__ch: Integer): Integer; external szMSVCRT name 'isxdigit';
function strchr(__s: PAnsiChar; __c: Integer): PAnsiChar; external szMSVCRT name 'strchr';

function __chkstk(size: size_t): Pointer;
begin
  Result := malloc(size);
end;
{$ENDIF CPU64}

{$IFDEF PCRE_8}
function pcre_malloc_jcl(Size: SizeInt): Pointer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
  if Assigned(pcre_malloc_user) then
    Result := pcre_malloc_user(Size)
  else
    Result := malloc(Size);
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
function pcre16_malloc_jcl(Size: SizeInt): Pointer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
  if Assigned(pcre16_malloc_user) then
    Result := pcre16_malloc_user(Size)
  else
    Result := malloc(Size);
end;
{$ENDIF PCRE_16}

{$IFDEF PCRE_8}
function pcre_stack_malloc_jcl(Size: SizeInt): Pointer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
  if Assigned(pcre_stack_malloc_user) then
    Result := pcre_stack_malloc_user(Size)
  else
    Result := malloc(Size);
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
function pcre16_stack_malloc_jcl(Size: SizeInt): Pointer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
  if Assigned(pcre16_stack_malloc_user) then
    Result := pcre16_stack_malloc_user(Size)
  else
    Result := malloc(Size);
end;
{$ENDIF PCRE_16}

function _malloc(size: size_t): Pointer;
begin
  Result := malloc(size);
end;

procedure free(pBlock: Pointer); cdecl; external szMSVCRT name 'free';

{$IFDEF PCRE_8}
procedure pcre_free_jcl(P: Pointer); {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
  if Assigned(pcre_free_user) then
    pcre_free_user(P)
  else
    free(P);
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
procedure pcre16_free_jcl(P: Pointer); {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
  if Assigned(pcre16_free_user) then
    pcre16_free_user(P)
  else
    free(P);
end;
{$ENDIF PCRE_16}

{$IFDEF PCRE_8}
procedure pcre_stack_free_jcl(P: Pointer); {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
  if Assigned(pcre_stack_free_user) then
    pcre_stack_free_user(P)
  else
    free(P);
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
procedure pcre16_stack_free_jcl(P: Pointer); {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
  if Assigned(pcre16_stack_free_user) then
    pcre16_stack_free_user(P)
  else
    free(P);
end;
{$ENDIF PCRE_16}

procedure _free(pBlock: Pointer);
begin
  free(pBlock);
end;

{$IFDEF PCRE_8}
function pcre_callout_jcl(var callout_block: pcre_callout_block): Integer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
  if Assigned(pcre_callout_user) then
    Result := pcre_callout_user(callout_block)
  else
    Result := 0;
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
function pcre16_callout_jcl(var callout_block: pcre16_callout_block): Integer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
  if Assigned(pcre16_callout_user) then
    Result := pcre16_callout_user(callout_block)
  else
    Result := 0;
end;
{$ENDIF PCRE_16}

{$IFDEF CPU32}
const
  {$IFDEF PCRE_8}
  _pcre_malloc: pcre_malloc_callback = pcre_malloc_jcl;
  _pcre_free: pcre_free_callback = pcre_free_jcl;
  _pcre_stack_malloc: pcre_stack_malloc_callback = pcre_stack_malloc_jcl;
  _pcre_stack_free: pcre_stack_free_callback = pcre_stack_free_jcl;
  _pcre_callout: pcre_callout_callback = pcre_callout_jcl;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  _pcre16_malloc: pcre16_malloc_callback = pcre16_malloc_jcl;
  _pcre16_free: pcre16_free_callback = pcre16_free_jcl;
  _pcre16_stack_malloc: pcre16_stack_malloc_callback = pcre16_stack_malloc_jcl;
  _pcre16_stack_free: pcre16_stack_free_callback = pcre16_stack_free_jcl;
  _pcre16_callout: pcre16_callout_callback = pcre16_callout_jcl;
  {$ENDIF PCRE_16}
{$ENDIF CPU32}
{$IFDEF CPU64}
const
  {$IFDEF PCRE_8}
  pcre_malloc: pcre_malloc_callback = pcre_malloc_jcl;
  pcre_free: pcre_free_callback = pcre_free_jcl;
  pcre_stack_malloc: pcre_stack_malloc_callback = pcre_stack_malloc_jcl;
  pcre_stack_free: pcre_stack_free_callback = pcre_stack_free_jcl;
  pcre_callout: pcre_callout_callback = pcre_callout_jcl;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_malloc: pcre16_malloc_callback = pcre16_malloc_jcl;
  pcre16_free: pcre16_free_callback = pcre16_free_jcl;
  pcre16_stack_malloc: pcre16_stack_malloc_callback = pcre16_stack_malloc_jcl;
  pcre16_stack_free: pcre16_stack_free_callback = pcre16_stack_free_jcl;
  pcre16_callout: pcre16_callout_callback = pcre16_callout_jcl;
  {$ENDIF PCRE_16}
{$ENDIF CPU64}

{$ENDIF PCRE_STATICLINK}

{$IFDEF PCRE_8}
procedure SetPCREMallocCallback(const Value: pcre_malloc_callback);
begin
  {$IFDEF PCRE_STATICLINK}
  pcre_malloc_user := Value;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre_malloc_func) then
    LoadPCRE;

  if Assigned(pcre_malloc_func) then
    pcre_malloc_func^ := Value
  else if Assigned(LibNotLoadedHandler) then
    LibNotLoadedHandler;
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
procedure SetPCRE16MallocCallback(const Value: pcre16_malloc_callback);
begin
  {$IFDEF PCRE_STATICLINK}
  pcre16_malloc_user := Value;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre16_malloc_func) then
    LoadPCRE;

  if Assigned(pcre16_malloc_func) then
    pcre16_malloc_func^ := Value
  else if Assigned(LibNotLoadedHandler) then
    LibNotLoadedHandler;
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_16}

{$IFDEF PCRE_8}
function GetPCREMallocCallback: pcre_malloc_callback;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre_malloc_user;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre_malloc_func) then
    LoadPCRE;

  if not Assigned(pcre_malloc_func) then
  begin
    Result := nil;
    if Assigned(LibNotLoadedHandler) then
      LibNotLoadedHandler;
  end
  else
    Result := pcre_malloc_func^;
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
function GetPCRE16MallocCallback: pcre16_malloc_callback;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre16_malloc_user;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre16_malloc_func) then
    LoadPCRE;

  if not Assigned(pcre16_malloc_func) then
  begin
    Result := nil;
    if Assigned(LibNotLoadedHandler) then
      LibNotLoadedHandler;
  end
  else
    Result := pcre16_malloc_func^;
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_16}

{$IFDEF PCRE_8}
function CallPCREMalloc(Size: SizeInt): Pointer;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre_malloc_jcl(Size);
  {$ELSE ~PCRE_STATICLINK}
  Result := pcre_malloc_func^(Size);
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
function CallPCRE16Malloc(Size: SizeInt): Pointer;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre16_malloc_jcl(Size);
  {$ELSE ~PCRE_STATICLINK}
  Result := pcre16_malloc_func^(Size);
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_16}

{$IFDEF PCRE_8}
procedure SetPCREFreeCallback(const Value: pcre_free_callback);
begin
  {$IFDEF PCRE_STATICLINK}
  pcre_free_user := Value;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre_free_func) then
    LoadPCRE;

  if Assigned(pcre_free_func) then
    pcre_free_func^ := Value
  else if Assigned(LibNotLoadedHandler) then
    LibNotLoadedHandler;
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
procedure SetPCRE16FreeCallback(const Value: pcre16_free_callback);
begin
  {$IFDEF PCRE_STATICLINK}
  pcre16_free_user := Value;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre16_free_func) then
    LoadPCRE;

  if Assigned(pcre16_free_func) then
    pcre16_free_func^ := Value
  else if Assigned(LibNotLoadedHandler) then
    LibNotLoadedHandler;
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_16}

{$IFDEF PCRE_8}
function GetPCREFreeCallback: pcre_free_callback;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre_free_user;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre_free_func) then
    LoadPCRE;

  if not Assigned(pcre_free_func) then
  begin
    Result := nil;
    if Assigned(LibNotLoadedHandler) then
      LibNotLoadedHandler;
  end
  else
    Result := pcre_free_func^
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
function GetPCRE16FreeCallback: pcre16_free_callback;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre16_free_user;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre16_free_func) then
    LoadPCRE;

  if not Assigned(pcre16_free_func) then
  begin
    Result := nil;
    if Assigned(LibNotLoadedHandler) then
      LibNotLoadedHandler;
  end
  else
    Result := pcre16_free_func^
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_16}

{$IFDEF PCRE_8}
procedure CallPCREFree(P: Pointer);
begin
  {$IFDEF PCRE_STATICLINK}
  pcre_free_jcl(P);
  {$ELSE ~PCRE_STATICLINK}
  pcre_free_func^(P);
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
procedure CallPCRE16Free(P: Pointer);
begin
  {$IFDEF PCRE_STATICLINK}
  pcre16_free_jcl(P);
  {$ELSE ~PCRE_STATICLINK}
  pcre16_free_func^(P);
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_16}

{$IFDEF PCRE_8}
procedure SetPCREStackMallocCallback(const Value: pcre_stack_malloc_callback);
begin
  {$IFDEF PCRE_STATICLINK}
  pcre_stack_malloc_user := Value;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre_stack_malloc_func) then
    LoadPCRE;

  if Assigned(pcre_stack_malloc_func) then
    pcre_stack_malloc_func^ := Value
  else if Assigned(LibNotLoadedHandler) then
    LibNotLoadedHandler;
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
procedure SetPCRE16StackMallocCallback(const Value: pcre16_stack_malloc_callback);
begin
  {$IFDEF PCRE_STATICLINK}
  pcre16_stack_malloc_user := Value;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre16_stack_malloc_func) then
    LoadPCRE;

  if Assigned(pcre16_stack_malloc_func) then
    pcre16_stack_malloc_func^ := Value
  else if Assigned(LibNotLoadedHandler) then
    LibNotLoadedHandler;
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_16}

{$IFDEF PCRE_8}
function GetPCREStackMallocCallback: pcre_stack_malloc_callback;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre_stack_malloc_user;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre_stack_malloc_func) then
    LoadPCRE;

  if not Assigned(pcre_stack_malloc_func) then
  begin
    Result := nil;
    if Assigned(LibNotLoadedHandler) then
      LibNotLoadedHandler;
  end
  else
    Result := pcre_stack_malloc_func^;
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
function GetPCRE16StackMallocCallback: pcre16_stack_malloc_callback;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre16_stack_malloc_user;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre16_stack_malloc_func) then
    LoadPCRE;

  if not Assigned(pcre16_stack_malloc_func) then
  begin
    Result := nil;
    if Assigned(LibNotLoadedHandler) then
      LibNotLoadedHandler;
  end
  else
    Result := pcre16_stack_malloc_func^;
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_16}

{$IFDEF PCRE_8}
function CallPCREStackMalloc(Size: SizeInt): Pointer;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre_stack_malloc_jcl(Size);
  {$ELSE ~PCRE_STATICLINK}
  Result := pcre_stack_malloc_func^(Size);
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
function CallPCRE16StackMalloc(Size: SizeInt): Pointer;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre16_stack_malloc_jcl(Size);
  {$ELSE ~PCRE_STATICLINK}
  Result := pcre16_stack_malloc_func^(Size);
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_16}

{$IFDEF PCRE_8}
procedure SetPCREStackFreeCallback(const Value: pcre_stack_free_callback);
begin
  {$IFDEF PCRE_STATICLINK}
  pcre_stack_free_user := Value;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre_stack_free_func) then
    LoadPCRE;

  if Assigned(pcre_stack_free_func) then
    pcre_stack_free_func^ := Value
  else if Assigned(LibNotLoadedHandler) then
    LibNotLoadedHandler;
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
procedure SetPCRE16StackFreeCallback(const Value: pcre16_stack_free_callback);
begin
  {$IFDEF PCRE_STATICLINK}
  pcre16_stack_free_user := Value;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre16_stack_free_func) then
    LoadPCRE;

  if Assigned(pcre16_stack_free_func) then
    pcre16_stack_free_func^ := Value
  else if Assigned(LibNotLoadedHandler) then
    LibNotLoadedHandler;
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_16}

{$IFDEF PCRE_8}
function GetPCREStackFreeCallback: pcre_stack_free_callback;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre_stack_free_user;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre_stack_free_func) then
    LoadPCRE;

  if not Assigned(pcre_stack_free_func) then
  begin
    Result := nil;
    if Assigned(LibNotLoadedHandler) then
      LibNotLoadedHandler;
  end
  else
    Result := pcre_stack_free_func^;
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
function GetPCRE16StackFreeCallback: pcre16_stack_free_callback;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre16_stack_free_user;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre16_stack_free_func) then
    LoadPCRE;

  if not Assigned(pcre16_stack_free_func) then
  begin
    Result := nil;
    if Assigned(LibNotLoadedHandler) then
      LibNotLoadedHandler;
  end
  else
    Result := pcre16_stack_free_func^;
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_16}

{$IFDEF PCRE_8}
procedure CallPCREStackFree(P: Pointer);
begin
  {$IFDEF PCRE_STATICLINK}
  pcre_stack_free_jcl(P);
  {$ELSE ~PCRE_STATICLINK}
  pcre_stack_free_func^(P);
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
procedure CallPCRE16StackFree(P: Pointer);
begin
  {$IFDEF PCRE_STATICLINK}
  pcre16_stack_free_jcl(P);
  {$ELSE ~PCRE_STATICLINK}
  pcre16_stack_free_func^(P);
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_16}

{$IFDEF PCRE_8}
procedure SetPCRECalloutCallback(const Value: pcre_callout_callback);
begin
  {$IFDEF PCRE_STATICLINK}
  pcre_callout_user := Value;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre_callout_func) then
    LoadPCRE;

  if Assigned(pcre_callout_func) then
    pcre_callout_func^ := Value
  else if Assigned(LibNotLoadedHandler) then
    LibNotLoadedHandler;
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
procedure SetPCRE16CalloutCallback(const Value: pcre16_callout_callback);
begin
  {$IFDEF PCRE_STATICLINK}
  pcre16_callout_user := Value;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre16_callout_func) then
    LoadPCRE;

  if Assigned(pcre16_callout_func) then
    pcre16_callout_func^ := Value
  else if Assigned(LibNotLoadedHandler) then
    LibNotLoadedHandler;
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_16}

{$IFDEF PCRE_8}
function GetPCRECalloutCallback: pcre_callout_callback;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre_callout_user;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre_callout_func) then
    LoadPCRE;

  if not Assigned(pcre_callout_func) then
  begin
    Result := nil;
    if Assigned(LibNotLoadedHandler) then
      LibNotLoadedHandler;
  end
  else
    Result := pcre_callout_func^;
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
function GetPCRE16CalloutCallback: pcre16_callout_callback;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre16_callout_user;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre16_callout_func) then
    LoadPCRE;

  if not Assigned(pcre16_callout_func) then
  begin
    Result := nil;
    if Assigned(LibNotLoadedHandler) then
      LibNotLoadedHandler;
  end
  else
    Result := pcre16_callout_func^;
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_16}

{$IFDEF PCRE_8}
function CallPCRECallout(var callout_block: pcre_callout_block): Integer;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre_callout_jcl(callout_block);
  {$ELSE ~PCRE_STATICLINK}
  Result := pcre_callout_func^(callout_block);
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_8}

{$IFDEF PCRE_16}
function CallPCRE16Callout(var callout_block: pcre16_callout_block): Integer;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre16_callout_jcl(callout_block);
  {$ELSE ~PCRE_STATICLINK}
  Result := pcre16_callout_func^(callout_block);
  {$ENDIF ~PCRE_STATICLINK}
end;
{$ENDIF PCRE_16}

{$IFNDEF PCRE_STATICLINK}
procedure InitPCREFuncPtrs(const Value: Pointer);
begin
  {$IFDEF PCRE_LINKONREQUEST}
  {$IFDEF PCRE_8}
  @pcre_compile := Value;
  @pcre_compile2 := Value;
  @pcre_config := Value;
  @pcre_copy_named_substring := Value;
  @pcre_copy_substring := Value;
  @pcre_dfa_exec := Value;
  @pcre_exec := Value;
  @pcre_free_substring := Value;
  @pcre_free_substring_list := Value;
  @pcre_fullinfo := Value;
  @pcre_get_named_substring := Value;
  @pcre_get_stringnumber := Value;
  @pcre_get_stringtable_entries := Value;
  @pcre_get_substring := Value;
  @pcre_get_substring_list := Value;
  @pcre_maketables := Value;
  @pcre_refcount := Value;
  @pcre_study := Value;
  @pcre_free_study := Value;
  @pcre_version := Value;
  @pcre_jit_stack_alloc := Value;
  @pcre_jit_stack_free := Value;
  @pcre_assign_jit_stack := Value;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  @pcre16_compile := Value;
  @pcre16_compile2 := Value;
  @pcre16_config := Value;
  @pcre16_copy_named_substring := Value;
  @pcre16_copy_substring := Value;
  @pcre16_dfa_exec := Value;
  @pcre16_exec := Value;
  @pcre16_free_substring := Value;
  @pcre16_free_substring_list := Value;
  @pcre16_fullinfo := Value;
  @pcre16_get_named_substring := Value;
  @pcre16_get_stringnumber := Value;
  @pcre16_get_stringtable_entries := Value;
  @pcre16_get_substring := Value;
  @pcre16_get_substring_list := Value;
  @pcre16_maketables := Value;
  @pcre16_refcount := Value;
  @pcre16_study := Value;
  @pcre16_free_study := Value;
  @pcre16_version := Value;
  @pcre16_jit_stack_alloc := Value;
  @pcre16_jit_stack_free := Value;
  @pcre16_assign_jit_stack := Value;
  {$ENDIF PCRE_16}
  {$ENDIF PCRE_LINKONREQUEST}
  {$IFDEF PCRE_8}
  pcre_malloc_func := nil;
  pcre_free_func := nil;
  pcre_stack_malloc_func := nil;
  pcre_stack_free_func := nil;
  pcre_callout_func := nil;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  pcre16_malloc_func := nil;
  pcre16_free_func := nil;
  pcre16_stack_malloc_func := nil;
  pcre16_stack_free_func := nil;
  pcre16_callout_func := nil;
  {$ENDIF PCRE_16}
end;
{$ENDIF ~PCRE_STATICLINK}
{$ENDIF ~PCRE_RTL}

function IsPCRELoaded: Boolean;
begin
  {$IFDEF PCRE_RTL}
  Result := True;
  {$ELSE ~PCRE_RTL}
  {$IFDEF PCRE_STATICLINK}
  Result := True;
  {$ELSE ~PCRE_STATICLINK}
  Result := PCRELib <> INVALID_MODULEHANDLE_VALUE;
  {$ENDIF ~PCRE_STATICLINK}
  {$ENDIF ~PCRE_RTL}
end;

function LoadPCRE: Boolean;
{$IFDEF PCRE_RTL}
begin
  Result := True;
end;
{$ELSE ~PCRE_RTL}
{$IFDEF PCRE_STATICLINK}
begin
  Result := True;
end;
{$ELSE ~PCRE_STATICLINK}
begin
  Result := PCRELib <> INVALID_MODULEHANDLE_VALUE;
  if Result then
    Exit;

  {$IFDEF PCRE_LINKONREQUEST}
  Result := JclSysUtils.LoadModule(PCRELib, PCRELibraryName);
  {$ELSE ~PCRE_LINKONREQUEST}
  Result := JclSysUtils.LoadModule(PCRELib, PCREDefaultLibraryName);
  {$ENDIF ~PCRE_LINKONREQUEST}
  if Result then
  begin
    {$IFDEF PCRE_LINKONREQUEST}
    {$IFDEF PCRE_8}
    @pcre_compile := GetModuleSymbol(PCRELib, PCRECompileExportName);
    @pcre_compile2 := GetModuleSymbol(PCRELib, PCRECompile2ExportName);
    @pcre_config := GetModuleSymbol(PCRELib, PCREConfigExportName);
    @pcre_copy_named_substring := GetModuleSymbol(PCRELib, PCRECopyNamedSubstringExportName);
    @pcre_copy_substring := GetModuleSymbol(PCRELib, PCRECopySubStringExportName);
    @pcre_dfa_exec := GetModuleSymbol(PCRELib, PCREDfaExecExportName);
    @pcre_exec := GetModuleSymbol(PCRELib, PCREExecExportName);
    @pcre_free_substring := GetModuleSymbol(PCRELib, PCREFreeSubStringExportName);
    @pcre_free_substring_list := GetModuleSymbol(PCRELib, PCREFreeSubStringListExportName);
    @pcre_fullinfo := GetModuleSymbol(PCRELib, PCREFullInfoExportName);
    @pcre_get_named_substring := GetModuleSymbol(PCRELib, PCREGetNamedSubstringExportName);
    @pcre_get_stringnumber := GetModuleSymbol(PCRELib, PCREGetStringNumberExportName);
    @pcre_get_stringtable_entries := GetModuleSymbol(PCRELib, PCREGetStringTableEntriesExportName);
    @pcre_get_substring := GetModuleSymbol(PCRELib, PCREGetSubStringExportName);
    @pcre_get_substring_list := GetModuleSymbol(PCRELib, PCREGetSubStringListExportName);
    @pcre_maketables := GetModuleSymbol(PCRELib, PCREMakeTablesExportName);
    @pcre_refcount := GetModuleSymbol(PCRELib, PCRERefCountExportName);
    @pcre_study := GetModuleSymbol(PCRELib, PCREStudyExportName);
    @pcre_free_study := GetModuleSymbol(PCRELib, PCREFreeStudyExportName);
    @pcre_version := GetModuleSymbol(PCRELib, PCREVersionExportName);
    @pcre_jit_stack_alloc := GetModuleSymbol(PCRELib, PCREJITStackAllocExportName);
    @pcre_jit_stack_free := GetModuleSymbol(PCRELib, PCREJITStackFreeExportName);
    @pcre_assign_jit_stack := GetModuleSymbol(PCRELib, PCREAssignJITStackExportName);
    pcre_malloc_func := GetModuleSymbol(PCRELib, PCREMallocExportName);
    pcre_free_func := GetModuleSymbol(PCRELib, PCREFreeExportName);
    pcre_stack_malloc_func := GetModuleSymbol(PCRELib, PCREStackMallocExportName);
    pcre_stack_free_func := GetModuleSymbol(PCRELib, PCREStackFreeExportName);
    pcre_callout_func := GetModuleSymbol(PCRELib, PCRECalloutExportName);
    {$ENDIF PCRE_8}
    {$IFDEF PCRE_16}
    @pcre16_compile := GetModuleSymbol(PCRELib, PCRE16CompileExportName);
    @pcre16_compile2 := GetModuleSymbol(PCRELib, PCRE16Compile2ExportName);
    @pcre16_config := GetModuleSymbol(PCRELib, PCRE16ConfigExportName);
    @pcre16_copy_named_substring := GetModuleSymbol(PCRELib, PCRE16CopyNamedSubstringExportName);
    @pcre16_copy_substring := GetModuleSymbol(PCRELib, PCRE16CopySubStringExportName);
    @pcre16_dfa_exec := GetModuleSymbol(PCRELib, PCRE16DfaExecExportName);
    @pcre16_exec := GetModuleSymbol(PCRELib, PCRE16ExecExportName);
    @pcre16_free_substring := GetModuleSymbol(PCRELib, PCRE16FreeSubStringExportName);
    @pcre16_free_substring_list := GetModuleSymbol(PCRELib, PCRE16FreeSubStringListExportName);
    @pcre16_fullinfo := GetModuleSymbol(PCRELib, PCRE16FullInfoExportName);
    @pcre16_get_named_substring := GetModuleSymbol(PCRELib, PCRE16GetNamedSubstringExportName);
    @pcre16_get_stringnumber := GetModuleSymbol(PCRELib, PCRE16GetStringNumberExportName);
    @pcre16_get_stringtable_entries := GetModuleSymbol(PCRELib, PCRE16GetStringTableEntriesExportName);
    @pcre16_get_substring := GetModuleSymbol(PCRELib, PCRE16GetSubStringExportName);
    @pcre16_get_substring_list := GetModuleSymbol(PCRELib, PCRE16GetSubStringListExportName);
    @pcre16_maketables := GetModuleSymbol(PCRELib, PCRE16MakeTablesExportName);
    @pcre16_refcount := GetModuleSymbol(PCRELib, PCRE16RefCountExportName);
    @pcre16_study := GetModuleSymbol(PCRELib, PCRE16StudyExportName);
    @pcre16_free_study := GetModuleSymbol(PCRELib, PCRE16FreeStudyExportName);
    @pcre16_version := GetModuleSymbol(PCRELib, PCRE16VersionExportName);
    @pcre16_jit_stack_alloc := GetModuleSymbol(PCRELib, PCRE16JITStackAllocExportName);
    @pcre16_jit_stack_free := GetModuleSymbol(PCRELib, PCRE16JITStackFreeExportName);
    @pcre16_assign_jit_stack := GetModuleSymbol(PCRELib, PCRE16AssignJITStackExportName);
    pcre16_malloc_func := GetModuleSymbol(PCRELib, PCRE16MallocExportName);
    pcre16_free_func := GetModuleSymbol(PCRELib, PCRE16FreeExportName);
    pcre16_stack_malloc_func := GetModuleSymbol(PCRELib, PCRE16StackMallocExportName);
    pcre16_stack_free_func := GetModuleSymbol(PCRELib, PCRE16StackFreeExportName);
    pcre16_callout_func := GetModuleSymbol(PCRELib, PCRE16CalloutExportName);
    {$ENDIF PCRE_16}
    {$ELSE ~PCRE_LINKONREQUEST}
    {$IFDEF PCRE_8}
    pcre_malloc_func := GetModuleSymbol(PCRELib, PCREMallocDefaultExportName);
    pcre_free_func := GetModuleSymbol(PCRELib, PCREFreeDefaultExportName);
    pcre_stack_malloc_func := GetModuleSymbol(PCRELib, PCREStackMallocDefaultExportName);
    pcre_stack_free_func := GetModuleSymbol(PCRELib, PCREStackFreeDefaultExportName);
    pcre_callout_func := GetModuleSymbol(PCRELib, PCRECalloutDefaultExportName);
    {$ENDIF PCRE_8}
    {$IFDEF PCRE_16}
    pcre16_malloc_func := GetModuleSymbol(PCRELib, PCRE16MallocDefaultExportName);
    pcre16_free_func := GetModuleSymbol(PCRELib, PCRE16FreeDefaultExportName);
    pcre16_stack_malloc_func := GetModuleSymbol(PCRELib, PCRE16StackMallocDefaultExportName);
    pcre16_stack_free_func := GetModuleSymbol(PCRELib, PCRE16StackFreeDefaultExportName);
    pcre16_callout_func := GetModuleSymbol(PCRELib, PCRE16CalloutDefaultExportName);
    {$ENDIF PCRE_16}
    {$ENDIF ~PCRE_LINKONREQUEST}
  end
  else
    InitPCREFuncPtrs(@LibNotLoadedHandler);
end;
{$ENDIF ~PCRE_STATICLINK}
{$ENDIF ~PCRE_RTL}

procedure UnloadPCRE;
begin
  {$IFNDEF PCRE_RTL}
  {$IFNDEF PCRE_STATICLINK}
  InitPCREFuncPtrs(@LibNotLoadedHandler);
  JclSysUtils.UnloadModule(PCRELib);
  {$ENDIF ~PCRE_STATICLINK}
  {$ENDIF ~PCRE_RTL}
end;

{$IFDEF PCRE_LINKDLL}
{$IFDEF PCRE_8}
function pcre_compile; external PCREDefaultLibraryName name PCRECompileDefaultExportName;
function pcre_compile2; external PCREDefaultLibraryName name PCRECompile2DefaultExportName;
function pcre_config; external PCREDefaultLibraryName name PCREConfigDefaultExportName;
function pcre_copy_named_substring; external PCREDefaultLibraryName name PCRECopyNamedSubStringDefaultExportName;
function pcre_copy_substring; external PCREDefaultLibraryName name PCRECopySubStringDefaultExportName;
function pcre_dfa_exec; external PCREDefaultLibraryName name PCREDfaExecDefaultExportName;
function pcre_exec; external PCREDefaultLibraryName name PCREExecDefaultExportName;
procedure pcre_free_substring; external PCREDefaultLibraryName name PCREFreeSubStringDefaultExportName;
procedure pcre_free_substring_list; external PCREDefaultLibraryName name PCREFreeSubStringListDefaultExportName;
function pcre_fullinfo; external PCREDefaultLibraryName name PCREFullInfoDefaultExportName;
function pcre_get_named_substring; external PCREDefaultLibraryName name PCREGetNamedSubStringDefaultExportName;
function pcre_get_stringnumber; external PCREDefaultLibraryName name PCREGetStringNumberDefaultExportName;
function pcre_get_stringtable_entries; external PCREDefaultLibraryName name PCREGetStringTableEntriesDefaultExportName;
function pcre_get_substring; external PCREDefaultLibraryName name PCREGetSubStringDefaultExportName;
function pcre_get_substring_list; external PCREDefaultLibraryName name PCREGetSubStringListDefaultExportName;
function pcre_maketables; external PCREDefaultLibraryName name PCREMakeTablesDefaultExportName;
function pcre_refcount; external PCREDefaultLibraryName name PCRERefCountDefaultExportName;
function pcre_study; external PCREDefaultLibraryName name PCREStudyDefaultExportName;
procedure pcre_free_study; external PCREDefaultLibraryName name PCREFreeStudyDefaultExportName;
function pcre_version; external PCREDefaultLibraryName name PCREVersionDefaultExportName;
function pcre_jit_stack_alloc; external PCREDefaultLibraryName name PCREJITStackAllocDefaultExportName;
procedure pcre_jit_stack_free; external PCREDefaultLibraryName name PCREJITStackFreeDefaultExportName;
procedure pcre_assign_jit_stack; external PCREDefaultLibraryName name PCREAssignJITStackDefaultExportName;
{$ENDIF PCRE_8}
{$IFDEF PCRE_16}
function pcre16_compile; external PCREDefaultLibraryName name PCRE16CompileDefaultExportName;
function pcre16_compile2; external PCREDefaultLibraryName name PCRE16Compile2DefaultExportName;
function pcre16_config; external PCREDefaultLibraryName name PCRE16ConfigDefaultExportName;
function pcre16_copy_named_substring; external PCREDefaultLibraryName name PCRE16CopyNamedSubStringDefaultExportName;
function pcre16_copy_substring; external PCREDefaultLibraryName name PCRE16CopySubStringDefaultExportName;
function pcre16_dfa_exec; external PCREDefaultLibraryName name PCRE16DfaExecDefaultExportName;
function pcre16_exec; external PCREDefaultLibraryName name PCRE16ExecDefaultExportName;
procedure pcre16_free_substring; external PCREDefaultLibraryName name PCRE16FreeSubStringDefaultExportName;
procedure pcre16_free_substring_list; external PCREDefaultLibraryName name PCRE16FreeSubStringListDefaultExportName;
function pcre16_fullinfo; external PCREDefaultLibraryName name PCRE16FullInfoDefaultExportName;
function pcre16_get_named_substring; external PCREDefaultLibraryName name PCRE16GetNamedSubStringDefaultExportName;
function pcre16_get_stringnumber; external PCREDefaultLibraryName name PCRE16GetStringNumberDefaultExportName;
function pcre16_get_stringtable_entries; external PCREDefaultLibraryName name PCRE16GetStringTableEntriesDefaultExportName;
function pcre16_get_substring; external PCREDefaultLibraryName name PCRE16GetSubStringDefaultExportName;
function pcre16_get_substring_list; external PCREDefaultLibraryName name PCRE16GetSubStringListDefaultExportName;
function pcre16_maketables; external PCREDefaultLibraryName name PCRE16MakeTablesDefaultExportName;
function pcre16_refcount; external PCREDefaultLibraryName name PCRE16RefCountDefaultExportName;
function pcre16_study; external PCREDefaultLibraryName name PCRE16StudyDefaultExportName;
procedure pcre16_free_study; external PCREDefaultLibraryName name PCRE16FreeStudyDefaultExportName;
function pcre16_version; external PCREDefaultLibraryName name PCRE16VersionDefaultExportName;
function pcre16_jit_stack_alloc; external PCREDefaultLibraryName name PCRE16JITStackAllocDefaultExportName;
procedure pcre16_jit_stack_free; external PCREDefaultLibraryName name PCRE16JITStackFreeDefaultExportName;
procedure pcre16_assign_jit_stack; external PCREDefaultLibraryName name PCRE16AssignJITStackDefaultExportName;
{$ENDIF PCRE_16}
{$ENDIF PCRE_LINKDLL}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

