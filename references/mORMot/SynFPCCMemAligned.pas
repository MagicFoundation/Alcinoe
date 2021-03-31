/// implements a Linux FPC heap manager that uses external glibc, Intel TBB or
// jemalloc libraries, with no overhead
// - define -dFPC_SYNCMEM (for glibc), -dFPC_SYNTBB (after "apt-get install libtbb2")
// or -dFPC_SYNJEMALLOC (after "apt-get install libjemalloc1")
// - with Linux glibc, alignment is 2*SizeOf(pointer) i.e. 16 bytes under x86_64
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynFPCCMemAligned;

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
  of those above. if you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. if you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

}

(*
  Usage:
    - for glibc (preferred), define -dFPC_NO_DEFAULT_MEMORYMANAGER -dFPC_SYNCMEM
    - for Intel TBB, define -dFPC_SYNTBB - after "apt-get install libtbb2"
    - for jemalloc, define -dFPC_SYNJEMALLOC - after "apt-get install libjemalloc1"

    - and ensure your dpr/lpr start with:
    uses
       {$I SynDprUses.inc} // includes this unit
       SynCommons, ....
    or at least this SynFPCCMemAligned unit is the very first in the uses list

  Some raw numbers, from TestSQL3 string allocation tests (single threaded):
    - FPC default heap
     500000 interning 8 KB in 77.34ms i.e. 6,464,959/s, aver. 0us, 98.6 MB/s
     500000 direct 7.6 MB in 100.73ms i.e. 4,963,518/s, aver. 0us, 75.7 MB/s
    - glibc 2.23
     500000 interning 8 KB in 76.06ms i.e. 6,573,152/s, aver. 0us, 100.2 MB/s
     500000 direct 7.6 MB in 36.64ms i.e. 13,645,915/s, aver. 0us, 208.2 MB/s
    - jemalloc 3.6
     500000 interning 8 KB in 78.60ms i.e. 6,361,323/s, aver. 0us, 97 MB/s
     500000 direct 7.6 MB in 58.08ms i.e. 8,608,667/s, aver. 0us, 131.3 MB/s
    - Intel TBB 4.4
     500000 interning 8 KB in 61.96ms i.e. 8,068,810/s, aver. 0us, 123.1 MB/s
     500000 direct 7.6 MB in 36.46ms i.e. 13,711,402/s, aver. 0us, 209.2 MB/s
    for multi-threaded process, we observed best scaling with TBB on this system
    BUT memory consumption raised to 60 more space (gblic=2.6GB vs TBB=170GB)!
    -> so for serious server work, glibc (FPC_SYNCMEM) sounds the best candidate

*)

interface

{$I Synopse.inc} // set proper flags, and define LINUX for BSD and ANDROID

{$ifndef FPC}
  THIS UNIT IS FOR FPC ONLY !
{$endif FPC}

{$ifdef FPC_SYNCMEM}
  {$ifndef LINUXNOTBSD}
    THIS UNIT IS FOR FPC/LINUX ONLY !
     - requires malloc_usable_size() -> use regular cmem unit instead
  {$endif LINUXNOTBSD}
{$endif FPC_SYNCMEM}


implementation

{$ifdef FPC_SYNCMEM}

// low-level direct calls to the external glibc library

function malloc(size: PtrUInt): pointer; cdecl; external 'c' name 'malloc';
function calloc(count, size: PtrUInt): pointer; cdecl; external 'c' name 'calloc';
procedure free(p: pointer); cdecl; external 'c' name 'free';
function realloc(p: pointer; size: PtrUInt): pointer; cdecl; external 'c' name 'realloc';

function msize(p: pointer): PtrUInt; cdecl; external 'c' name 'malloc_usable_size';
// function missing on some platforms, so this unit is enabled only for LINUXNOTBSD
// see https://www.gnu.org/software/gnulib/manual/html_node/malloc_005fusable_005fsize.html
// = Mac OS X 10.5, FreeBSD 6.0, NetBSD 5.0, OpenBSD 3.8, Minix 3.1.8, AIX 5.1, HP-UX 11.00,
// IRIX 6.5, OSF/1 5.1, Solaris 11.3, mingw, MSVC 14, Interix 3.5, BeOS, Android 4.1


{$ifndef FPC_SYNCMEM_NO_MCHECK}
  {$define FPC_SYNCMEM_MCHECK}
  // try to override default error handler which calls abort()
  // - it would call mcheck() to trigger Error(reExternalException)
  // - enabled even if glibc seems to have hooks disabled on most distros; it
  // shouldn't hurt anyway
  // - define FPC_SYNCMEM_NO_MCHECK if you encounter e.g. linking issue
{$endif FPC_SYNCMEM_NO_MCHECK}

{$ifdef FPC_SYNCMEM_MCHECK}

type
  TAbortFunc = procedure(mstatus: integer); cdecl;

function mcheck(abort: TAbortFunc): integer; cdecl external 'c' name 'mcheck';
// see http://man7.org/linux/man-pages/man3/mcheck.3.html

{$endif FPC_SYNCMEM_MCHECK}

{$else}

uses
  {$ifdef LINUXNOTBSD}
  cthreads, // as required by libraries - will also link needed glibc
  {$endif LINUXNOTBSD}
  dl;

// late-binding API calls to the external malloc libraries

var
  malloc: function(size: PtrUInt): pointer; cdecl;
  calloc: function(count, size: PtrUInt): pointer; cdecl;
  free: procedure(p: pointer); cdecl;
  realloc: function(p: pointer; size: PtrUInt): pointer; cdecl;
  msize: function(p: pointer): PtrUInt; cdecl;

{$endif FPC_SYNCMEM}

// TMemoryManager replacement

function _GetMem(size: PtrUInt): pointer;
begin
  result := malloc(size);
end;

function _FreeMem(p: pointer): PtrUInt;
begin
  free(p); // free(nil) has no effect
  result := 0; // should return the chunk size - only used by heaptrc
end;

function _FreeMemSize(p: pointer; size: PtrUInt): PtrUInt;
begin // our unit won't check the "size" value (not mandatory)
  if size <> 0 then
    free(p);
  result := 0; // should return the chunk size - only used by heaptrc
end;

function _AllocMem(size: PtrUInt): pointer;
begin
  result := calloc(size, 1); // no need to call FillChar() e.g. from mmap
end;

function _ReAllocMem(var p: pointer; size: PtrUInt): pointer;
begin
  result := realloc(p, size); // is free(p) if size=0 or malloc(size) if p=nil
  p := result;
end;

function _MemSize(p: pointer): PtrUInt;
begin // AFAIK used only by fpc_AnsiStr_SetLength() in RTL
  result := msize(p);
end;

function _GetHeapStatus: THeapStatus;
begin
  FillChar(result, sizeof(result), 0);
end;

function _GetFPCHeapStatus: TFPCHeapStatus;
begin
  FillChar(result, sizeof(result), 0);
end;

const
  NewMM: TMemoryManager = (
    NeedLock: false;
    GetMem: @_Getmem;
    FreeMem: @_FreeMem;
    FreememSize: @_FreememSize;
    AllocMem: @_AllocMem;
    ReallocMem: @_ReAllocMem;
    MemSize: @_MemSize;
    InitThread: nil;
    DoneThread: nil;
    RelocateHeap: nil;
    GetHeapStatus: @_GetHeapStatus;
    GetFPCHeapStatus: @_GetFPCHeapStatus);

var
  OldMM: TMemoryManager;

  {$ifndef FPC_SYNCMEM}
  lib: pointer;
  {$endif FPC_SYNCMEM}

{$ifdef FPC_SYNCMEM_MCHECK}

const
  MCHECK_OK = 0;
  MCHECK_FREE = 1;
  MCHECK_HEAD = 2;
  MCHECK_TAIL = 3;

{$I-}
procedure mcheckabort(mstatus: integer); cdecl;
begin
  write(StdErr, 'WARNING: mcheckabort(', mstatus, ') called - ');
  case mstatus of
    MCHECK_FREE: writeln(StdErr, 'Block freed twice');
    MCHECK_HEAD: writeln(StdErr, 'Memory before the block was clobbered');
    MCHECK_TAIL: writeln(StdErr, 'Memory after the block was clobbered');
    else writeln('Unknown/Unexpected Error');
  end;
  Error(reExternalException); // notify problem, but don't call abort()
end;
{$I+}

{$endif FPC_SYNCMEM_MCHECK}


{.$define VERBOSE}

{$I-}
procedure InitMM;
begin
  {$ifdef FPC_SYNCMEM}
  {$ifdef VERBOSE}writeln('using glibc');{$endif}
  {$ifdef FPC_SYNCMEM_MCHECK}
  mcheck(mcheckabort); // override default error handler which calls abort()
  {$endif FPC_SYNCMEM_MCHECK}
  {$else}
  {$ifdef FPC_SYNJEMALLOC} // jemalloc 3.6 seems slower, but maybe less fragmented
  lib := dlopen('libjemalloc.so.1', RTLD_LAZY);
  if lib <> nil then begin
    pointer(@malloc)  := dlsym(lib, 'malloc');
    pointer(@calloc)  := dlsym(lib, 'calloc');
    pointer(@free)    := dlsym(lib, 'free');
    pointer(@realloc) := dlsym(lib, 'realloc');
    pointer(@msize)   := dlsym(lib, 'malloc_usable_size');
    {$ifdef VERBOSE}writeln('using jemalloc');{$endif}
  end else
    writeln(StdErr, dlerror, '  [apt-get install libjemalloc1]');
  {$else}
  {$ifdef FPC_SYNTBB}
  lib := dlopen('libtbbmalloc.so.2', RTLD_LAZY);
  if lib = nil then
   lib := dlopen('libtbbmalloc.so', RTLD_LAZY);
  if lib = nil then
    writeln(StdErr, dlerror, '  [apt-get install libtbb2]')
  else begin
    pointer(@malloc)  := dlsym(lib, 'scalable_malloc');
    pointer(@calloc)  := dlsym(lib, 'scalable_calloc');
    pointer(@free)    := dlsym(lib, 'scalable_free');
    pointer(@realloc) := dlsym(lib, 'scalable_realloc');
    pointer(@msize)   := dlsym(lib, 'scalable_msize');
    {$ifdef VERBOSE}writeln('using Intel TBB');{$endif}
  end;
  {$endif FPC_SYNTBB}
  {$endif FPC_SYNJEMALLOC}
  {$endif FPC_SYNCMEM}
  if pointer(@msize) <> nil then begin
    {$ifdef CPUX64} // no cdecl on x86_64 -> direct call is just fine :)
    NewMM.GetMem  := pointer(@malloc);
    NewMM.FreeMem := pointer(@free);
    NewMM.MemSize := pointer(@msize);
    {$endif CPUX64}
    GetMemoryManager(OldMM);
    SetMemoryManager(NewMM);
  end;
end;
{$I+}

initialization
  InitMM;

finalization
  if pointer(@msize) <> nil then begin
    SetMemoryManager(OldMM);
    {$ifndef FPC_SYNCMEM}
    if lib <> nil then
      dlclose(lib);
    {$endif FPC_SYNCMEM}
  end;
end.

