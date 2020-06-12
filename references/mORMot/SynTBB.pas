/// implements a Linux FPC heap manager that uses Intel TBB, with no overhead
// - expected to be enabled with -dFPC_NO_DEFAULT_MEMORYMANAGER -dFPC_SYNTBB,
// and run e.g. "apt-get install libtbb2" on the Linux target system
unit SynTBB;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2019 Arnaud Bouchez
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

  Portions created by the Initial Developer are Copyright (C) 2019
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


  Version 1.18
  - initial revision

}

interface

{$I Synopse.inc} // set proper flags, and define LINUX for BSD and ANDROID

{$ifndef FPC}
  THIS UNIT IS FOR FPC ONLY !
{$endif FPC}
{$ifndef LINUX}
  THIS UNIT IS FOR POSIX ONLY !
{$endif LINUX}

implementation

uses
  pthreads, // as required by TBB library - will also link needed glibc
  dl;

// late-binding API calls to the external Internal TBB malloc library

var
  malloc: function(size: PtrUInt): pointer; cdecl;
  calloc: function(count, size: PtrUInt): pointer; cdecl;
  free: procedure(p: pointer); cdecl;
  realloc: function(p: pointer; size: PtrUInt): pointer; cdecl;
  msize: function(p: pointer): PtrUInt; cdecl;

// TMemoryManager replacement

function _GetMem(size: PtrUInt): pointer;
begin
  result := malloc(size);
end;

function _FreeMem(p: pointer): PtrUInt;
begin
  free(p); // free(nil) has no effect
  result := 0;
end;

function _FreeMemSize(p: pointer; size: PtrUInt): PtrUInt;
begin // our unit won't check the "size" value (not mandatory)
  if size <> 0 then
    free(p);
  result := 0;
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
  lib: pointer;

initialization
{  lib := dlopen('libjemalloc.so.1', RTLD_LAZY); // jemalloc 3.6 is slower than libtbb2
  if lib <> nil then begin
    pointer(@malloc) := dlsym(lib, 'malloc');
    pointer(@calloc) := dlsym(lib, 'calloc');
    pointer(@free) := dlsym(lib, 'free');
    pointer(@realloc) := dlsym(lib, 'realloc');
    pointer(@msize) := dlsym(lib, 'malloc_usable_size');
    writeln('pointer(@msize)= ', PtrInt(@msize));
  end else }
   lib := dlopen('libtbbmalloc.so.2', RTLD_LAZY);
  if lib = nil then
    lib := dlopen('libtbbmalloc.so', RTLD_LAZY);
  if lib = nil then
    writeln(dlerror, '  [apt-get install libtbb2]')
  else begin
    pointer(@malloc) := dlsym(lib, 'scalable_malloc');
    pointer(@calloc) := dlsym(lib, 'scalable_calloc');
    pointer(@free) := dlsym(lib, 'scalable_free');
    pointer(@realloc) := dlsym(lib, 'scalable_realloc');
    pointer(@msize) := dlsym(lib, 'scalable_msize');
  end;
  if Assigned(msize) then begin
    {$ifdef CPUX64} // no cdecl on x86_64 -> direct call is just fine
    NewMM.GetMem := pointer(@malloc);
    NewMM.FreeMem := pointer(@free);
    NewMM.MemSize := pointer(@msize);
    {$endif}
    GetMemoryManager(OldMM);
    SetMemoryManager(NewMM);
  end;

finalization
  if lib <> nil then begin
    SetMemoryManager(OldMM);
    dlclose(lib);
  end;
end.

