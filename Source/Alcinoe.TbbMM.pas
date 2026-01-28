{**************************************************************************
A simple wrapper around the Intel Threading Building Blocks (oneTBB) memory 
allocator (tbbmalloc.dll). It provides a Delphi memory manager implemented 
on top of the TBB scalable allocator.

Usage 
Place this unit first in your project’s uses clause, for example:

  uses
    Alcinoe.TbbMM,
    ...,
    System.Classes,
    ...;


Make sure your application can load tbbmalloc.dll at runtime. Prebuilt Win32
and Win64 versions are shipped with Alcinoe and can be found in:
(Alcinoe)\Libraries\dll\oneTBB\...
***************************************************************************}
unit Alcinoe.TbbMM;

interface

{$I Alcinoe.inc}

implementation

type
  size_t = Cardinal;

const
  tbbmallocDLL = 'tbbmalloc.dll';

{****************************************************************************}
function scalable_malloc(Size: size_t): Pointer; cdecl; external tbbmallocDLL;
function scalable_realloc(P: Pointer; Size: size_t): Pointer; cdecl; external tbbmallocDLL;
procedure scalable_free(P: Pointer); cdecl; external tbbmallocDLL;

{****************************************}
function GetMem(Size: NativeInt): Pointer;
begin
  Result := scalable_malloc(size);
end;

{************************************}
function FreeMem(P: Pointer): Integer;
begin
  scalable_free(P);
  Result := 0;
end;

{********************************************************}
function ReallocMem(P: Pointer; Size: NativeInt): Pointer;
begin
  Result := scalable_realloc(P, Size);
end;

{******************************************}
function AllocMem(Size: NativeInt): Pointer;
begin
  Result := GetMem(Size);
  if Assigned(Result) then FillChar(Result^, Size, 0);
end;

{*****************************************************************}
function RegisterUnregisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  Result := False;
end;

const
  MemoryManager: TMemoryManagerEx = (
    GetMem: GetMem; // function(Size: NativeInt): Pointer;
    FreeMem: FreeMem; // function(P: Pointer): Integer;
    ReallocMem: ReallocMem; // function(P: Pointer; Size: NativeInt): Pointer;
    AllocMem: AllocMem; // function(Size: NativeInt): Pointer;
    RegisterExpectedMemoryLeak: RegisterUnregisterExpectedMemoryLeak; // function(P: Pointer): Boolean;
    UnregisterExpectedMemoryLeak: RegisterUnregisterExpectedMemoryLeak // function(P: Pointer): Boolean;
  );

initialization
  {$IF defined(DEBUG)}
  //ALLog('Alcinoe.TbbMM','initialization');
  {$ENDIF}
  SetMemoryManager(MemoryManager);

end.