{**************************************************************************
A simple wrapper around the Microsoft mimalloc memory allocator
(mimalloc.dll). It provides a Delphi memory manager implemented
on top of the mimalloc allocator.

Usage
Place this unit first in your project’s uses clause, for example:

  uses
    Alcinoe.MimallocMM,
    ...,
    System.Classes,
    ...;

Make sure your application can load mimalloc.dll at runtime.
***************************************************************************}
unit Alcinoe.MimallocMM;

interface

{$I Alcinoe.inc}

implementation

type
  size_t = NativeUInt;

const
  mimallocDLL = 'mimalloc.dll';

{*******************************************************************}
function mi_malloc(Size: size_t): Pointer; cdecl; external mimallocDLL;
function mi_realloc(P: Pointer; Size: size_t): Pointer; cdecl; external mimallocDLL;
function mi_calloc(Count, Size: size_t): Pointer; cdecl; external mimallocDLL;
procedure mi_free(P: Pointer); cdecl; external mimallocDLL;

{****************************************}
function GetMem(Size: NativeInt): Pointer;
begin
  Result := mi_malloc(size_t(Size));
end;

{************************************}
function FreeMem(P: Pointer): Integer;
begin
  mi_free(P);
  Result := 0;
end;

{********************************************************}
function ReallocMem(P: Pointer; Size: NativeInt): Pointer;
begin
  Result := mi_realloc(P, size_t(Size));
end;

{******************************************}
function AllocMem(Size: NativeInt): Pointer;
begin
  Result := mi_calloc(1, size_t(Size));
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
  //ALLog('Alcinoe.MimallocMM','initialization');
  {$ENDIF}
  SetMemoryManager(MemoryManager);

end.