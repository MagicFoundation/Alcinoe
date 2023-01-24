{*******************************************************************************
A simple wrapper around the Intel Threading Building Blocks memory allocation
library - tbbmalloc.dll. Implements memory manager based on Intel TBB
memory allocator.

Usage:
Put this unit to the very first place on your program in section "uses".
It should look like:

uses ALTbbMM,
     ...,
     System.Classes,
     etc...

Be sure that your application can reach the dll "tbbmalloc.dll". A prebuilt
version of this libraries for Win32 and Win64 are provided together with
Alcinoe and can be found on the folders "dll\tbbmalloc\...".

The latest version of the DLL could be downloaded from the web-site of
Intel Threading Building:
- https://www.threadingbuildingblocks.org/

You will need to download the latest version archived for your operation
system from the downloading page:
 - https://www.threadingbuildingblocks.org/download

Inside of this archive you will find directory "bin" that will contain required DLL.
Take a note that they have very strange naming. So "Intel64" refers to the 64-bit
applications that is logical. But "IA32" refers to all the 32-bit applications.
You can be easily confused thinking that IA means Intel Itanium but it doesn't,
i don't know why they decided to name its folder like this and there is a lot of
complainings in the official forums from the people that cannot find 32-bit version
of the dynamical library. Inside you will discover many subfolders named as "vc10",
"vc11" etc. They are aligned for the corresponding version of MSVC-compiler bundled
with their corresponding version of Visual Studio. There are identical libraries
but built with different versions of compiler. it's seam that dll compiled with the last
version of visual studio are a little more faster than dll compiled with the first
version but you will need to run adequate vcredist
*******************************************************************************}

unit ALTbbMM;

interface

implementation

type
  size_t = Cardinal;

const
  tbbmallocDLL = 'tbbmalloc.dll';

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
  SetMemoryManager(MemoryManager);

end.



