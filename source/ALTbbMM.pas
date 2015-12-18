{*****************************************************
www:          http://sourceforge.net/projects/alcinoe/
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALTbbMM
Version:      4.01

Description:  A simple wrapper around the Intel Threading Building Blocks memory allocation
              library - tbbmalloc.dll. Implements memory manager based on Intel TBB
              memory allocator.

Usage:        Put this unit to the very first place on your program in section "uses".
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
              but built with different versions of compiler. You can safely use the very first
              (in December 2015 in was "vc10") because there is no difference which of them we will
              use in Delphi.

Legal issues: Copyright (C) 1999-2013 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Known bug :

History :

Link :

* Please send all your feedback to alcinoe@arkadia.com
* If you have downloaded this source from a website different from
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALTbbMM;

interface

implementation

type size_t = Cardinal;

function scalable_getmem(Size: size_t): Pointer; cdecl;              external 'tbbmalloc.dll' name 'scalable_malloc'; inline;
function scalable_realloc(P: Pointer; Size: size_t): Pointer; cdecl; external 'tbbmalloc.dll' name 'scalable_realloc'; inline;
procedure scalable_freemem(P: Pointer); cdecl;                       external 'tbbmalloc.dll' name 'scalable_free'; inline;

{************************************************}
function GetMem(Size: NativeInt): Pointer; inline;
begin
  Result := scalable_getmem(size);
end;

{********************************************}
function FreeMem(P: Pointer): Integer; inline;
begin
  scalable_freemem(P);
  Result := 0;
end;

{****************************************************************}
function ReallocMem(P: Pointer; Size: NativeInt): Pointer; inline;
begin
  Result := scalable_realloc(P, Size);
end;

{**************************************************}
function AllocMem(Size: NativeInt): Pointer; inline;
begin
  Result := GetMem(Size);
  if Assigned(Result) then FillChar(Result^, Size, 0);
end;

{*************************************************************************}
function RegisterUnregisterExpectedMemoryLeak(P: Pointer): Boolean; inline;
begin
  Result := False;
end;

const
  MemoryManager: TMemoryManagerEx = (
    GetMem: GetMem;
    FreeMem: FreeMem;
    ReallocMem: ReallocMem;
    AllocMem: AllocMem;
    RegisterExpectedMemoryLeak: RegisterUnregisterExpectedMemoryLeak;
    UnregisterExpectedMemoryLeak: RegisterUnregisterExpectedMemoryLeak
  );

initialization
  SetMemoryManager(MemoryManager);

end.

