{*****************************************************************
www:          http://sourceforge.net/projects/alcinoe/
svn:          https://alcinoe.svn.sourceforge.net/svnroot/alcinoe
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      AlWindows
Version:      4.00

Description:  Windows API function not (yet) in the windows.pas

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

Know bug :

History :     26/06/2012: Add xe2 support

Link :

* Please send all your feedback to svanderclock@arkadia.com
* If you have downloaded this source from a website different from
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALWindows;

interface

uses Windows;

type

  _ALMEMORYSTATUSEX = record
    dwLength: DWORD;
    dwMemoryLoad: DWORD;
    ullTotalPhys: Int64;
    ullAvailPhys: Int64;
    ullTotalPageFile: Int64;
    ullAvailPageFile: Int64;
    ullTotalVirtual: Int64;
    ullAvailVirtual: Int64;
    ullAvailExtendedVirtual: Int64;
  end;
  TALMemoryStatusEx = _ALMEMORYSTATUSEX;

function ALGlobalMemoryStatusEx(var lpBuffer : TALMEMORYSTATUSEX): BOOL; stdcall; // Windows XP / Windows Server 2003
Var ALInterlockedCompareExchange64: function(var Destination: LONGLONG; Exchange, Comperand: LONGLONG): LONGLONG; stdcall; // from Windows Vista / Windows Server 2003
Var ALGetTickCount64: function: int64; stdcall; // from Windows Vista / Windows Server 2008
Var ALSetProcessWorkingSetSizeEx: function(hProcess: THandle; dwMinimumWorkingSetSize, dwMaximumWorkingSetSize: {$if CompilerVersion >= 23}{Delphi XE2}Size_T{$ELSE}Cardinal{$IFEND}; Flags: DWORD): BOOL; stdcall; // from Windows Vista / Windows Server 2003
function ALInterlockedExchange64(var Target: LONGLONG; Value: LONGLONG): LONGLONG;

const INVALID_SET_FILE_POINTER = DWORD(-1);
      QUOTA_LIMITS_HARDWS_MIN_DISABLE = $2;
      QUOTA_LIMITS_HARDWS_MIN_ENABLE  = $1;
      QUOTA_LIMITS_HARDWS_MAX_DISABLE = $8;
      QUOTA_LIMITS_HARDWS_MAX_ENABLE  = $4;

implementation

uses sysutils;

{*****************************************************************************}
function ALGlobalMemoryStatusEx; external kernel32 name 'GlobalMemoryStatusEx';

{*********************************************************************************}
function  ALInterlockedExchange64(var Target: LONGLONG; Value: LONGLONG): LONGLONG;
begin
  repeat
    Result := Target;
  until (ALInterlockedCompareExchange64(Target, Value, Result) = Result);
end;

{******************************************}
function ALGetTickCount64XP: int64; stdcall;
begin
  Result := GetTickCount;
end;

{***************************}
procedure ALInitWindowsFunct;
var kernel32: HModule;
begin
  // Kernel32 is always loaded already, so use GetModuleHandle
  // instead of LoadLibrary
  kernel32 := GetModuleHandle('kernel32');
  if kernel32 = 0 then RaiseLastOSError;
  @ALGetTickCount64 := GetProcAddress(kernel32, 'GetTickCount64');
  if not Assigned(ALGetTickCount64) then ALGetTickCount64 := ALGetTickCount64XP;
  @ALInterlockedCompareExchange64 := GetProcAddress(kernel32, 'InterlockedCompareExchange64');
  @ALSetProcessWorkingSetSizeEx := GetProcAddress(kernel32, 'SetProcessWorkingSetSizeEx');
end;

initialization
  ALInitWindowsFunct;

end.
