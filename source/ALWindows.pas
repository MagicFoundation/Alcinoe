{*****************************************************************
www:          http://sourceforge.net/projects/alcinoe/
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
Author(s):    Stéphane Vander Clock (skype/email: svanderclock@yahoo.fr)

product:      AlWindows
Version:      4.00

Description:  Windows API function not (yet) in the windows.pas

Know bug :

History :     26/06/2012: Add xe2 support

Link :

**************************************************************}
unit ALWindows;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

uses Winapi.Windows;

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
Var ALGetTickCount64: function: int64; stdcall; // from Windows Vista / Windows Server 2008
function ALCreateProcessWithLogonW(lpUsername: LPCWSTR;  // LPCWSTR
                                   lpDomain: LPCWSTR;  // LPCWSTR
                                   lpPassword: LPCWSTR; // LPCWSTR
                                   dwLogonFlags: DWORD; // DWORD
                                   lpApplicationName: LPCWSTR; // LPCWSTR
                                   lpCommandLine: LPWSTR;  // LPWSTR
                                   dwCreationFlags: DWORD; // DWORD
                                   lpEnvironment: LPVOID; // LPVOID
                                   lpCurrentDirectory: LPCWSTR;  // LPCWSTR
                                   const lpStartupInfo: TStartupInfoW;  // LPSTARTUPINFOW
                                   var lpProcessInfo: TProcessInformation): BOOL; stdcall; // LPPROCESS_INFORMATION
function AttachConsole(dwProcessId: DWORD): BOOL; stdcall;
function ALUserExists(const aUserName: AnsiString): boolean;


const INVALID_SET_FILE_POINTER = DWORD(-1);
      QUOTA_LIMITS_HARDWS_MIN_DISABLE = $2;
      QUOTA_LIMITS_HARDWS_MIN_ENABLE  = $1;
      QUOTA_LIMITS_HARDWS_MAX_DISABLE = $8;
      QUOTA_LIMITS_HARDWS_MAX_ENABLE  = $4;
      LOGON_WITH_PROFILE        = $1;
      LOGON_NETCREDENTIALS_ONLY = $2;
      ATTACH_PARENT_PROCESS = DWORD(-1);

implementation

uses System.Ansistrings,
     System.SysUtils;

{*****************************************************************************}
function ALGlobalMemoryStatusEx; external kernel32 name 'GlobalMemoryStatusEx';

{***********************************************************************************}
function ALCreateProcessWithLogonW; external advapi32 name 'CreateProcessWithLogonW';

{*************************************************************}
function AttachConsole; external kernel32 name 'AttachConsole';

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
end;

{**********************************************************}
function ALUserExists(const aUserName: AnsiString): boolean;
var SID: PSID;
    szDomain: PansiChar;
    cbDomain, cbSID: DWORD;
    NameUse: SID_NAME_USE;
begin

  // initial values, reset them
  SID      := nil;
  cbSID    := 0;
  szDomain := '';
  cbDomain := 0;

  // first attempt to get the buffer sizes
  LookupAccountNameA(nil,  // lpSystemName
                     PAnsiChar(aUserName), // lpAccountName: PAnsiChar;
                     SID, //  Sid: PSID;
                     cbSID,  // var cbSid: DWORD;
                     szDomain, // ReferencedDomainName: PAnsiChar;
                     cbDomain, // var cbReferencedDomainName: DWORD;
                     NameUse); // var peUse: SID_NAME_USE

  // init buffers according to retrieved data
  szDomain := System.Ansistrings.AnsiStrAlloc(cbDomain);
  SID      := AllocMem(cbSID);
  try

    // check if user exists
    result := LookupAccountNameA(nil,
                                 PAnsiChar(aUserName),
                                 SID,
                                 cbSID,
                                 szDomain,
                                 cbDomain,
                                 NameUse);

  finally
    System.Ansistrings.StrDispose(szDomain);
    FreeMem(SID);
  end;

end;

initialization
  ALInitWindowsFunct;

end.
