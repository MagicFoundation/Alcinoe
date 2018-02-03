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
{ Portions of this code are translated from DelayImp.h.                                            }
{ The Initial Developer of DelayImp.h is Inprise Corporation. Portions created by Inprise          }
{ Corporation are Copyright (C) 1999, 2000 by Inprise Corporation. All Rights Reserved.            }
{                                                                                                  }
{ The Original Code is JclWin32.pas.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel. Portions created by Marcel van  }
{ Brakel are Copyright (C) Marcel van Brakel. All Rights Reserved.                                 }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Marcel van Brakel                                                                              }
{   Peter Friese                                                                                   }
{   Andreas Hausladen (ahuser)                                                                     }
{   Flier Lu (flier)                                                                               }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit defines various Win32 API declarations which are either missing or incorrect in one or }
{ more of the supported Delphi versions. This unit is not intended for regular code, only API      }
{ declarations.                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclWin32;

{$I jcl.inc}
{$I windowsonly.inc}

{$MINENUMSIZE 4}
{$ALIGN ON}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  Winapi.Windows, System.SysUtils,
  {$IFNDEF FPC}
  Winapi.AccCtrl, Winapi.ActiveX,
  {$ENDIF ~FPC}
  {$ELSE ~HAS_UNITSCOPE}
  Windows, SysUtils,
  {$IFNDEF FPC}
  AccCtrl,
  {$ENDIF ~FPC}
  ActiveX,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase;

{$HPPEMIT '#include <WinDef.h>'}
{$HPPEMIT '#include <WinNT.h>'}
{$HPPEMIT '#include <WinBase.h>'}
{$HPPEMIT '#include <BaseTsd.h>'}
{$HPPEMIT '#include <ImageHlp.h>'}
{$HPPEMIT '#include <lm.h>'}
{$HPPEMIT '#include <Nb30.h>'}
{$HPPEMIT '#include <RasDlg.h>'}
{$HPPEMIT '#include <Reason.h>'}
{$HPPEMIT '#include <ShlWApi.h>'}
{$HPPEMIT '#include <WinError.h>'}
{$HPPEMIT '#include <WinIoCtl.h>'}
{$HPPEMIT '#include <WinUser.h>'}
//{$HPPEMIT '#include <Powrprof.h>'}
{$HPPEMIT '#include <delayimp.h>'}
{$HPPEMIT '#include <propidl.h>'}
{$HPPEMIT '#include <msidefs.h>'}
{$HPPEMIT '#include <shlguid.h>'}
{$IFNDEF COMPILER14_UP}
{$HPPEMIT '#include <imgguids.h>'}
{$ENDIF ~COMPILER14_UP}
{$HPPEMIT '#include <objbase.h>'}
{$HPPEMIT '#include <ntsecapi.h>'}
{$HPPEMIT ''}
{$IFDEF RTL230_UP}
{$HPPEMIT '// To avoid ambiguity between IMAGE_LOAD_CONFIG_DIRECTORY32 and  Winapi::Windows::IMAGE_LOAD_CONFIG_DIRECTORY32'}
{$HPPEMIT '#define IMAGE_LOAD_CONFIG_DIRECTORY32 ::IMAGE_LOAD_CONFIG_DIRECTORY32'}
{$HPPEMIT ''}
{$HPPEMIT '// To avoid ambiguity between IMAGE_LOAD_CONFIG_DIRECTORY64 and  Winapi::Windows::IMAGE_LOAD_CONFIG_DIRECTORY64'}
{$HPPEMIT '#define IMAGE_LOAD_CONFIG_DIRECTORY64 ::IMAGE_LOAD_CONFIG_DIRECTORY64'}
{$HPPEMIT ''}
{$ENDIF RTL230_UP}

// EJclWin32Error
{$IFDEF MSWINDOWS}
type
  EJclWin32Error = class(EJclError)
  private
    FLastError: DWORD;
    FLastErrorMsg: string;
  public
    constructor Create(const Msg: string);
    constructor CreateFmt(const Msg: string; const Args: array of const);
    constructor CreateRes(Ident: Integer); overload;
    constructor CreateRes(ResStringRec: PResStringRec); overload;
    property LastError: DWORD read FLastError;
    property LastErrorMsg: string read FLastErrorMsg;
  end;
{$ENDIF MSWINDOWS}

//DOM-IGNORE-BEGIN

{$I win32api\WinDef.int}
{$I win32api\WinNT.int}
{$I win32api\WinBase.int}
{$I win32api\AclApi.int}
{$I win32api\ImageHlp.int}
{$I win32api\IoAPI.int}
{$I win32api\LmErr.int}
{$I win32api\LmCons.int}
{$I win32api\LmAccess.int}
{$I win32api\LmApiBuf.int}
{$I win32api\Lmwksta.int}
{$I win32api\Nb30.int}
{$I win32api\RasDlg.int}
{$I win32api\Reason.int}
{$I win32api\ShlObj.int}
{$I win32api\ShlWApi.int}
{$I win32api\WinError.int}
{$I win32api\WinIoctl.int}
{$I win32api\WinNLS.int}
{$I win32api\WinUser.int}
{$I win32api\PowrProf.int}
{$I win32api\DelayImp.int}
{$I win32api\MsiDefs.int}
{$I win32api\ShlGuid.int}
{$I win32api\imgguids.int}
{$I win32api\ObjBase.int}
{$I win32api\PropIdl.int}
{$I win32api\NtSecApi.int}
{$I win32api\TlHelp32.int}
{$I win32api\Winternl.int}

//DOM-IGNORE-END

{$IFDEF MSWINDOWS}

const
  RtdlSetNamedSecurityInfoW: function(pObjectName: LPWSTR; ObjectType: SE_OBJECT_TYPE;
    SecurityInfo: SECURITY_INFORMATION; psidOwner, psidGroup: PSID;
    pDacl, pSacl: PACL): DWORD stdcall = SetNamedSecurityInfoW;

  RtdlSetWaitableTimer: function(hTimer: THandle; var lpDueTime: TLargeInteger;
    lPeriod: Longint; pfnCompletionRoutine: TFNTimerAPCRoutine;
    lpArgToCompletionRoutine: Pointer; fResume: BOOL): BOOL stdcall = SetWaitableTimer;

  RtdlNetUserAdd: function(servername: LPCWSTR; level: DWORD;
    buf: PByte; parm_err: PDWord): NET_API_STATUS stdcall = NetUserAdd;

  RtdlNetUserDel: function(servername: LPCWSTR;
    username: LPCWSTR): NET_API_STATUS stdcall = NetUserDel;

  RtdlNetGroupAdd: function(servername: LPCWSTR; level: DWORD; buf: PByte;
    parm_err: PDWord): NET_API_STATUS stdcall = NetGroupAdd;

  RtdlNetGroupEnum: function(servername: LPCWSTR; level: DWORD;
    out bufptr: PByte; prefmaxlen: DWORD; out entriesread, totalentries: DWORD;
    resume_handle: PDWORD_PTR): NET_API_STATUS stdcall = NetGroupEnum;

  RtdlNetGroupDel: function(servername: LPCWSTR;
    groupname: LPCWSTR): NET_API_STATUS stdcall = NetGroupDel;

  RtdlNetLocalGroupAdd: function(servername: LPCWSTR; level: DWORD;
    buf: PByte; parm_err: PDWord): NET_API_STATUS stdcall = NetLocalGroupAdd;

  RtdlNetLocalGroupEnum: function(servername: LPCWSTR; level: DWORD;
    out bufptr: PByte; prefmaxlen: DWORD; out entriesread, totalentries: DWORD;
    resumehandle: PDWORD_PTR): NET_API_STATUS stdcall = NetLocalGroupEnum;

  RtdlNetLocalGroupDel: function(servername: LPCWSTR;
    groupname: LPCWSTR): NET_API_STATUS stdcall = NetLocalGroupDel;

  RtdlNetLocalGroupAddMembers: function(servername: LPCWSTR; groupname: LPCWSTR;
    level: DWORD; buf: PByte;
    totalentries: DWORD): NET_API_STATUS stdcall = NetLocalGroupAddMembers;

  RtdlNetApiBufferFree: function(Buffer: Pointer): NET_API_STATUS stdcall = NetApiBufferFree;

  RtdlGetCalendarInfoA: function(Locale: LCID; Calendar: CALID; CalType: CALTYPE;
    lpCalData: PAnsiChar; cchData: Integer;
    lpValue: PDWORD): Integer stdcall = GetCalendarInfoA;

  RtdlGetCalendarInfoW: function(Locale: LCID; Calendar: CALID; CalType: CALTYPE;
    lpCalData: PWideChar; cchData: Integer;
    lpValue: PDWORD): Integer stdcall = GetCalendarInfoW;

  RtdlEnumCalendarInfoExW: function(lpCalInfoEnumProc: TCalInfoEnumProcExW;
    Locale: LCID; Calendar: CALID; CalType: CALTYPE): BOOL stdcall = EnumCalendarInfoExW;

  RtdlGetVolumeNameForVolumeMountPointW: function(lpszVolumeMountPoint: LPCWSTR;
    lpszVolumeName: LPWSTR; cchBufferLength: DWORD): BOOL stdcall = GetVolumeNameForVolumeMountPointW;

  RtdlSetVolumeMountPointW: function(lpszVolumeMountPoint: LPCWSTR;
    lpszVolumeName: LPCWSTR): BOOL stdcall = SetVolumeMountPointW;

  RtdlDeleteVolumeMountPointW: function(lpszVolumeMountPoint: LPCWSTR): BOOL
    stdcall = DeleteVolumeMountPointW;

  RtdlNetBios: function(P: PNCB): UCHAR stdcall = NetBios;

{$ENDIF MSWINDOWS}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclResources;

procedure GetProcedureAddress(var P: Pointer; const ModuleName, ProcName: string);
var
  ModuleHandle: HMODULE;
begin
  if not Assigned(P) then
  begin
    ModuleHandle := GetModuleHandle(PChar(ModuleName));
    if ModuleHandle = 0 then
    begin
      ModuleHandle := SafeLoadLibrary(PChar(ModuleName));
      if ModuleHandle = 0 then
        raise EJclError.CreateResFmt(@RsELibraryNotFound, [ModuleName]);
    end;
    P := GetProcAddress(ModuleHandle, PChar(ProcName));
    if not Assigned(P) then
      raise EJclError.CreateResFmt(@RsEFunctionNotFound, [ModuleName, ProcName]);
  end;
end;

//== { EJclWin32Error } ======================================================

{$IFDEF MSWINDOWS}

constructor EJclWin32Error.Create(const Msg: string);
begin
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  inherited CreateResFmt(@RsWin32Error, [FLastErrorMsg, FLastError, NativeLineBreak, Msg]);
end;

constructor EJclWin32Error.CreateFmt(const Msg: string; const Args: array of const);
begin
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  inherited CreateResFmt(@RsWin32Error, [FLastErrorMsg, FLastError, NativeLineBreak, Format(Msg, Args)]);
end;

constructor EJclWin32Error.CreateRes(Ident: Integer);
begin
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  inherited CreateResFmt(@RsWin32Error, [FLastErrorMsg, FLastError, NativeLineBreak, LoadStr(Ident)]);
end;

constructor EJclWin32Error.CreateRes(ResStringRec: PResStringRec);
begin
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  inherited CreateResFmt(@RsWin32Error, [FLastErrorMsg, FLastError, NativeLineBreak, LoadResString(ResStringRec)]);
end;

{$ENDIF MSWINDOWS}

{$I win32api\AclApi.imp}
{$I win32api\ImageHlp.imp}
{$I win32api\IoAPI.imp}
{$I win32api\LmAccess.imp}
{$I win32api\LmApiBuf.imp}
{$I win32api\Lmwksta.imp}
{$I win32api\Nb30.imp}
{$I win32api\WinBase.imp}
{$I win32api\WinNLS.imp}
{$I win32api\WinUser.imp}
{$I win32api\WinNT.imp}
{$I win32api\PowrProf.imp}
{$I win32api\ObjBase.imp}
{$I win32api\PropIdl.imp}
{$I win32api\NtSecApi.imp}
{$I win32api\TlHelp32.imp}
{$I win32api\Winternl.imp}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.



