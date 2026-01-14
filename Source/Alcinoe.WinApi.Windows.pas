unit Alcinoe.WinApi.Windows;

interface

{$I Alcinoe.inc}

{$MINENUMSIZE 4} // https://stackoverflow.com/questions/48953749/why-this-c-to-pascal-conversion-crash

uses
  Winapi.Windows;

type
  wchar_t = WideChar;
  PSTR = PAnsiChar;
  PPSTR = ^PSTR;
  PCSTR = PAnsiChar;
  PPCSTR = ^PCSTR;
  PCWSTR = PWideChar;
  PPCWSTR = ^PCWSTR;
  PWSTR = PWideChar;
  PPWSTR = ^PWSTR;
  PPUCHAR = ^PUCHAR;
  PLARGE_INTEGER = ^LARGE_INTEGER;
  PPBYTE = ^PBYTE;
  GUID = TGUID;
  HANDLE = THandle;
  PSECURITY_ATTRIBUTES = ^SECURITY_ATTRIBUTES;
  LPOVERLAPPED = POVERLAPPED;

type
  UNICODE_STRING = record
    Length: USHORT;
    MaximumLength: USHORT;
    Buffer: PWSTR;
  end;
  PUNICODE_STRING = ^UNICODE_STRING;

function RtlNtStatusToDosError(Status: NTSTATUS): ULONG; stdcall; external 'ntdll.dll';

const
  FILE_SKIP_COMPLETION_PORT_ON_SUCCESS = $1;
  FILE_SKIP_SET_EVENT_ON_HANDLE        = $2;

function SetFileCompletionNotificationModes(FileHandle: THandle; Flags: UCHAR): BOOL; stdcall; external kernel32;

function CreateProcessWithLogonW(
           lpUsername: LPCWSTR;  // LPCWSTR
           lpDomain: LPCWSTR;  // LPCWSTR
           lpPassword: LPCWSTR; // LPCWSTR
           dwLogonFlags: DWORD; // DWORD
           lpApplicationName: LPCWSTR; // LPCWSTR
           lpCommandLine: LPWSTR;  // LPWSTR
           dwCreationFlags: DWORD; // DWORD
           lpEnvironment: LPVOID; // LPVOID
           lpCurrentDirectory: LPCWSTR;  // LPCWSTR
           const lpStartupInfo: TStartupInfoW;  // LPSTARTUPINFOW
           var lpProcessInfo: TProcessInformation): BOOL; stdcall; external advapi32; // LPPROCESS_INFORMATION

type
  _FILE_FS_SECTOR_SIZE_INFORMATION = record
    LogicalBytesPerSector: ULONG; // Logical bytes per sector reported by physical storage. This is the same value as the block size for used for Logical Block Addressing (LBA).
    PhysicalBytesPerSectorForAtomicity: ULONG; // Actual bytes per sector reported by physical storage used for an atomic write.
    PhysicalBytesPerSectorForPerformance: ULONG; // Bytes per sector reported by physical storage for best performance.
    FileSystemEffectivePhysicalBytesPerSectorForAtomicity: ULONG; // The portion of PhysicalBytesPerSectorForAtomicity considered as the physical sector size by the file system
    Flags: ULONG; // Flags for sector alignment and performance capabilities.
    ByteOffsetForSectorAlignment: ULONG;  // The offset value, in bytes, used to align the partition to a physical sector boundary. This member is set to SSINFO_OFFSET_UNKNOWN if proper device information is not available to calculate the value.
    ByteOffsetForPartitionAlignment: ULONG; // The offset, in bytes, of the beginning of the first logical sector within the first physical sector. This member is set to SSINFO_OFFSET_UNKNOWN if proper device information is not available to calculate the value.
  end;
  FILE_FS_SECTOR_SIZE_INFORMATION = _FILE_FS_SECTOR_SIZE_INFORMATION;
  PFILE_FS_SECTOR_SIZE_INFORMATION = ^FILE_FS_SECTOR_SIZE_INFORMATION;

Type
  _IO_STATUS_BLOCK = record
    //union {
    Status: NTSTATUS;
    //    PVOID Pointer;
    //}
    Information: ULONG_PTR;
  end;
  IO_STATUS_BLOCK = _IO_STATUS_BLOCK;
  PIO_STATUS_BLOCK = ^IO_STATUS_BLOCK;

Type
  _FSINFOCLASS = (
    FileFsVolumeInformation = 1,
    FileFsLabelInformation = 2,
    FileFsSizeInformation = 3,
    FileFsDeviceInformation = 4,
    FileFsAttributeInformation = 5,
    FileFsControlInformation = 6,
    FileFsFullSizeInformation = 7,
    FileFsObjectIdInformation = 8,
    FileFsDriverPathInformation = 9,
    FileFsVolumeFlagsInformation = 10,
    FileFsSectorSizeInformation = 11);
  FS_INFORMATION_CLASS = _FSINFOCLASS;
  PFS_INFORMATION_CLASS = ^FS_INFORMATION_CLASS;

function NtQueryVolumeInformationFile(
           FileHandle: THANDLE;
           IoStatusBlock: PIO_STATUS_BLOCK;
           FsInformation: PVOID;
           Length: ULONG;
           FsInformationClass: FS_INFORMATION_CLASS): NTSTATUS; stdcall; external 'ntdll.dll';

const
  STATUS_SUCCESS = $00000000;

const
  INVALID_SET_FILE_POINTER = DWORD(-1);
  LOGON_WITH_PROFILE        = $1;
  LOGON_NETCREDENTIALS_ONLY = $2;

procedure ALCheckWinApiErrorCode(const AExtraInfo: String; const AErrorCode: DWORD; const AModuleHandle: HMODULE = 0); overload; inline;
procedure ALCheckWinApiNTStatus(const AExtraInfo: String; const AStatus: NTSTATUS; const AModuleHandle: HMODULE = 0); overload; inline;
procedure ALCheckWinApiBoolean(const AExtraInfo: String; const ABoolean: Boolean; const AModuleHandle: HMODULE = 0); overload; inline;
function  ALCheckWinApiHandle(const AExtraInfo: String; const AHandle: THandle; const AModuleHandle: HMODULE = 0): THandle; overload; inline;
function  ALCheckWinApiPointer(const AExtraInfo: String; const APointer: Pointer; const AModuleHandle: HMODULE = 0): Pointer; overload; inline;
procedure ALCheckWinApiErrorCode(const AErrorCode: DWORD; const AModuleHandle: HMODULE = 0); overload; inline;
procedure ALCheckWinApiNTStatus(const AStatus: NTSTATUS; const AModuleHandle: HMODULE = 0); overload; inline;
procedure ALCheckWinApiBoolean(const ABoolean: Boolean; const AModuleHandle: HMODULE = 0); overload; inline;
function  ALCheckWinApiHandle(const AHandle: THandle; const AModuleHandle: HMODULE = 0): THandle; overload; inline;
function  ALCheckWinApiPointer(const APointer: Pointer; const AModuleHandle: HMODULE = 0): Pointer; overload; inline;

implementation

uses
  System.SysUtils;

{********************************************************************************************************************}
procedure ALCheckWinApiErrorCode(const AExtraInfo: String; const AErrorCode: DWORD; const AModuleHandle: HMODULE = 0);
begin
  if AErrorCode <> 0 then begin
    var LErrorMessage := SysErrorMessage(AErrorCode, AModuleHandle);
    if (LErrorMessage = '') and (AModuleHandle <> 0) then LErrorMessage := SysErrorMessage(AErrorCode);
    if LErrorMessage = '' then LErrorMessage := 'A call to an OS function failed';
    if (AExtraInfo <> '') then raise Exception.CreateFmt('%s (code=%d, hex=0x%X) - %s', [LErrorMessage, AErrorCode, AErrorCode, AExtraInfo])
    else raise Exception.CreateFmt('%s (code=%d, hex=0x%X)', [LErrorMessage, AErrorCode, AErrorCode]);
  end;
end;

{*******************************************************************************************************************}
procedure ALCheckWinApiNTStatus(const AExtraInfo: String; const AStatus: NTSTATUS; const AModuleHandle: HMODULE = 0);
begin
  if AStatus < 0 then begin
    var LWin32Error: Cardinal := Cardinal(RtlNtStatusToDosError(AStatus));
    var LErrorMessage: String;
    if LWin32Error <> 0 then begin
      LErrorMessage := SysErrorMessage(LWin32Error, AModuleHandle);
      if (LErrorMessage = '') and (AModuleHandle <> 0) then LErrorMessage := SysErrorMessage(LWin32Error);
    end
    else LErrorMessage := '';
    if LErrorMessage = '' then LErrorMessage := 'A call to an NT API function failed';
    if (AExtraInfo <> '') then raise Exception.CreateFmt('%s (ntstatus=%d) - %s', [LErrorMessage, AStatus, AExtraInfo])
    else raise Exception.CreateFmt('%s (ntstatus=%d)', [LErrorMessage, AStatus]);
  end;
end;

{******************************************************************************************************************}
procedure ALCheckWinApiBoolean(const AExtraInfo: String; const ABoolean: Boolean; const AModuleHandle: HMODULE = 0);
begin
  If not ABoolean then begin
    var LLastError := GetLastError;
    If LLastError = 0 then begin
      if AExtraInfo <> '' then raise Exception.CreateFmt('A call to an OS function failed - %s', [AExtraInfo])
      else raise Exception.Create('A call to an OS function failed');
    end
    else
      ALCheckWinApiErrorCode(AExtraInfo, LLastError, AModuleHandle);
  end;
end;

{************************************************************************************************************************}
function ALCheckWinApiHandle(const AExtraInfo: String; const AHandle: THandle; const AModuleHandle: HMODULE = 0): THandle;
begin
  ALCheckWinApiBoolean(
    AExtraInfo, // const AExtraInfo: String;
    (AHandle <> 0) and (AHandle <> INVALID_HANDLE_VALUE), // const ABoolean: Boolean;
    AModuleHandle); // const AModuleHandle: HMODULE = 0)
  Result := AHandle;
end;

{**************************************************************************************************************************}
function ALCheckWinApiPointer(const AExtraInfo: String; const APointer: Pointer; const AModuleHandle: HMODULE = 0): Pointer;
begin
  ALCheckWinApiBoolean(
    AExtraInfo, // const AExtraInfo: String;
    APointer <> nil, // const ABoolean: Boolean;
    AModuleHandle); // const AModuleHandle: HMODULE = 0)
  Result := APointer;
end;

{******************************************************************************************}
procedure ALCheckWinApiErrorCode(const AErrorCode: DWORD; const AModuleHandle: HMODULE = 0);
begin
  ALCheckWinApiErrorCode(''{AExtraInfo}, AErrorCode, AModuleHandle);
end;

{*****************************************************************************************}
procedure ALCheckWinApiNTStatus(const AStatus: NTSTATUS; const AModuleHandle: HMODULE = 0);
begin
  ALCheckWinApiNTStatus(''{AExtraInfo}, AStatus, AModuleHandle);
end;

{****************************************************************************************}
procedure ALCheckWinApiBoolean(const ABoolean: Boolean; const AModuleHandle: HMODULE = 0);
begin
  ALCheckWinApiBoolean(''{AExtraInfo}, ABoolean, AModuleHandle);
end;

{**********************************************************************************************}
function ALCheckWinApiHandle(const AHandle: THandle; const AModuleHandle: HMODULE = 0): THandle;
begin
  Result := ALCheckWinApiHandle(''{AExtraInfo}, AHandle, AModuleHandle);
end;

{************************************************************************************************}
function ALCheckWinApiPointer(const APointer: Pointer; const AModuleHandle: HMODULE = 0): Pointer;
begin
  Result := ALCheckWinApiPointer(''{AExtraInfo}, APointer, AModuleHandle);
end;

end.