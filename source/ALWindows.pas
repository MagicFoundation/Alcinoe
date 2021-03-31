{*******************************************************************************
Description:  Windows API function not (yet) in the windows.pas
*******************************************************************************}

unit ALWindows;

interface

{$MINENUMSIZE 4} // https://stackoverflow.com/questions/48953749/why-this-c-to-pascal-conversion-crash

uses
  Winapi.Windows;

type
  _MEMORYSTATUSEX = record
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
  MEMORYSTATUSEX = _MEMORYSTATUSEX;
  LPMEMORYSTATUSEX = ^MEMORYSTATUSEX;

{$IF CompilerVersion < 32} // tokyo
function GetTickCount64: UInt64; stdcall; external kernel32; // Windows Vista / Windows Server 2008
{$ENDIF}
function GlobalMemoryStatusEx(var lpBuffer : TMEMORYSTATUSEX): BOOL; stdcall; external kernel32; // Windows XP / Windows Server 2003
function CreateProcessWithLogonW(lpUsername: LPCWSTR;  // LPCWSTR
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
function AttachConsole(dwProcessId: DWORD): BOOL; stdcall; external kernel32;
function ALUserExists(const aUserName: AnsiString): boolean;

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
  NTSTATUS = LONG;

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
  _FSINFOCLASS = (FileFsVolumeInformation = 1,
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

function NtQueryVolumeInformationFile(FileHandle: THANDLE;
                                      IoStatusBlock: PIO_STATUS_BLOCK;
                                      FsInformation: PVOID;
                                      Length: ULONG;
                                      FsInformationClass: FS_INFORMATION_CLASS): NTSTATUS; stdcall; external 'ntdll.dll'; // Available starting with Windows XP.

const
  STATUS_SUCCESS = $00000000;

const
  INVALID_SET_FILE_POINTER = DWORD(-1);
  QUOTA_LIMITS_HARDWS_MIN_DISABLE = $2;
  QUOTA_LIMITS_HARDWS_MIN_ENABLE  = $1;
  QUOTA_LIMITS_HARDWS_MAX_DISABLE = $8;
  QUOTA_LIMITS_HARDWS_MAX_ENABLE  = $4;
  LOGON_WITH_PROFILE        = $1;
  LOGON_NETCREDENTIALS_ONLY = $2;
  ATTACH_PARENT_PROCESS = DWORD(-1);

implementation

uses
  System.Ansistrings,
  System.SysUtils;

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

end.
