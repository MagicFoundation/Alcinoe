{*******************************************************************************
Description:  Function to launch executable (and wait for termination)
*******************************************************************************}

unit ALExecute;

interface

uses
  winapi.windows,
  system.classes;

const
  SE_CREATE_TOKEN_NAME = 'SeCreateTokenPrivilege';
  SE_ASSIGNPRIMARYTOKEN_NAME = 'SeAssignPrimaryTokenPrivilege';
  SE_LOCK_MEMORY_NAME = 'SeLockMemoryPrivilege';
  SE_INCREASE_QUOTA_NAME = 'SeIncreaseQuotaPrivilege';
  SE_UNSOLICITED_INPUT_NAME = 'SeUnsolicitedInputPrivilege';
  SE_MACHINE_ACCOUNT_NAME = 'SeMachineAccountPrivilege';
  SE_TCB_NAME = 'SeTcbPrivilege';
  SE_SECURITY_NAME = 'SeSecurityPrivilege';
  SE_TAKE_OWNERSHIP_NAME = 'SeTakeOwnershipPrivilege';
  SE_LOAD_DRIVER_NAME = 'SeLoadDriverPrivilege';
  SE_SYSTEM_PROFILE_NAME = 'SeSystemProfilePrivilege';
  SE_SYSTEMTIME_NAME = 'SeSystemtimePrivilege';
  SE_PROF_SINGLE_PROCESS_NAME = 'SeProfileSingleProcessPrivilege';
  SE_INC_BASE_PRIORITY_NAME = 'SeIncreaseBasePriorityPrivilege';
  SE_CREATE_PAGEFILE_NAME = 'SeCreatePagefilePrivilege';
  SE_CREATE_PERMANENT_NAME = 'SeCreatePermanentPrivilege';
  SE_BACKUP_NAME = 'SeBackupPrivilege';
  SE_RESTORE_NAME = 'SeRestorePrivilege';
  SE_SHUTDOWN_NAME = 'SeShutdownPrivilege';
  SE_DEBUG_NAME = 'SeDebugPrivilege';
  SE_AUDIT_NAME = 'SeAuditPrivilege';
  SE_SYSTEM_ENVIRONMENT_NAME = 'SeSystemEnvironmentPrivilege';
  SE_CHANGE_NOTIFY_NAME = 'SeChangeNotifyPrivilege';
  SE_REMOTE_SHUTDOWN_NAME = 'SeRemoteShutdownPrivilege';
  SE_UNDOCK_NAME = 'SeUndockPrivilege';
  SE_SYNC_AGENT_NAME = 'SeSyncAgentPrivilege';
  SE_ENABLE_DELEGATION_NAME = 'SeEnableDelegationPrivilege';
  SE_MANAGE_VOLUME_NAME = 'SeManageVolumePrivilege';

function AlGetEnvironmentString: AnsiString;
function ALWinExec(const aCommandLine: AnsiString;
                   const aCurrentDirectory: AnsiString;
                   const aEnvironment: AnsiString;
                   const aInputStream: Tstream;
                   const aOutputStream: TStream;
                   const aOwnerThread: TThread = nil): Dword; overload;
function ALWinExec(const aCommandLine: AnsiString;
                   const aInputStream: Tstream;
                   const aOutputStream: TStream;
                   const aOwnerThread: TThread = nil): Dword; overload;
procedure ALWinExec(const aCommandLine: AnsiString;
                    const aCurrentDirectory: AnsiString); overload;
procedure ALWinExec(const aUserName: ANSIString;
                    const aPassword: ANSIString;
                    const aCommandLine: ANSIString;
                    const aCurrentDirectory: AnsiString;
                    const aLogonFlags: dword = 0); overload;
function ALWinExecAndWait(const aCommandLine:AnsiString;
                          const aCurrentDirectory: AnsiString;
                          const aEnvironment: AnsiString;
                          const aVisibility : integer):DWORD; overload;
function ALWinExecAndWait(const aCommandLine:AnsiString;
                          const aVisibility : integer):DWORD; overload;
Function ALWinExecAndWaitV2(const aCommandLine: AnsiString;
                            const aVisibility: integer): DWORD;
function ALNTSetPrivilege(const sPrivilege: AnsiString; bEnabled: Boolean): Boolean;
function ALStartService(const aServiceName: AnsiString; const aComputerName: AnsiString = ''; const aTimeOut: integer = 180): boolean;
function ALStopService(const aServiceName: AnsiString; const aComputerName: AnsiString = ''; const aTimeOut: integer = 180): boolean;
function ALMakeServiceAutorestarting(const aServiceName: AnsiString;
                                     const aComputerName: AnsiString = '';
                                     const aTimeToRestartInSec: integer = 60 {one minute by default};
                                     const aTimeToResetInSec: integer = 180 {three minutes by default}): boolean;

implementation

uses
  system.sysutils,
  winapi.messages,
  winapi.winsvc,
  System.Diagnostics,
  ALWindows,
  ALCommon;

{******************************************}
Function AlGetEnvironmentString: AnsiString;
var P, Q : PAnsiChar;
    I : Integer;
begin
  P := PAnsiChar(GetEnvironmentStringsA);
  try

    I := 0;
    Q := P;
    if Q^ <> #0 then begin
      Repeat
        While Q^ <> #0 do begin
         Inc(Q);
         Inc(I);
        end;
        Inc(Q);
        Inc(I);
      Until Q^ = #0;
    end;
    SetLength(Result, I);
    if I > 0 then ALMove(P^, Pointer(Result)^, I);

  finally
    FreeEnvironmentStringsA(P);
  end;
end;

{************************************************}
function ALWinExec(const aCommandLine: AnsiString;
                   const aCurrentDirectory: AnsiString;
                   const aEnvironment: AnsiString;
                   const aInputStream: Tstream;
                   const aOutputStream: TStream;
                   const aOwnerThread: TThread = nil): Dword;

var LOutputReadPipe,LOutputWritePipe: THANDLE;
    LInputReadPipe,LInputWritePipe: THANDLE;

  {-----------------------------}
  procedure InternalProcessInput;
  var LBytesWritten: Dword;
      LBuffer: Tbytes;
      P: cardinal;
  begin
    If (aInputStream <> nil) and (aInputStream.size > 0) then begin
      SetLength(LBuffer,aInputStream.size);
      aInputStream.readBuffer(pointer(LBuffer)^,aInputStream.size);
      P := 0;
      While true do begin
        if (aOwnerThread <> nil) and (aOwnerThread.checkTerminated) then break;
        if not WriteFile(LInputWritePipe,     // handle to file to write to
                         LBuffer[P],          // pointer to data to write to file
                         cardinal(length(LBuffer)) - P, // number of bytes to write
                         LBytesWritten,       // pointer to number of bytes written
                         nil) then RaiseLastOSError; // pointer to structure needed for overlapped I/O
        If LBytesWritten = Dword(length(LBuffer)) then break
        else P := P + LBytesWritten;
      end;
    end;
  end;

  {------------------------------}
  procedure InternalProcessOutput;
  var LBytesInPipe: Cardinal;
      LBytesRead: Dword;
      LBuffer: TBytes;
  begin
    While true do begin

      if (aOwnerThread <> nil) and (aOwnerThread.checkTerminated) then break;

      If not PeekNamedPipe(LOutputReadPipe,  // handle to pipe to copy from
                           nil,              // pointer to data buffer
                           0,                // size, in bytes, of data buffer
                           nil,              // pointer to number of bytes read
                           @LBytesInPipe,    // pointer to total number of bytes available
                           nil) then break;  // pointer to unread bytes in this message

      if LBytesInPipe > 0 then begin
        SetLength(LBuffer, LBytesInPipe);
        if not ReadFile(LOutputReadPipe,             // handle of file to read
                        pointer(LBuffer)^,           // address of buffer that receives data
                        LBytesInPipe,                // number of bytes to read
                        LBytesRead,                  // address of number of bytes read
                        nil) then RaiseLastOSError;  // address of structure for data
        If (LBytesRead > 0) and (aOutputStream <> nil) then aOutputStream.WriteBuffer(pointer(LBuffer)^, LBytesRead);
      end
      else break;

    end;
  end;

Var LProcessInformation: TProcessInformation;
    LStartupInfo: TStartupInfoA;
    LSecurityAttributes: TSecurityAttributes;
    LPEnvironment: Pointer;
    LPCurrentDirectory: Pointer;

begin

  // Set up the security attributes struct.
  LSecurityAttributes.nLength := sizeof(TSecurityAttributes);
  LSecurityAttributes.lpSecurityDescriptor := NiL;
  LSecurityAttributes.bInheritHandle := TRUE;

  // Create the child output pipe.
  if not CreatePipe(LOutputReadPipe,          // address of variable for read handle
                    LOutputWritePipe,         // address of variable for write handle
                    @LSecurityAttributes,     // pointer to security attributes
                    0) then RaiseLastOSError; // number of bytes reserved for pipe

  Try

    // Create the child input pipe.
    if not CreatePipe(LInputReadPipe,           // address of variable for read handle
                      LInputWritePipe,          // address of variable for write handle
                      @LSecurityAttributes,     // pointer to security attributes
                      0) then RaiseLastOSError; // number of bytes reserved for pipe

    Try

      // Set up the start up info struct.
      ZeroMemory(@LStartupInfo,sizeof(TStartupInfo));
      LStartupInfo.cb := sizeof(TStartupInfo);
      LStartupInfo.dwFlags := STARTF_USESTDHANDLES;
      LStartupInfo.hStdOutput := LOutputWritePipe;
      LStartupInfo.hStdInput  := LInputReadPipe;
      LStartupInfo.hStdError  := LOutputWritePipe;

      if aEnvironment <> '' then LPEnvironment := PAnsiChar(aEnvironment)
      else LPEnvironment := nil;
      if aCurrentDirectory <> '' then LPCurrentDirectory := PAnsiChar(aCurrentDirectory)
      else LPCurrentDirectory := nil;

      // Launch the process that you want to redirect.
      if not CreateProcessA(nil,                     // pointer to name of executable module
                            PAnsiChar(aCommandLine), // pointer to command line string
                            @LSecurityAttributes,    // pointer to process security attributes
                            NiL,                     // pointer to thread security attributes
                            TrUE,                    // handle inheritance flag
                            CREATE_NO_WINDOW,        // creation flags
                            LPEnvironment,           // pointer to new environment block
                            LPCurrentDirectory,      // pointer to current directory name
                            LStartupInfo,            // pointer to STARTUPINFO
                            LProcessInformation) then RaiseLastOSError; // pointer to PROCESS_INFORMATION
      Try

        InternalProcessInput;
        while (WaitForSingleObject(LProcessInformation.hProcess, 1{to not use 100% CPU usage}) = WAIT_TIMEOUT) do begin
          InternalProcessOutput;
          if (aOwnerThread <> nil) and (aOwnerThread.checkTerminated) then break;
        end;
        InternalProcessOutput;
        if not GetExitCodeProcess(LProcessInformation.hProcess, Cardinal(Result)) then raiseLastOsError;

      finally
        if not CloseHandle(LProcessInformation.hThread) then raiseLastOsError;
        if not CloseHandle(LProcessInformation.hProcess) then raiseLastOsError;
      end;

    Finally
      if not CloseHandle(LInputReadPipe) then raiseLastOsError;
      if not CloseHandle(LInputWritePipe) then raiseLastOsError;
    end;

  Finally
    if not CloseHandle(LOutputReadPipe) then raiseLastOsError;
    if not CloseHandle(LOutputWritePipe) then raiseLastOsError;
  end;

end;

{************************************************}
function ALWinExec(const aCommandLine: AnsiString;
                   const aInputStream: Tstream;
                   const aOutputStream: TStream;
                   const aOwnerThread: TThread = nil): Dword;
Begin
  Result := ALWinExec(aCommandLine,
                      '',
                      '',
                      aInputStream,
                      aOutputStream,
                      aOwnerThread);
End;

{*************************************************}
procedure ALWinExec(const aCommandLine: AnsiString;
                    const aCurrentDirectory: AnsiString);
Var LProcessInformation: TProcessInformation;
    LStartupInfo: TStartupInfoA;
    LSecurityAttributes: TSecurityAttributes;
    LPCurrentDirectory: Pointer;
begin

  // Set up the security attributes struct.
  LSecurityAttributes.nLength := sizeof(TSecurityAttributes);
  LSecurityAttributes.lpSecurityDescriptor := nil;
  LSecurityAttributes.bInheritHandle := true;

  // Set up the start up info struct.
  ZeroMemory(@LStartupInfo,sizeof(TStartupInfo));
  LStartupInfo.cb := sizeof(TStartupInfo);
  LStartupInfo.dwFlags := STARTF_USESTDHANDLES;

  if aCurrentDirectory <> '' then LPCurrentDirectory := PAnsiChar(aCurrentDirectory)
  else LPCurrentDirectory := nil;

  // Launch the process
  if not CreateProcessA(nil,                     // pointer to name of executable module
                        PAnsiChar(aCommandLine), // pointer to command line string
                        @LSecurityAttributes,    // pointer to process security attributes
                        nil,                     // pointer to thread security attributes
                        false,                   // handle inheritance flag
                        CREATE_NO_WINDOW,        // creation flags
                        nil,                     // pointer to new environment block
                        LPCurrentDirectory,      // pointer to current directory name
                        LStartupInfo,            // pointer to STARTUPINFO
                        LProcessInformation)     // pointer to PROCESS_INFORMATION
  then RaiseLastOSError;

end;

{**********************************************}
Procedure ALWinExec(const aUserName: ANSIString;
                    const aPassword: ANSIString;
                    const aCommandLine: ANSIString;
                    const aCurrentDirectory: AnsiString;
                    const aLogonFlags: dword = 0);
var LStartupInfo: TStartupInfoW;
    LProcessInformation: TProcessInformation;
    LCommandLineW: WideString;
begin

  // clear the startup info and set count of bytes to read
  ZeroMemory(@LStartupInfo, SizeOf(TStartupInfoW));
  LStartupInfo.cb := SizeOf(TStartupInfoW);

  // http://msdn.microsoft.com/en-us/library/windows/desktop/ms682431(v=vs.85).aspx
  // The function can modify the contents of this string. Therefore, this parameter cannot be a pointer
  // to read-only memory (such as a const variable or a literal string). If this parameter is a
  // constant string, the function may cause an access violation.
  // ...
  // Because argv[0] is the module name, C programmers typically repeat the module name as the first
  // token in the command line.
  LCommandLineW := String(aCommandLine);

  // try to create the process under specified user
  if not CreateProcessWithLogonW(PWideChar(String(aUserName)), // The name of the user.
                                 nil, // The name of the domain or server whose account database contains the lpUsername account
                                 PWideChar(String(aPassword)), // The clear-text password for the lpUsername account.
                                 aLogonFlags, // The logon option.
                                 nil, // The name of the module to be executed.
                                 PWideChar(LCommandLineW), // The command line to be executed.
                                 0, // The flags that control how the process is created.
                                 nil, // a pointer to an environment block for the new process. If this parameter is NULL, the new process uses an environment created from the profile of the user specified by lpUsername.
                                 PwideChar(string(aCurrentDirectory)), // The full path to the current directory for the process.
                                 LStartupInfo, // a pointer to a STARTUPINFO or STARTUPINFOEX structure.
                                 LProcessInformation) // A pointer to a PROCESS_INFORMATION structure that receives identification information for the new process
  then raiseLastOsError;
  CloseHandle(LProcessInformation.hProcess);
  CloseHandle(LProcessInformation.hThread);

end;

{******************************************************}
function ALWinExecAndWait(const aCommandLine:AnsiString;
                          const aCurrentDirectory: AnsiString;
                          const aEnvironment: AnsiString;
                          const aVisibility : integer):DWORD;
var StartupInfo:TStartupInfoA;
    ProcessInfo:TProcessInformation;
    PEnvironment: Pointer;
    PCurrentDirectory: Pointer;
begin

  if aEnvironment <> '' then PEnvironment := PAnsiChar(aEnvironment)
  else PEnvironment := nil;
  if aCurrentDirectory <> '' then PCurrentDirectory := PAnsiChar(aCurrentDirectory)
  else PCurrentDirectory := nil;

  FillChar(StartupInfo,Sizeof(StartupInfo),#0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := aVisibility;
  if not CreateProcessA(nil,
                        PAnsiChar(aCommandLine),       { pointer to command line string }
                        nil,                           { pointer to process security attributes}
                        nil,                           { pointer to thread security attributes }
                        false,                         { handle inheritance flag }
                        CREATE_NEW_CONSOLE or          { creation flags }
                        NORMAL_PRIORITY_CLASS,
                        PEnvironment,                 { pointer to new environment block }
                        PCurrentDirectory,            { pointer to current directory name }
                        StartupInfo,                   { pointer to STARTUPINFO }
                        ProcessInfo)                   { pointer to PROCESS_INF }
  then Result := DWORD(-1)
  else begin
    WaitforSingleObject(ProcessInfo.hProcess,INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess,Result);
    CloseHandle( ProcessInfo.hProcess );
    CloseHandle( ProcessInfo.hThread );
  end;

end;

{******************************************************}
function ALWinExecAndWait(const aCommandLine:AnsiString;
                          const aVisibility : integer):DWORD;
begin
  result := ALWinExecAndWait(aCommandLine,
                             '', // aCurrentDirectory
                             '', // aEnvironment
                             aVisibility);
end;

{*********************************************************}
{*  ALWinExecAndWaitV2:                                   }
{*  The routine will process paint messages and messages  }
{*  send from other threads while it waits.               }
Function ALWinExecAndWaitV2(const aCommandLine: AnsiString;
                            const aVisibility: integer): DWORD;

  {------------------------------------------}
  Procedure WaitFor( processHandle: THandle );
  Var msg: TMsg;
      ret: DWORD;
  Begin
    Repeat

      ret := MsgWaitForMultipleObjects(1,               { 1 handle to wait on }
                                       processHandle,   { the handle }
                                       False,           { wake on any event }
                                       INFINITE,        { wait without timeout }
                                       QS_PAINT or      { wake on paint messages }
                                       QS_SENDMESSAGE); { or messages from other threads }

      If ret = WAIT_FAILED Then Exit;
      If ret = (WAIT_OBJECT_0 + 1) Then
        While PeekMessage( msg, 0, WM_PAINT, WM_PAINT, PM_REMOVE ) Do
          DispatchMessage( msg );

    Until ret = WAIT_OBJECT_0;
  End;

Var StartupInfo:TStartupInfoA;
    ProcessInfo:TProcessInformation;
Begin
  FillChar(StartupInfo,Sizeof(StartupInfo),#0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := aVisibility;
  If not CreateProcessA(nil,
                        PAnsiChar(aCommandLine),  { pointer to command line string }
                        nil,                      { pointer to process security attributes }
                        nil,                      { pointer to thread security attributes }
                        false,                    { handle inheritance flag }
                        CREATE_NEW_CONSOLE or     { creation flags }
                        NORMAL_PRIORITY_CLASS,
                        nil,                      { pointer to new environment block }
                        nil,                      { pointer to current directory name }
                        StartupInfo,              { pointer to STARTUPINFO }
                        ProcessInfo)              { pointer to PROCESS_INF }
  Then Result := DWORD(-1)   { failed, GetLastError has error code }
  Else Begin
     Waitfor(ProcessInfo.hProcess);
     GetExitCodeProcess(ProcessInfo.hProcess, Result);
     CloseHandle( ProcessInfo.hProcess );
     CloseHandle( ProcessInfo.hThread );
  End;
End;

{*******************************************************************************************************************}
// taken from http://www.delphi-zone.com/2010/02/how-to-use-the-adjusttokenprivileges-function-to-enable-a-privilege/
function ALNTSetPrivilege(const sPrivilege: AnsiString; bEnabled: Boolean): Boolean;
var
  hToken: THandle;
  TokenPriv: TOKEN_PRIVILEGES;
  PrevTokenPriv: TOKEN_PRIVILEGES;
  ReturnLength: Cardinal;
begin

  // Only for Windows NT/2000/XP and later.
  if not (Win32Platform = VER_PLATFORM_WIN32_NT) then begin
    Result := False;
    Exit;
  end;

  // obtain the processes token
  if OpenProcessToken(GetCurrentProcess(),
                      TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,
                      hToken) then begin

    try

      // Get the locally unique identifier (LUID) .
      if LookupPrivilegeValueA(nil,
                               PAnsiChar(sPrivilege),
                               TokenPriv.Privileges[0].Luid) then begin

        TokenPriv.PrivilegeCount := 1; // one privilege to set

        case bEnabled of
          True: TokenPriv.Privileges[0].Attributes  := SE_PRIVILEGE_ENABLED;
          False: TokenPriv.Privileges[0].Attributes := 0;
        end;

        ReturnLength := 0; // replaces a var parameter
        PrevTokenPriv := TokenPriv;

        // enable or disable the privilege
        AdjustTokenPrivileges(hToken,
                              False,
                              TokenPriv,
                              SizeOf(PrevTokenPriv),
                              PrevTokenPriv,
                              ReturnLength);

      end;

    finally
      CloseHandle(hToken);
    end;

  end;

  // test the return value of AdjustTokenPrivileges.
  Result := GetLastError = ERROR_SUCCESS;
  if not Result then
    raise Exception.Create(SysErrorMessage(GetLastError));

end;

{***********************************************************}
{Computer name could be a network name, for example \\SERVER.
 If empty this will be applied to local machine.}
function ALStartService(const aServiceName: AnsiString; const aComputerName: AnsiString = ''; const aTimeOut: integer = 180): boolean;
var LServiceStatus: TServiceStatus;
    LServiceManager: SC_HANDLE;
    LServiceInstance: SC_HANDLE;
    LServiceArgVectors: PansiChar;
    LStopwatch: TStopwatch;
begin
  result := False;
  LServiceManager := OpenSCManagerA(PAnsiChar(aComputerName), nil, SC_MANAGER_CONNECT);
  try

    if LServiceManager > 0 then begin

      LServiceInstance := OpenServiceA(LServiceManager, PAnsiChar(aServiceName), SERVICE_START or SERVICE_QUERY_STATUS);
      try

        if LServiceInstance > 0 then begin

          // NOTICE: even if this function fails we need to do the QueryServiceStatus,
          // because it could fail if the service is already running, so we don't
          // check result of this function.
          LServiceArgVectors := nil;
          StartServiceA(LServiceInstance, 0, LServiceArgVectors);
          if QueryServiceStatus(LServiceInstance, LServiceStatus) then begin

            LStopwatch := TStopwatch.StartNew;
            while LServiceStatus.dwCurrentState = SERVICE_START_PENDING do begin
              if LStopwatch.ElapsedMilliseconds > aTimeOut * 1000 then exit;
              Sleep(1000); // sleep one seconds
              if (not QueryServiceStatus(LServiceInstance, LServiceStatus)) then Exit;
            end;
            result := (LServiceStatus.dwCurrentState = SERVICE_RUNNING);

          end;

        end;

      finally
        CloseServiceHandle(LServiceInstance);
      end;

    end;

  finally
    CloseServiceHandle(LServiceManager);
  end;
end;

{***********************************************************}
{Computer name could be a network name, for example \\SERVER.
 If empty this will be applied to local machine.}
function ALStopService(const aServiceName: AnsiString; const aComputerName: AnsiString = ''; const aTimeOut: integer = 180): boolean;
var LServiceStatus: TServiceStatus;
    LServiceManager: SC_HANDLE;
    LServiceInstance: SC_HANDLE;
    LStopwatch: TStopwatch;
begin
  result := false;
  LServiceManager := OpenSCManagerA(PAnsiChar(aComputerName), nil, SC_MANAGER_CONNECT);
  try

    if LServiceManager > 0 then begin

      LServiceInstance := OpenServiceA(LServiceManager, PAnsiChar(aServiceName), SERVICE_STOP or SERVICE_QUERY_STATUS);
      try

        if LServiceInstance > 0 then begin

          // NOTICE: even if this function fails, we need to do QueryServiceStatus
          // because it can fail if the service is already stopped, so we don't check
          // result of this function.
          ControlService(LServiceInstance, SERVICE_CONTROL_STOP, LServiceStatus);
          if QueryServiceStatus(LServiceInstance, LServiceStatus) then begin

            LStopwatch := TStopwatch.StartNew;
            while LServiceStatus.dwCurrentState = SERVICE_STOP_PENDING do begin
              if LStopwatch.ElapsedMilliseconds > aTimeOut * 1000 then exit;
              Sleep(1000); // sleep 1 second
              if (not QueryServiceStatus(LServiceInstance, LServiceStatus)) then break;
            end;
            result := (LServiceStatus.dwCurrentState = SERVICE_STOPPED);

          end;

        end;

      finally
        CloseServiceHandle(LServiceInstance);
      end;

    end;

  finally
    CloseServiceHandle(LServiceManager);
  end;
end;

{************************************************************}
{The Service Manager will try to restart the service waiting
 between the attempting "aTimeToRestartInSec" seconds. In the
 case of three consistent fails it will try to wait "aTimeToResetInSec"
 seconds then counter of fails will be erased and it will try to
 launch again.
 Computer name could be a network name, for example \\SERVER.
 If empty this will be applied to local machine.}
function ALMakeServiceAutorestarting(const aServiceName: AnsiString;
                                     const aComputerName: AnsiString = '';
                                     const aTimeToRestartInSec: integer = 60 {one minute by default};
                                     const aTimeToResetInSec: integer = 180 {three minutes by default}): boolean;
var LServiceFailureActions: SERVICE_FAILURE_ACTIONS;
    LFailActions: array[1..1] of SC_ACTION;
    LServiceManager: SC_HANDLE;
    LServiceInstance: SC_HANDLE;
begin
  result := false;
  LServiceManager := OpenSCManagerA(PAnsiChar(aComputerName), nil, SC_MANAGER_CONNECT);
  try

    if LServiceManager > 0 then begin

      LServiceInstance := OpenServiceA(LServiceManager, PAnsiChar(aServiceName), SERVICE_ALL_ACCESS);
      try

        if LServiceInstance > 0 then begin

          LFailActions[1].&Type := SC_ACTION_RESTART;
          LFailActions[1].Delay := aTimeToRestartInSec * 1000; // must be in milliseconds

          LServiceFailureActions.dwResetPeriod := aTimeToResetInSec;  // must be in seconds, so don't need to do x1000
          LServiceFailureActions.cActions      := 1;
          LServiceFailureActions.lpRebootMsg   := nil;
          LServiceFailureActions.lpCommand     := nil;
          LServiceFailureActions.lpsaActions   := @LFailActions;

          result := ChangeServiceConfig2(LServiceInstance, SERVICE_CONFIG_FAILURE_ACTIONS, @LServiceFailureActions);

        end;

      finally
        CloseServiceHandle(LServiceInstance);
      end;

    end;

  finally
    CloseServiceHandle(LServiceManager);
  end;
end;

end.
