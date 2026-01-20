unit Alcinoe.Execute;

interface

{$I Alcinoe.inc}

uses
  winapi.windows,
  system.classes;

function AlGetEnvironmentStringA: AnsiString;
function AlGetEnvironmentStringW: String;
function ALWinExecA(
           const aCommandLine: AnsiString;
           const aCurrentDirectory: AnsiString;
           const aEnvironment: AnsiString;
           const aInputStream: Tstream;
           const aOutputStream: TStream;
           const aOwnerThread: TThread = nil): Dword; overload;
function ALWinExecW(
           const aCommandLine: String;
           const aCurrentDirectory: String;
           const aEnvironment: String;
           const aInputStream: Tstream;
           const aOutputStream: TStream;
           const aOwnerThread: TThread = nil): Dword; overload;
function ALWinExecA(
           const aCommandLine: AnsiString;
           const aInputStream: Tstream;
           const aOutputStream: TStream;
           const aOwnerThread: TThread = nil): Dword; overload;
function ALWinExecW(
           const aCommandLine: String;
           const aInputStream: Tstream;
           const aOutputStream: TStream;
           const aOwnerThread: TThread = nil): Dword; overload;
procedure ALWinExecA(
            const aCommandLine: AnsiString;
            const aCurrentDirectory: AnsiString); overload;
procedure ALWinExecA(
            const aUserName: ANSIString;
            const aPassword: ANSIString;
            const aCommandLine: ANSIString;
            const aCurrentDirectory: AnsiString;
            const aLogonFlags: dword = 0); overload;
function ALWinExecAndWaitA(
           const aCommandLine:AnsiString;
           const aCurrentDirectory: AnsiString;
           const aEnvironment: AnsiString;
           const aVisibility : integer):DWORD; overload;
function ALWinExecAndWaitA(
           const aCommandLine:AnsiString;
           const aVisibility : integer):DWORD; overload;

implementation

uses
  system.sysutils,
  Alcinoe.WinApi.Windows,
  Alcinoe.Common;

{*******************************************}
Function AlGetEnvironmentStringA: AnsiString;
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

{***************************************}
Function AlGetEnvironmentStringW: String;
var P, Q : PChar;
    I : Integer;
begin
  P := PChar(GetEnvironmentStringsW);
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
    if I > 0 then ALMove(P^, Pointer(Result)^, I*sizeof(char));

  finally
    FreeEnvironmentStringsW(P);
  end;
end;

{******************}
function ALWinExecA(
           const aCommandLine: AnsiString;
           const aCurrentDirectory: AnsiString;
           const aEnvironment: AnsiString;
           const aInputStream: Tstream;
           const aOutputStream: TStream;
           const aOwnerThread: TThread = nil): Dword;

var LOutputReadPipe,LOutputWritePipe: THANDLE;
    LInputReadPipe,LInputWritePipe: THANDLE;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
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
        if not WriteFile(
                 LInputWritePipe,     // handle to file to write to
                 LBuffer[P],          // pointer to data to write to file
                 cardinal(length(LBuffer)) - P, // number of bytes to write
                 LBytesWritten,       // pointer to number of bytes written
                 nil) then RaiseLastOSError; // pointer to structure needed for overlapped I/O
        If LBytesWritten = Dword(length(LBuffer)) then break
        else P := P + LBytesWritten;
      end;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure InternalProcessOutput;
  var LBytesInPipe: Cardinal;
      LBytesRead: Dword;
      LBuffer: TBytes;
  begin
    While true do begin
      if (aOwnerThread <> nil) and (aOwnerThread.checkTerminated) then break;
      If not PeekNamedPipe(
               LOutputReadPipe,  // handle to pipe to copy from
               nil,              // pointer to data buffer
               0,                // size, in bytes, of data buffer
               nil,              // pointer to number of bytes read
               @LBytesInPipe,    // pointer to total number of bytes available
               nil) then break;  // pointer to unread bytes in this message
      if LBytesInPipe > 0 then begin
        SetLength(LBuffer, LBytesInPipe);
        if not ReadFile(
                 LOutputReadPipe,             // handle of file to read
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
  if not CreatePipe(
           LOutputReadPipe,          // address of variable for read handle
           LOutputWritePipe,         // address of variable for write handle
           @LSecurityAttributes,     // pointer to security attributes
           0) then RaiseLastOSError; // number of bytes reserved for pipe

  Try

    // Create the child input pipe.
    if not CreatePipe(
             LInputReadPipe,           // address of variable for read handle
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
      if not CreateProcessA(
               nil,                     // pointer to name of executable module
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

{******************}
function ALWinExecW(
           const aCommandLine: String;
           const aCurrentDirectory: String;
           const aEnvironment: String;
           const aInputStream: Tstream;
           const aOutputStream: TStream;
           const aOwnerThread: TThread = nil): Dword;

var LOutputReadPipe,LOutputWritePipe: THANDLE;
    LInputReadPipe,LInputWritePipe: THANDLE;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
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
        if not WriteFile(
                 LInputWritePipe,     // handle to file to write to
                 LBuffer[P],          // pointer to data to write to file
                 cardinal(length(LBuffer)) - P, // number of bytes to write
                 LBytesWritten,       // pointer to number of bytes written
                 nil) then RaiseLastOSError; // pointer to structure needed for overlapped I/O
        If LBytesWritten = Dword(length(LBuffer)) then break
        else P := P + LBytesWritten;
      end;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure InternalProcessOutput;
  var LBytesInPipe: Cardinal;
      LBytesRead: Dword;
      LBuffer: TBytes;
  begin
    While true do begin
      if (aOwnerThread <> nil) and (aOwnerThread.checkTerminated) then break;
      If not PeekNamedPipe(
               LOutputReadPipe,  // handle to pipe to copy from
               nil,              // pointer to data buffer
               0,                // size, in bytes, of data buffer
               nil,              // pointer to number of bytes read
               @LBytesInPipe,    // pointer to total number of bytes available
               nil) then break;  // pointer to unread bytes in this message
      if LBytesInPipe > 0 then begin
        SetLength(LBuffer, LBytesInPipe);
        if not ReadFile(
                 LOutputReadPipe,             // handle of file to read
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
    LStartupInfo: TStartupInfo;
    LSecurityAttributes: TSecurityAttributes;
    LPEnvironment: Pointer;
    LPCurrentDirectory: Pointer;

begin

  // Set up the security attributes struct.
  LSecurityAttributes.nLength := sizeof(TSecurityAttributes);
  LSecurityAttributes.lpSecurityDescriptor := NiL;
  LSecurityAttributes.bInheritHandle := TRUE;

  // Create the child output pipe.
  if not CreatePipe(
           LOutputReadPipe,          // address of variable for read handle
           LOutputWritePipe,         // address of variable for write handle
           @LSecurityAttributes,     // pointer to security attributes
           0) then RaiseLastOSError; // number of bytes reserved for pipe

  Try

    // Create the child input pipe.
    if not CreatePipe(
             LInputReadPipe,           // address of variable for read handle
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

      if aEnvironment <> '' then LPEnvironment := PChar(aEnvironment)
      else LPEnvironment := nil;
      if aCurrentDirectory <> '' then LPCurrentDirectory := PChar(aCurrentDirectory)
      else LPCurrentDirectory := nil;

      // Launch the process that you want to redirect.
      if not CreateProcessW(
               nil,                  // pointer to name of executable module
               PChar(aCommandLine),  // pointer to command line string
               @LSecurityAttributes, // pointer to process security attributes
               NiL,                  // pointer to thread security attributes
               TrUE,                 // handle inheritance flag
               CREATE_NO_WINDOW or CREATE_UNICODE_ENVIRONMENT, // creation flags
               LPEnvironment,        // pointer to new environment block
               LPCurrentDirectory,   // pointer to current directory name
               LStartupInfo,         // pointer to STARTUPINFO
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

{******************}
function ALWinExecA(
           const aCommandLine: AnsiString;
           const aInputStream: Tstream;
           const aOutputStream: TStream;
           const aOwnerThread: TThread = nil): Dword;
Begin
  Result := ALWinExecA(
              aCommandLine,
              '',
              '',
              aInputStream,
              aOutputStream,
              aOwnerThread);
End;


{******************}
function ALWinExecW(
           const aCommandLine: String;
           const aInputStream: Tstream;
           const aOutputStream: TStream;
           const aOwnerThread: TThread = nil): Dword;
Begin
  Result := ALWinExecW(
              aCommandLine,
              '',
              '',
              aInputStream,
              aOutputStream,
              aOwnerThread);
End;

{*******************}
procedure ALWinExecA(
            const aCommandLine: AnsiString;
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
  if not CreateProcessA(
           nil,                     // pointer to name of executable module
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

{*******************}
Procedure ALWinExecA(
            const aUserName: ANSIString;
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
  if not CreateProcessWithLogonW(
           PWideChar(String(aUserName)), // The name of the user.
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

{*************************}
function ALWinExecAndWaitA(
           const aCommandLine:AnsiString;
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
  if not CreateProcessA(
           nil,
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

{*************************}
function ALWinExecAndWaitA(
           const aCommandLine:AnsiString;
           const aVisibility : integer):DWORD;
begin
  result := ALWinExecAndWaitA(
              aCommandLine,
              '', // aCurrentDirectory
              '', // aEnvironment
              aVisibility);
end;

end.