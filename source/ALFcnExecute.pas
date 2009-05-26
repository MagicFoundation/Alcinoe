{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      Alcinoe WinExec Functions
Version:      3.51

Description:  Function to launch executable (and wait for termination)

Legal issues: Copyright (C) 1999-2009 by Arkadia Software Engineering

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

Know bug:

History:      04/05/2007: overload the function ALWinExec32;

Link:

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALFcnExecute;

interface

uses windows,
     classes,
     sysutils,
     messages;

function ALWinExec32(const FileName, CurrentDirectory, Environment: string; InputStream: Tstream; OutputStream: TStream): Dword; overload;
function ALWinExec32(const FileName: string; InputStream: Tstream; OutputStream: TStream): Dword; overload;
function ALWinExecAndWait32(FileName:String; Visibility : integer):DWORD;
Function ALWinExecAndWait32V2(FileName: String; Visibility: integer): DWORD;

implementation

{**********************************}
function ALWinExec32(const FileName,
                           CurrentDirectory,
                           Environment: string;
                     InputStream: Tstream;
                     OutputStream: TStream): Dword;

Const AstrBufferSize: Dword = 4096;

Var aProcessInformation: TProcessInformation;
    aStartupInfo: TStartupInfo;
    aSecurityAttributes: TSecurityAttributes;
    aOutputReadPipe,aOutputWritePipe: THANDLE;
    aInputReadPipe,aInputWritePipe: THANDLE;
    aStrBuffer: String;
    aPEnvironment: Pointer;
    aPCurrentDirectory: Pointer;

  {-----------------------------}
  procedure InternalProcessInput;
  var aBytesWritten: Dword;
  begin
    If InputStream.size > 0 then begin
      SetLength(aStrBuffer,InputStream.size);
      InputStream.read(aStrBuffer[1],InputStream.size);
      While true do begin
        if not WriteFile(
                         aInputWritePipe,    // handle to file to write to
                         aStrBuffer[1],      // pointer to data to write to file
                         length(AstrBuffer), // number of bytes to write
                         aBytesWritten,      // pointer to number of bytes written
                         nil                 // pointer to structure needed for overlapped I/O
                        ) then RaiseLastOSError;

        If aBytesWritten = Dword(length(AstrBuffer)) then break
        else delete(aStrBuffer,1,aBytesWritten);

      end;
    end;
  end;

  {------------------------------}
  procedure InternalProcessOutput;
  var aBytesInPipe: Cardinal;
      aBytesRead: Dword;
  begin
    While true do begin
      If not PeekNamedPipe(
                           aOutputReadPipe, // handle to pipe to copy from
                           nil,             // pointer to data buffer
                           0,               // size, in bytes, of data buffer
                           nil,             // pointer to number of bytes read
                           @aBytesInPipe,   // pointer to total number of bytes available
                           nil              // pointer to unread bytes in this message
                          ) then break;

      If aBytesInPipe > 0 then begin
        If aBytesInPipe < AstrBufferSize then aBytesInPipe := AstrBufferSize;

        ReadFile(
                 aOutputReadPipe, // handle of file to read
                 aStrBuffer[1],   // address of buffer that receives data
                 AstrBufferSize,  // number of bytes to read
                 aBytesRead,      // address of number of bytes read
                 nil              // address of structure for data
                );

        If aBytesRead > 0 then OutputStream.Write(aStrBuffer[1], aBytesRead);
      end
      else break;
    end;
  end;


begin

  // Set up the security attributes struct.
  aSecurityAttributes.nLength := sizeof(TSecurityAttributes);
  aSecurityAttributes.lpSecurityDescriptor := NiL;
  aSecurityAttributes.bInheritHandle := TRUE;

  // Create the child output pipe.
  if not CreatePipe(
                    aOutputReadPipe,      // address of variable for read handle
                    aOutputWritePipe,     // address of variable for write handle
                    @aSecurityAttributes, // pointer to security attributes
                    0                     // number of bytes reserved for pipe
                   ) then RaiseLastOSError;
  Try

    // Create the child input pipe.
    if not CreatePipe(
                      aInputReadPipe,       // address of variable for read handle
                      aInputWritePipe,      // address of variable for write handle
                      @aSecurityAttributes, // pointer to security attributes
                      0                     // number of bytes reserved for pipe
                     ) then RaiseLastOSError;
    Try

      // Set up the start up info struct.
      ZeroMemory(@aStartupInfo,sizeof(TStartupInfo));
      aStartupInfo.cb := sizeof(TStartupInfo);
      aStartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      aStartupInfo.wShowWindow := SW_HIDE;
      aStartupInfo.hStdOutput := aOutputWritePipe;
      aStartupInfo.hStdInput  := aInputReadPipe;
      aStartupInfo.hStdError  := aOutputWritePipe;

      if Environment <> '' then aPEnvironment := Pchar(Environment)
      else aPEnvironment := nil;
      if CurrentDirectory <> '' then aPCurrentDirectory := Pchar(CurrentDirectory)
      else aPCurrentDirectory := nil;


      // Launch the process that you want to redirect.
      if not CreateProcess(
                           nil,                     // pointer to name of executable module
                           PChar(FileName),         // pointer to command line string
                           @aSecurityAttributes,    // pointer to process security attributes
                           NiL,                     // pointer to thread security attributes
                           TrUE,                    // handle inheritance flag
                           CREATE_NEW_CONSOLE,      // creation flags
                           aPEnvironment,           // pointer to new environment block
                           aPCurrentDirectory,      // pointer to current directory name
                           aStartupInfo,            // pointer to STARTUPINFO
                           aProcessInformation      // pointer to PROCESS_INFORMATION
                          ) then RaiseLastOSError;
      Try

        InputStream.Position := 0;
        InternalProcessInput;
        SetLength(aStrBuffer,AstrBufferSize);
        while (WaitForSingleObject(aProcessInformation.hProcess, 0) = WAIT_TIMEOUT) do InternalProcessOutput;
        InternalProcessOutput;
        GetExitCodeProcess(aProcessInformation.hProcess, Cardinal(Result));

      finally
        If not CloseHandle(aProcessInformation.hThread) then RaiseLastOSError;
        If not CloseHandle(aProcessInformation.hProcess) then RaiseLastOSError;
      end;

    Finally
      If not CloseHandle(aInputReadPipe) then RaiseLastOSError;
      If not CloseHandle(aInputWritePipe) then RaiseLastOSError;
    end;

  Finally
    If not CloseHandle(aOutputReadPipe) then RaiseLastOSError;
    If not CloseHandle(aOutputWritePipe) then RaiseLastOSError;
  end;
end;

{******************************************}
function ALWinExec32(const FileName: string;
                     InputStream: Tstream;
                     OutputStream: TStream): Dword;
Begin
  Result := ALWinExec32(
                        FileName,
                        '',
                        '',
                        InputStream,
                        OutputStream
                       );
End;

{***********************************************************************}
function ALWinExecAndWait32(FileName:String; Visibility : integer):DWORD;
var StartupInfo:TStartupInfo;
    ProcessInfo:TProcessInformation;
begin
  FillChar(StartupInfo,Sizeof(StartupInfo),#0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  if not CreateProcess(
                       nil,
                       Pchar(FileName),               { pointer to command line string }
                       nil,                           { pointer to process security attributes}
                       nil,                           { pointer to thread security attributes }
                       false,                         { handle inheritance flag }
                       CREATE_NEW_CONSOLE or          { creation flags }
                       NORMAL_PRIORITY_CLASS,
                       nil,                           { pointer to new environment block }
                       nil,                           { pointer to current directory name }
                       StartupInfo,                   { pointer to STARTUPINFO }
                       ProcessInfo                    { pointer to PROCESS_INF }
                      )
  then Result := DWORD(-1)
  else begin
    WaitforSingleObject(ProcessInfo.hProcess,INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess,Result);
    CloseHandle( ProcessInfo.hProcess );
    CloseHandle( ProcessInfo.hThread );
  end;
end;


{**************************************************************************}
{*  ALWinExecAndWait32V2:                                                  }
{*  The routine will process paint messages and messages                   }
{*  send from other threads while it waits.                                }
Function ALWinExecAndWait32V2(FileName: String; Visibility: integer): DWORD;

  {------------------------------------------}
  Procedure WaitFor( processHandle: THandle );
  Var msg: TMsg;
      ret: DWORD;
  Begin
    Repeat

      ret := MsgWaitForMultipleObjects(
               1,             { 1 handle to wait on }
               processHandle, { the handle }
               False,         { wake on any event }
               INFINITE,      { wait without timeout }
               QS_PAINT or    { wake on paint messages }
               QS_SENDMESSAGE { or messages from other threads }
               );
      If ret = WAIT_FAILED Then Exit;
      If ret = (WAIT_OBJECT_0 + 1) Then
        While PeekMessage( msg, 0, WM_PAINT, WM_PAINT, PM_REMOVE ) Do
          DispatchMessage( msg );

    Until ret = WAIT_OBJECT_0;
  End;

Var StartupInfo:TStartupInfo;
    ProcessInfo:TProcessInformation;
Begin
  FillChar(StartupInfo,Sizeof(StartupInfo),#0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  If not CreateProcess(
                       nil,
                       Pchar(FileName),         { pointer to command line string }
                       nil,                     { pointer to process security attributes }
                       nil,                     { pointer to thread security attributes }
                       false,                   { handle inheritance flag }
                       CREATE_NEW_CONSOLE or    { creation flags }
                       NORMAL_PRIORITY_CLASS,
                       nil,                     { pointer to new environment block }
                       nil,                     { pointer to current directory name }
                       StartupInfo,             { pointer to STARTUPINFO }
                       ProcessInfo              { pointer to PROCESS_INF }
                      ) 
  Then Result := DWORD(-1)   { failed, GetLastError has error code }
  Else Begin
     Waitfor(ProcessInfo.hProcess);
     GetExitCodeProcess(ProcessInfo.hProcess, Result);
     CloseHandle( ProcessInfo.hProcess );
     CloseHandle( ProcessInfo.hThread );
  End;
End;

end.
