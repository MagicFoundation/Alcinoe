{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCLCmdStarter.dpr, released on 2008-11-03.

The Initial Developer of the Original Code is Andreas Hausladen
Portions created by Andreas Hausladen are Copyright (C) 2008 Andreas Hausladen
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JCL home page,
located at http://jcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

program JCLCmdStarter;

{$APPTYPE CONSOLE}

{ JCLCmdStarter is used to get the JCLInstaller into the forground under Windows Vista.
  Under Vista a GUI application will be send to the back if it is started from a console window
  that is closing after triggering the start of the GUI application.

  JCLCmdStarter waits until the GUI application is idle and then terminates. It is also
  a replacement for Windows's "start" command. }

uses
  Windows;

{.$R CommCtrlAsInvoker.res}

function SysErrorMessage(ErrorCode: Cardinal): string;
var
  Buffer: array[0..1024] of Char;
var
  Len: Integer;
begin
  Len := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS or
    FORMAT_MESSAGE_ARGUMENT_ARRAY, nil, ErrorCode, 0, Buffer, Length(Buffer), nil);
  while (Len > 0) and ((Buffer[Len - 1] <= #32) or (Buffer[Len - 1] = '.')) do
    Dec(Len);
  SetString(Result, Buffer, Len);
end;

var
  P: PChar;
  InStr: Boolean;
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
  S: string;
  SecAttr: TSecurityAttributes;
  LastError: Cardinal;
begin
  P := GetCommandLine;
  if P = nil then
  begin
    WriteLn(ErrOutput, 'GetCommandLine returned NULL');
    Halt(1);
  end;

  InStr := False;
  while P[0] <> #0 do
  begin
    if P[0] = '"' then
    begin
      if InStr and (P[1] = '"') then
        Inc(P)
      else
        InStr := not InStr;
    end
    else if (P[0] <= ' ') and not InStr then
      Break;
    Inc(P);
  end;

  if P[0] <> #0 then
  begin
    while (P[0] <> #0) and (P[0] <= #32) do
      Inc(P);
  end;

  if P[0] <> #0 then
  begin
    S := P;

    FillChar(SecAttr, SizeOf(SecAttr), 0);
    SecAttr.nLength := SizeOf(SecAttr);
    SecAttr.bInheritHandle := True;

    FillChar(StartupInfo, SizeOf(StartupInfo), 0);
    if CreateProcess(nil, PChar(S), @SecAttr, nil, True, 0, nil, nil, StartupInfo, ProcessInfo) then
    begin
      CloseHandle(ProcessInfo.hThread);
      WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
      CloseHandle(ProcessInfo.hProcess);
      ExitCode := 0;
    end
    else
    begin
      LastError := GetLastError;

      SetLength(S, 1024);
      SetLength(S, FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS or
                                 FORMAT_MESSAGE_ARGUMENT_ARRAY, nil, LastError, 0, PChar(S), Length(S), nil));
      WriteLn(ErrOutput, S);
      ExitCode := 1;
    end;
  end
  else
  begin
    WriteLn(ErrOutput, 'No executable specified');
    ExitCode := 1;
  end;
end.