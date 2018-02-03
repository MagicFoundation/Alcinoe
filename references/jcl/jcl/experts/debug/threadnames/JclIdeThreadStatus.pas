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
{ The Original Code is JclIdeThreadStatus.pas.                                                     }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones.                                        }
{ Portions created by Petr Vones are Copyright (C) of Petr Vones.                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Delphi IDE debugger Thread Status window extension.                                              }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclIdeThreadStatus;

{$I jcl.inc}

interface

uses
  Windows, Classes, SysUtils;

procedure RegisterThread(ThreadID: DWORD; const ThreadName: string); overload;
procedure RegisterThread(Thread: TThread; const ThreadName: string; IncludeClassName: Boolean = True); overload;

procedure UnregisterThread(ThreadID: DWORD); overload;
procedure UnregisterThread(Thread: TThread); overload;

procedure ChangeThreadName(ThreadID: DWORD; const ThreadName: string); overload;
procedure ChangeThreadName(Thread: TThread; const ThreadName: string; IncludeClassName: Boolean = True); overload;

function ThreadNamesAvailable: Boolean;

implementation

uses
  JclDebug, JclPeImage, JclSysUtils,
  ThreadExpertSharedNames;

type
  PThreadRec = ^TThreadRec;
  TThreadRec = record
    Func: TThreadFunc;
    Parameter: Pointer;
  end;

var
  SharedThreadNames: TSharedThreadNames;
  HookImports: TJclPeMapImgHooks;
  Kernel32_CreateThread: function(lpThreadAttributes: Pointer;
    dwStackSize: DWORD; lpStartAddress: TFNThreadStartRoutine;
    lpParameter: Pointer; dwCreationFlags: DWORD; var lpThreadId: DWORD): THandle; stdcall;
  Kernel32_ExitThread: procedure(dwExitCode: DWORD); stdcall;
  {$IFDEF DELPHI7_UP}
  Kernel32_ResumeThread: function(hThread: THandle): DWORD; stdcall;
  {$ENDIF DELPHI7_UP}

function NewCreateThread(lpThreadAttributes: Pointer;
  dwStackSize: DWORD; lpStartAddress: TFNThreadStartRoutine;
  lpParameter: Pointer; dwCreationFlags: DWORD; var lpThreadId: DWORD): THandle; stdcall;
var
  Instance: TObject;
begin
  Result := Kernel32_CreateThread(lpThreadAttributes, dwStackSize, lpStartAddress,
    lpParameter, dwCreationFlags, lpThreadId);
  if (Result <> 0) and (lpParameter <> nil) then
  try
    Instance := PThreadRec(lpParameter)^.Parameter;
    if Instance is TJclDebugThread then
      RegisterThread(TJclDebugThread(Instance), TJclDebugThread(Instance).ThreadName, True)
    else
    if Instance is TThread then
      RegisterThread(TThread(Instance), '', True);
  except
  end;
end;

procedure NewExitThread(dwExitCode: DWORD); stdcall;
var
  ThreadID: DWORD;
begin
  ThreadID := GetCurrentThreadId;
  try
    UnregisterThread(ThreadID);
  except
  end;
  Kernel32_ExitThread(dwExitCode);
end;

{$IFDEF DELPHI7_UP}
function NewResumeThread(hThread: THandle): DWORD; stdcall;
begin
  Result := Kernel32_ResumeThread(hThread);
  if Result <= 1 then
  try
    SharedThreadNames.UpdateResumeStatus;
  except
  end;
end;
{$ENDIF DELPHI7_UP}

function CreateThreadName(const ThreadName, ThreadClassName: string): string;
begin
  if ThreadClassName <> '' then
  begin
    if ThreadName = '' then
      Result := Format('[%s]', [ThreadClassName])
    else
      Result := Format('[%s] "%s"', [ThreadClassName, ThreadName]);
  end
  else
    Result := Format('"%s"', [ThreadName]);
end;

procedure RegisterThread(ThreadID: DWORD; const ThreadName: string);
begin
  if Assigned(SharedThreadNames) then
    SharedThreadNames.RegisterThread(ThreadID, CreateThreadName(ThreadName, ''));
end;

procedure RegisterThread(Thread: TThread; const ThreadName: string; IncludeClassName: Boolean);
begin
  if Assigned(SharedThreadNames) then
    SharedThreadNames.RegisterThread(Thread.ThreadID, CreateThreadName(ThreadName, Thread.ClassName));
end;

procedure UnregisterThread(ThreadID: DWORD);
begin
  if Assigned(SharedThreadNames) then
    SharedThreadNames.UnregisterThread(ThreadID);
end;

procedure UnregisterThread(Thread: TThread);
begin
  if Assigned(SharedThreadNames) then
    SharedThreadNames.UnregisterThread(Thread.ThreadID);
end;

procedure ChangeThreadName(ThreadID: DWORD; const ThreadName: string);
begin
  if Assigned(SharedThreadNames) then
    SharedThreadNames[ThreadID] := CreateThreadName(ThreadName, '');
end;

procedure ChangeThreadName(Thread: TThread; const ThreadName: string; IncludeClassName: Boolean);
begin
  if Assigned(SharedThreadNames) then
    SharedThreadNames[Thread.ThreadID] := CreateThreadName(ThreadName, Thread.ClassName);
end;

function ThreadNamesAvailable: Boolean;
begin
  Result := Assigned(SharedThreadNames);
end;

procedure Init;
begin
  if IsDebuggerAttached and TSharedThreadNames.Exists then
  begin
    SharedThreadNames := TSharedThreadNames.Create(False);
    HookImports := TJclPeMapImgHooks.Create;
    with HookImports do
    begin
      HookImport(SystemBase, kernel32, 'CreateThread', @NewCreateThread, @Kernel32_CreateThread);
      HookImport(SystemBase, kernel32, 'ExitThread', @NewExitThread, @Kernel32_ExitThread);
      {$IFDEF DELPHI7_UP}
      HookImport(SystemBase, kernel32, 'ResumeThread', @NewResumeThread, @Kernel32_ResumeThread);
      {$ENDIF DELPHI7_UP}
    end;
  end;
end;

initialization
  Init;

finalization
  FreeAndNil(HookImports);
  FreeAndNil(SharedThreadNames);

end.
