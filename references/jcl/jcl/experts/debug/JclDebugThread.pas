{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) extension                                                        }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.0 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclDebugThread.pas.                                                         }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying help file JCL.chm.  }
{ Portions created by these individuals are Copyright (C) of these individuals.                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclDebugThread;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Classes, SysUtils;

procedure RegisterThread(ThreadID: DWORD; const ThreadName: string); overload;
procedure RegisterThread(Thread: TThread; const ThreadName: string; IncludeClassName: Boolean = True); overload;

procedure UnregisterThread(ThreadID: DWORD); overload;
procedure UnregisterThread(Thread: TThread); overload;

procedure ChangeThreadName(ThreadID: DWORD; const ThreadName: string); overload;
procedure ChangeThreadName(Thread: TThread; const ThreadName: string; IncludeClassName: Boolean = True); overload;

function ThreadNamesAvailable: Boolean;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\debug';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

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

  TJclDebugThreadNotifier = class(TObject)
  public
    procedure ThreadRegistered(ThreadID: DWORD);
  end;

var
  SharedThreadNames: TSharedThreadNames;
  HookImports: TJclPeMapImgHooks;
  Notifier: TJclDebugThreadNotifier;
  Kernel32_CreateThread: function (lpThreadAttributes: Pointer;
    dwStackSize: DWORD; lpStartAddress: TFNThreadStartRoutine;
    lpParameter: Pointer; dwCreationFlags: DWORD; var lpThreadId: DWORD): THandle; stdcall;
  Kernel32_ExitThread: procedure (dwExitCode: DWORD); stdcall;

function NewCreateThread(lpThreadAttributes: Pointer;
  dwStackSize: DWORD; lpStartAddress: TFNThreadStartRoutine;
  lpParameter: Pointer; dwCreationFlags: DWORD; var lpThreadId: DWORD): THandle; stdcall;
var
  Instance: TObject;
begin
  Result := Kernel32_CreateThread(lpThreadAttributes, dwStackSize, lpStartAddress, lpParameter, dwCreationFlags, lpThreadId);
  if (Result <> 0) and (lpParameter <> nil) then
  try
    Instance := PThreadRec(lpParameter)^.Parameter;
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
  Kernel32_ExitThread(dwExitCode);
  try
    UnregisterThread(ThreadID);
  except
  end;
end;

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
    end;
    { TODO -oPV -cDesign : TJclDebugThread could hold its name. In case of that the name could be read in hooked CreateThread }
    Notifier := TJclDebugThreadNotifier.Create;
    JclDebugThreadList.OnThreadRegistered := Notifier.ThreadRegistered;
  end;
end;

//=== { TJclDebugThreadNotifier } ============================================

procedure TJclDebugThreadNotifier.ThreadRegistered(ThreadID: DWORD);
begin
  with JclDebugThreadList do
    SharedThreadNames.RegisterThread(ThreadID,
      CreateThreadName(ThreadNames[ThreadID], JclDebugThreadList.ThreadClassNames[ThreadID]));
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  Init;

finalization
  FreeAndNil(HookImports);
  FreeAndNil(SharedThreadNames);
  FreeAndNil(Notifier);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
