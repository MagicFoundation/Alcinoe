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
{ The Original Code is ThreadExpertSharedNames.pas.                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones.                                        }
{ Portions created by Petr Vones are Copyright (C) of Petr Vones.                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit ThreadExpertSharedNames;

{$I jcl.inc}

interface

uses
  Windows, SysUtils, Classes,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase, JclFileUtils, JclSynch;

type
  TSharedThreadNames = class(TObject)
  private
    FIdeMode: Boolean;
    FMapping: TJclSwapFileMapping;
    FMutex: TJclMutex;
    FNotifyEvent: TJclEvent;
    FProcessID: DWORD;
    FReadMutex: TJclMutex;
    FView: TJclFileMappingView;
    function GetThreadName(ThreadID: DWORD): string;
    procedure InternalRegisterThread(ThreadID: DWORD; const ThreadName: string; UpdateOnly: Boolean);
    procedure SetThreadName(ThreadID: DWORD; const Value: string);
  protected
    function EnterMutex: Boolean;
  public
    constructor Create(IdeMode: Boolean);
    destructor Destroy; override;
    procedure Cleanup(ProcessID: DWORD);
    class function Exists: Boolean;
    procedure RegisterThread(ThreadID: DWORD; const ThreadName: string);
    function ThreadNameTimoeut(ThreadID, Timeout: DWORD; var ThreadName: string): Boolean;
    procedure UnregisterThread(ThreadID: DWORD);
    procedure UpdateResumeStatus;
    property ThreadName[ThreadID: DWORD]: string read GetThreadName write SetThreadName; default;
    property NotifyEvent: TJclEvent read FNotifyEvent;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\debug\threadnames';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  // do not reference Ota units there because of the ThreadExceptExample
  {JclOtaConsts, JclOtaResources,} JclSysUtils;

const
  MaxThreadCount       = 256;
  IdeEnterMutexTimeout = 5000;
  MutexName            = 'DebugThreadNamesMutex';
  MutexReadName        = 'DebugThreadNamesReadMutex';
  MappingName          = 'DebugThreadNamesMapping';
  EventName            = 'DebugThreadNamesEvent';

resourcestring
  RsEnterMutexTimeout = 'JCL Thread Name IDE Expert Mutex Timeout';

type
  TThreadName = record
    ThreadID: DWORD;
    ProcessID: DWORD;
    ThreadName: ShortString;
  end;

  PThreadNames = ^TThreadNames;
  TThreadNames = record
    Count: Integer;
    Threads: array [0..MaxThreadCount - 1] of TThreadName;
  end;

procedure SetIdeDebuggerThreadName(ThreadID: DWORD; const ThreadName: string);
type
  TThreadNameInfo = record
    FType: Longword;     // must be 0x1000
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: Longword; // thread ID (-1 indicates caller thread)
    FFlags: Longword;    // reserved for future use, must be zero
  end;
var
  ThreadNameInfo: TThreadNameInfo;
begin
  ThreadNameInfo.FType := $1000;
  ThreadNameInfo.FName := PChar(ThreadName);
  ThreadNameInfo.FThreadID := ThreadID;
  ThreadNameInfo.FFlags := 0;
  try
    RaiseException($406D1388, 0, SizeOf(ThreadNameInfo) div SizeOf(Longword), @ThreadNameInfo);
  except
  end;
end;

//=== { TSharedThreadNames } =================================================

constructor TSharedThreadNames.Create(IdeMode: Boolean);
begin
  inherited Create;
  FIdeMode := IdeMode;
  FMutex := TJclMutex.Create(nil, False, MutexName);
  FReadMutex := TJclMutex.Create(nil, False, MutexReadName);
  FMapping := TJclSwapFileMapping.Create(MappingName, PAGE_READWRITE, SizeOf(TThreadNames), nil);
  FView := TJclFileMappingView.Create(FMapping, FILE_MAP_ALL_ACCESS, 0, 0);
  FNotifyEvent := TJclEvent.Create(nil, False, False, EventName);
  FProcessID := GetCurrentProcessId;
end;

destructor TSharedThreadNames.Destroy;
begin
  Cleanup(FProcessID);
  FreeAndNil(FMapping);
  FreeAndNil(FMutex);
  FreeAndNil(FReadMutex);
  FreeAndNil(FNotifyEvent);
  inherited Destroy;
end;

procedure TSharedThreadNames.Cleanup(ProcessID: DWORD);
var
  I: Integer;
begin
  if EnterMutex then
  try
    with PThreadNames(FView.Memory)^ do
      for I := Low(Threads) to High(Threads) do
        with Threads[I] do
          if ProcessID = ProcessID then
          begin
            FReadMutex.WaitForever;
            try
              ProcessID := 0;
              ThreadID := 0;
              ThreadName := '';
            finally
              FReadMutex.Release;
            end;    
          end;
  finally
    FMutex.Release;
  end;
end;

function TSharedThreadNames.EnterMutex: Boolean;
begin
  if FIdeMode then
  begin
    case FMutex.WaitFor(IdeEnterMutexTimeout) of
      wrSignaled:
        Result := True;
      wrTimeout:
        raise EJclError.Create(RsEnterMutexTimeout);
    else
      Result := False;
    end;
  end
  else
  begin
    Sleep(0); // Prevent random deadlocks with IDE
    Result := FMutex.WaitForever = wrSignaled;
  end;
end;

class function TSharedThreadNames.Exists: Boolean;
{$IFDEF DELPHI7_UP}
begin
  Result := True;
end;  
{$ELSE DELPHI7_UP}
var
  H: THandle;
begin
  H := OpenMutex(MUTEX_ALL_ACCESS, False, PChar(MutexName));
  Result := (H <> 0);
  if Result then
    CloseHandle(H);
end;
{$ENDIF DELPHI7_UP}

function TSharedThreadNames.GetThreadName(ThreadID: DWORD): string;
var
  I: Integer;
begin
  Result := '';
  if FReadMutex.WaitForever = wrSignaled then
  try
    with PThreadNames(FView.Memory)^ do
      for I := Low(Threads) to High(Threads) do
        if Threads[I].ThreadID = ThreadID then
        begin
          Result := string(Threads[I].ThreadName);
          Break;
        end;
  finally
    FReadMutex.Release;
  end;
end;

procedure TSharedThreadNames.InternalRegisterThread(ThreadID: DWORD; const ThreadName: string; UpdateOnly: Boolean);
var
  I, Slot: Integer;
  NeedNotify: Boolean;
begin
  if EnterMutex then
  try
    Slot := -1;
    NeedNotify := ThreadID = MainThreadID;
    with PThreadNames(FView.Memory)^ do
    begin
      for I := Low(Threads) to High(Threads) do
        if Threads[I].ThreadID = ThreadID then
        begin
          Slot := I;
          NeedNotify := True;
          Break;
        end
        else
        if (not UpdateOnly) and (Slot = -1) and (Threads[I].ThreadID = 0) then
          Slot := I;
      if Slot <> -1 then
      begin
        FReadMutex.WaitForever;
        try
          Threads[Slot].ProcessID := FProcessID;
          Threads[Slot].ThreadID := ThreadID;
          Threads[Slot].ThreadName := ShortString(ThreadName);
        finally
          FReadMutex.Release;
        end;
      end;
    end;
    {$IFDEF DELPHI7_UP}
    SetIdeDebuggerThreadName(ThreadID, ThreadName);
    {$ENDIF DELPHI7_UP}
    if NeedNotify then
      FNotifyEvent.SetEvent;
  finally
    FMutex.Release;
  end;
end;

procedure TSharedThreadNames.RegisterThread(ThreadID: DWORD; const ThreadName: string);
begin
  InternalRegisterThread(ThreadID, ThreadName, False);
end;

procedure TSharedThreadNames.SetThreadName(ThreadID: DWORD; const Value: string);
begin
  InternalRegisterThread(ThreadID, Value, True);
end;

function TSharedThreadNames.ThreadNameTimoeut(ThreadID, Timeout: DWORD; var ThreadName: string): Boolean;
var
  I: Integer;
begin
  Result := FReadMutex.WaitFor(Timeout) = wrSignaled;
  if Result then
    try
      with PThreadNames(FView.Memory)^ do
        for I := Low(Threads) to High(Threads) do
          if Threads[I].ThreadID = ThreadID then
          begin
            ThreadName := string(Threads[I].ThreadName);
            Break;
          end;
    finally
      FReadMutex.Release;
    end;
end;

procedure TSharedThreadNames.UnregisterThread(ThreadID: DWORD);
var
  I: Integer;
begin
  EnterMutex;
  try
    with PThreadNames(FView.Memory)^ do
      for I := Low(Threads) to High(Threads) do
        if Threads[I].ThreadID = ThreadID then
        begin
          FReadMutex.WaitForever;
          try
            Threads[I].ProcessID := 0;
            Threads[I].ThreadID := 0;
            Threads[I].ThreadName := '';
          finally
            FReadMutex.Release;
          end;
          Break;
        end;
  finally
    FMutex.Release;
  end;
end;

procedure TSharedThreadNames.UpdateResumeStatus;
var
  I: Integer;
begin
  EnterMutex;
  try
    with PThreadNames(FView.Memory)^ do
      for I := Low(Threads) to High(Threads) do
        if Threads[I].ThreadID <> 0 then
        begin
          FReadMutex.WaitForever;
          try
            SetIdeDebuggerThreadName(Threads[I].ThreadID, string(Threads[I].ThreadName));
          finally
            FReadMutex.Release;
          end;
        end;
  finally
    FMutex.Release;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
