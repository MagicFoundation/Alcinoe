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
{ The Original Code is ThreadExpertUnit.pas.                                                       }
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
unit ThreadExpertUnit;

{$I jcl.inc}

interface

uses
  Windows, Classes, SysUtils, ToolsAPI, ComCtrls, Dialogs,
  ThreadExpertSharedNames,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclOtaUtils, JclSynch;

type
  TNameChangeThread = class;

  TJclThreadsExpert = class(TJclOTAExpert)
  private
    DebuggerServices: IOTADebuggerServices;
    FProcessesCount: Integer;
    FNameChangeThread: TNameChangeThread;
    FNotifierIndex: Integer;
    FSharedThreadNames: TSharedThreadNames;
    FThreadsStatusListView: TListView;
    function GetThreadsStatusListView: TListView;
    function GetThreadsStatusListViewFound: Boolean;
    procedure ListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    function UpdateItem(Item: TListItem): Boolean;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure UpdateContent;
    property ProcessesCount: Integer read FProcessesCount;
    property ThreadsStatusListView: TListView read GetThreadsStatusListView;
    property ThreadsStatusListViewFound: Boolean read GetThreadsStatusListViewFound;
  end;

  TDebuggerNotifier = class(TNotifierObject, IOTADebuggerNotifier)
  private
    FExpert: TJclThreadsExpert;
  protected
    procedure BreakpointAdded({$IFDEF RTL170_UP} const {$ENDIF} Breakpoint: IOTABreakpoint);
    procedure BreakpointDeleted({$IFDEF RTL170_UP} const {$ENDIF} Breakpoint: IOTABreakpoint);
    procedure ProcessCreated({$IFDEF RTL170_UP} const {$ENDIF} Process: IOTAProcess);
    procedure ProcessDestroyed({$IFDEF RTL170_UP} const {$ENDIF} Process: IOTAProcess);
  public
    constructor Create(AExpert: TJclThreadsExpert);
  end;

  TNameChangeThread = class(TThread)
  private
    FExpert: TJclThreadsExpert;
    FNotifyEvent: TJclEvent;
    FTerminateEvent: THandle;
    procedure TryFindThreadsStatusListView;
    procedure UpdateRequest;
  protected
    procedure Execute; override;
  public
    constructor Create(AExpert: TJclThreadsExpert; ANotifyEvent: TJclEvent);
    destructor Destroy; override;
    procedure TerminateThread;
  end;

// design package entry point
procedure Register;

// expert DLL entry point
function JCLWizardInit(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var TerminateProc: TWizardTerminateProc): Boolean; stdcall;

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
  Forms, Controls,
  JclSysUtils,
  JclOtaConsts, JclOtaResources;

const
  ThreadsStatusListViewFindPeriod = 2000;
  ReadNameTimeout                 = 500;

procedure Register;
begin
  try
    RegisterPackageWizard(TJclThreadsExpert.Create);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

var
  JCLWizardIndex: Integer = -1;

procedure JclWizardTerminate;
begin
  try
    if JCLWizardIndex <> -1 then
      TJclOTAExpertBase.GetOTAWizardServices.RemoveWizard(JCLWizardIndex);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

function JCLWizardInit(const BorlandIDEServices: IBorlandIDEServices;
    RegisterProc: TWizardRegisterProc;
    var TerminateProc: TWizardTerminateProc): Boolean stdcall;
begin
  try
    TerminateProc := JclWizardTerminate;

    JCLWizardIndex := TJclOTAExpertBase.GetOTAWizardServices.AddWizard(TJclThreadsExpert.Create);

    Result := True;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      Result := False;
    end;
  end;
end;

//== { TJclThreadsExpert } ===================================================

constructor TJclThreadsExpert.Create;
begin
  inherited Create(JclThreadsExpertName);
  DebuggerServices := BorlandIDEServices as IOTADebuggerServices;
  FSharedThreadNames := TSharedThreadNames.Create(True);
  FNotifierIndex := DebuggerServices.AddNotifier(TDebuggerNotifier.Create(Self));
  FNameChangeThread := TNameChangeThread.Create(Self, FSharedThreadNames.NotifyEvent);
end;

destructor TJclThreadsExpert.Destroy;
begin
  if FNotifierIndex <> -1 then
    DebuggerServices.RemoveNotifier(FNotifierIndex);
  if Assigned(FThreadsStatusListView) then
    FThreadsStatusListView.OnChange := nil;
  FNameChangeThread.TerminateThread;
  FreeAndNil(FNameChangeThread);
  FreeAndNil(FSharedThreadNames);
  inherited Destroy;
end;

function TJclThreadsExpert.GetThreadsStatusListView: TListView;
var
  I: Integer;
  F: TForm;
begin
  if FThreadsStatusListView = nil then
  begin
    F := nil;
    with Screen do
      for I := 0 to FormCount - 1 do
        if Forms[I].ClassName = 'TThreadStatus' then
        begin
          F := Forms[I];
          Break;
        end;
    if F <> nil then
      with F do
        for I := 0 to ControlCount -1 do
          if Controls[I] is TListView then
          begin
            FThreadsStatusListView := TListView(Controls[I]);
            Break;
          end;
    if FThreadsStatusListView <> nil then
      FThreadsStatusListView.OnChange := ListViewChange;
  end;
  Result := FThreadsStatusListView;
end;

function TJclThreadsExpert.GetThreadsStatusListViewFound: Boolean;
begin
  Result := Assigned(FThreadsStatusListView);
end;

procedure TJclThreadsExpert.ListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  try
    if Change = ctText then
      UpdateItem(Item);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TJclThreadsExpert.UpdateContent;
var
  I: Integer;
begin
  try
    with ThreadsStatusListView do
    begin
      {Items.BeginUpdate;
      try}
        for I := 0 to Items.Count - 1 do
          if not UpdateItem(Items[I]) then
            Break;
      {finally
        Items.EndUpdate;
      end;}
    end;          
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

var
  CaptionChanging: Boolean;

function TJclThreadsExpert.UpdateItem(Item: TListItem): Boolean;
var
  TID: DWORD;
  Caption, ThreadName: string;
begin
  Result := True;
  if CaptionChanging then
    Exit;
  Caption := Item.Caption;
  if (Length(Caption) >= 9) and (Caption[1] = '$') then
  begin
    Caption := Copy(Caption, 1, 9);
    TID := StrToInt(Caption);
    Result := FSharedThreadNames.ThreadNameTimoeut(TID, ReadNameTimeout, ThreadName);
    if Result then
    begin
      CaptionChanging := True;
      try
        Item.Caption := Format('%s  %s', [Caption, ThreadName]);
      finally
        CaptionChanging := False;
      end;
    end;
  end;
end;

//=== { TDebuggerNotifier } ==================================================

constructor TDebuggerNotifier.Create(AExpert: TJclThreadsExpert);
begin
  FExpert := AExpert;
end;

procedure TDebuggerNotifier.BreakpointAdded({$IFDEF RTL170_UP} const {$ENDIF} Breakpoint: IOTABreakpoint);
begin
end;

procedure TDebuggerNotifier.BreakpointDeleted({$IFDEF RTL170_UP} const {$ENDIF} Breakpoint: IOTABreakpoint);
begin
end;

procedure TDebuggerNotifier.ProcessCreated({$IFDEF RTL170_UP} const {$ENDIF} Process: IOTAProcess);
begin
  try
    FExpert.GetThreadsStatusListView;
    Inc(FExpert.FProcessesCount);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TDebuggerNotifier.ProcessDestroyed({$IFDEF RTL170_UP} const {$ENDIF} Process: IOTAProcess);
begin
  try
    Dec(FExpert.FProcessesCount);
    FExpert.FSharedThreadNames.Cleanup(Process.ProcessId);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

//=== { TNameChangeThread } ==================================================

constructor TNameChangeThread.Create(AExpert: TJclThreadsExpert; ANotifyEvent: TJclEvent);
begin
  inherited Create(True);
  Priority := tpLowest;
  FExpert := AExpert;
  FNotifyEvent := ANotifyEvent;
  FTerminateEvent := CreateEvent(nil, True, False, nil);
  Resume;
end;

destructor TNameChangeThread.Destroy;
begin
  CloseHandle(FTerminateEvent);
  inherited Destroy;
end;

procedure TNameChangeThread.Execute;
var
  WaitHandles: array [0..1] of THandle;
  WaitTimeout: DWORD;
begin
  WaitHandles[0] := FTerminateEvent;
  WaitHandles[1] := FNotifyEvent.Handle;
  WaitTimeout := ThreadsStatusListViewFindPeriod;
  repeat
    case Windows.WaitForMultipleObjects(2, @WaitHandles, False, WaitTimeout) of
      WAIT_OBJECT_0:
        Break;
      WAIT_OBJECT_0 + 1:
        begin
          Synchronize(UpdateRequest);
          Sleep(30); // To prevent overload the IDE by many update requests
        end;
      WAIT_TIMEOUT:
        if FExpert.ProcessesCount > 0 then
        begin
          if not FExpert.ThreadsStatusListViewFound then
            Synchronize(TryFindThreadsStatusListView);
          if FExpert.ThreadsStatusListViewFound then
            WaitTimeout := INFINITE;
        end;
    end;
  until Terminated;
end;

procedure TNameChangeThread.TerminateThread;
begin
  Terminate;
  SetEvent(FTerminateEvent);
  WaitFor;
end;

procedure TNameChangeThread.TryFindThreadsStatusListView;
begin
  if FExpert.GetThreadsStatusListView <> nil then
    FExpert.UpdateContent;
end;

procedure TNameChangeThread.UpdateRequest;
begin
  FExpert.UpdateContent;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
