(*
    "The contents of this file are subject to the Mozilla Public License
    Version 1.1 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at
    http://www.mozilla.org/MPL/

    Software distributed under the License is distributed on an "AS IS"
    basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
    License for the specific language governing rights and limitations
    under the License.

    The Initial Developer of the Original Code is
      Henri Gourvest <hgourvest@progdigy.com>.
*)
{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$IFNDEF CPU64}
{$ALIGN ON}
{$MINENUMSIZE 4}
{$ENDIF}

unit PDGService;
{$I PDGAppServer.inc}
interface
uses
{$IFDEF FPC}sockets{$ELSE}WinSock{$ENDIF},
{$if Defined(MadExcept) and Defined(CONSOLEAPP)}
  MadExcept,
{$ifend}
  SysUtils,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  PDGSocketStub;

type
  TPDGService = class
  private
    FThreads: TPDGThread;
    FName: string;
    FDisplayName: string;
{$IFNDEF CONSOLEAPP}
    FDependencies: string;
    FServiceType: Cardinal;
    FStartType: Cardinal;
{$ENDIF}
    function Start: boolean;
    procedure Pause;
    procedure Resume;
  protected
{$IFNDEF CONSOLEAPP}
    procedure InstallService;
    procedure RemoveService;
{$ENDIF}
  public
    procedure Run;
    function CreateServer(clazz: TSocketServerClass; Port: Word; const Bind: Longint = INADDR_ANY): TSocketServer;
    function CreateThread(clazz: TPDGThreadClass): TPDGThread;
    constructor Create; virtual;
    destructor Destroy; override;
    property Name: string read FName write FName;
    property DisplayName: string read FDisplayName write FDisplayName;
{$IFNDEF CONSOLEAPP}
    property Dependencies: string read FDependencies write FDependencies;
    property ServiceType: Cardinal read FServiceType write FServiceType;
    property StartType: Cardinal read FStartType write FStartType;
{$ENDIF}
  end;

var
  Application: TPDGService;

{$IFNDEF CONSOLEAPP}
const
  SERVICE_WIN32_OWN_PROCESS     = $00000010;
  SERVICE_WIN32_SHARE_PROCESS   = $00000020;
  SERVICE_INTERACTIVE_PROCESS   = $00000100;
  SERVICE_WIN32                 = (SERVICE_WIN32_OWN_PROCESS or
                                   SERVICE_WIN32_SHARE_PROCESS);

  SERVICE_BOOT_START            = $00000000;
  SERVICE_SYSTEM_START          = $00000001;
  SERVICE_AUTO_START            = $00000002;
  SERVICE_DEMAND_START          = $00000003;
  SERVICE_DISABLED              = $00000004;
{$ENDIF}

implementation

{$IFNDEF CONSOLEAPP}
type
  PServiceStatus = ^TServiceStatus;
  TServiceStatus = record
    dwServiceType: DWORD;
    dwCurrentState: DWORD;
    dwControlsAccepted: DWORD;
    dwWin32ExitCode: DWORD;
    dwServiceSpecificExitCode: DWORD;
    dwCheckPoint: DWORD;
    dwWaitHint: DWORD;
  end;

  TServiceTableEntry = record
    lpServiceName: PAnsiChar;
    lpServiceProc: TFarProc;
  end;

  SERVICE_STATUS_HANDLE = DWORD;

var
  ServiceStatus: TServiceStatus;
  ServiceStatusHandle: THandle;

const
  SERVICE_CONTROL_STOP           = $00000001;
  SERVICE_CONTROL_PAUSE          = $00000002;
  SERVICE_CONTROL_CONTINUE       = $00000003;
  SERVICE_CONTROL_INTERROGATE    = $00000004;
  SERVICE_CONTROL_SHUTDOWN       = $00000005;

  SERVICE_STOPPED                = $00000001;
  SERVICE_START_PENDING          = $00000002;
  SERVICE_STOP_PENDING           = $00000003;
  SERVICE_RUNNING                = $00000004;
  SERVICE_CONTINUE_PENDING       = $00000005;
  SERVICE_PAUSE_PENDING          = $00000006;
  SERVICE_PAUSED                 = $00000007;

  SERVICE_ACCEPT_STOP            = $00000001;
  SERVICE_ACCEPT_PAUSE_CONTINUE  = $00000002;
  SERVICE_ACCEPT_SHUTDOWN        = $00000004;

  SC_MANAGER_CONNECT             = $0001;
  SC_MANAGER_CREATE_SERVICE      = $0002;
  SC_MANAGER_ENUMERATE_SERVICE   = $0004;
  SC_MANAGER_LOCK                = $0008;
  SC_MANAGER_QUERY_LOCK_STATUS   = $0010;
  SC_MANAGER_MODIFY_BOOT_CONFIG  = $0020;

  SERVICE_QUERY_CONFIG           = $0001;
  SERVICE_CHANGE_CONFIG          = $0002;
  SERVICE_QUERY_STATUS           = $0004;
  SERVICE_ENUMERATE_DEPENDENTS   = $0008;
  SERVICE_START                  = $0010;
  SERVICE_STOP                   = $0020;
  SERVICE_PAUSE_CONTINUE         = $0040;
  SERVICE_INTERROGATE            = $0080;
  SERVICE_USER_DEFINED_CONTROL   = $0100;

  SERVICE_ERROR_IGNORE          = $00000000;
  SERVICE_ERROR_NORMAL          = $00000001;
  SERVICE_ERROR_SEVERE          = $00000002;
  SERVICE_ERROR_CRITICAL        = $00000003;

  _DELETE                  = $00010000;

  advapi32 = 'advapi32.dll';

function SetServiceStatus(hServiceStatus: SERVICE_STATUS_HANDLE;
  var lpServiceStatus: TServiceStatus): BOOL; stdcall;
  external advapi32 name 'SetServiceStatus';

function RegisterServiceCtrlHandler(lpServiceName: PChar;
  lpHandlerProc: TFarProc): SERVICE_STATUS_HANDLE; stdcall;
  external advapi32 name {$IFDEF UNICODE}'RegisterServiceCtrlHandlerW'{$ELSE}'RegisterServiceCtrlHandlerA'{$ENDIF};

function StartServiceCtrlDispatcher(
  var lpServiceStartTable: TServiceTableEntry): BOOL; stdcall;
  external advapi32 name {$IFDEF UNICODE}'StartServiceCtrlDispatcherW'{$ELSE}'StartServiceCtrlDispatcherA'{$ENDIF};

function OpenSCManager(lpMachineName, lpDatabaseName: PChar;
  dwDesiredAccess: DWORD): THandle; stdcall;
  external advapi32 name {$IFDEF UNICODE}'OpenSCManagerW'{$ELSE}'OpenSCManagerA'{$ENDIF};

function CreateService(hSCManager: THandle; lpServiceName, lpDisplayName: PChar;
  dwDesiredAccess, dwServiceType, dwStartType, dwErrorControl: DWORD;
  lpBinaryPathName, lpLoadOrderGroup: PChar; lpdwTagId: LPDWORD; lpDependencies,
  lpServiceStartName, lpPassword: PChar): THandle; stdcall;
  external advapi32 name {$IFDEF UNICODE}'CreateServiceW'{$ELSE}'CreateServiceA'{$ENDIF};

function CloseServiceHandle(hSCObject: THandle): BOOL; stdcall;
  external advapi32 name 'CloseServiceHandle';

function OpenService(hSCManager: THandle; lpServiceName: PChar;
  dwDesiredAccess: DWORD): THandle; stdcall;
  external advapi32 name {$IFDEF UNICODE}'OpenServiceW'{$ELSE}'OpenServiceA'{$ENDIF};

function ControlService(hService: THandle; dwControl: DWORD;
  var lpServiceStatus: TServiceStatus): BOOL; stdcall;
  external advapi32 name 'ControlService';

function QueryServiceStatus(hService: THandle; var
  lpServiceStatus: TServiceStatus): BOOL; stdcall;
  external advapi32 name 'QueryServiceStatus';

function DeleteService(hService: THandle): BOOL; stdcall;
  external advapi32 name 'DeleteService';
{$ENDIF}

procedure Terminate;
begin
  if Application <> nil then
  begin
    Application.Free;
    Application := nil;
  end;
end;

{$IFNDEF CONSOLEAPP}
procedure ServiceCtrlHandler(Opcode: LongWord); stdcall;
begin
  case Opcode of
    SERVICE_CONTROL_PAUSE:
      begin
        // Do whatever it takes to pause here.
        ServiceStatus.dwCurrentState := SERVICE_PAUSED;
        Application.Pause;
      end;

    SERVICE_CONTROL_CONTINUE:
      begin
        // Do whatever it takes to continue here.
        ServiceStatus.dwCurrentState := SERVICE_RUNNING;
        Application.Resume;
      end;

    SERVICE_CONTROL_STOP:
      begin
        // Do whatever it takes to stop here.
        ServiceStatus.dwWin32ExitCode := 0;
        ServiceStatus.dwCurrentState  := SERVICE_STOPPED;
        ServiceStatus.dwCheckPoint    := 0;
        ServiceStatus.dwWaitHint      := 0;

        if (not SetServiceStatus(ServiceStatusHandle, ServiceStatus)) then
          RaiseLastOSError;
        Terminate;
        Exit;
      end;

    SERVICE_CONTROL_INTERROGATE:
    begin
      // Fall through to send current status.
    end;

  else
    raise Exception.CreateFmt('Unrecognized opcode %d', [Opcode]);
  end;

  // Send current status.
  if (not SetServiceStatus(ServiceStatusHandle, ServiceStatus)) then
    RaiseLastOSError;
end;
{$ENDIF}

{$IFNDEF CONSOLEAPP}
procedure ServiceMain(argc: LongWord; argv: PChar); stdcall;
var
  status: LongWord;
begin
  ServiceStatus.dwServiceType        := Application.FServiceType;
  ServiceStatus.dwCurrentState       := SERVICE_START_PENDING;
  ServiceStatus.dwControlsAccepted   := SERVICE_ACCEPT_STOP or SERVICE_ACCEPT_PAUSE_CONTINUE or SERVICE_ACCEPT_SHUTDOWN;
  ServiceStatus.dwWin32ExitCode      := 0;
  ServiceStatus.dwServiceSpecificExitCode := 0;
  ServiceStatus.dwCheckPoint         := 0;
  ServiceStatus.dwWaitHint           := 0;

  ServiceStatusHandle := RegisterServiceCtrlHandler(
      @Application.FName[1], @ServiceCtrlHandler);

  if (ServiceStatusHandle = 0) then
  begin
     raise Exception.CreateFmt('RegisterServiceCtrlHandler failed %d', [GetLastError]);
     Exit;
  end;

  ServiceStatus.dwCurrentState       := SERVICE_RUNNING;
  ServiceStatus.dwCheckPoint         := 0;
  ServiceStatus.dwWaitHint           := 0;

  if (not SetServiceStatus(ServiceStatusHandle, ServiceStatus)) then
  begin
    status := GetLastError;
    raise Exception.CreateFmt('SetServiceStatus error %d', [status]);
  end;

  // This is where the service does its work.
  if not Application.Start then
  begin
    ServiceStatus.dwCurrentState := SERVICE_STOPPED;
    ServiceStatus.dwWin32ExitCode := GetLastError;
    SetServiceStatus(ServiceStatusHandle, ServiceStatus);
  end;
end;
{$ENDIF}

{$IFNDEF CONSOLEAPP}
function StartService: boolean;
var
  DispatchTable: array[0..1] of TServiceTableEntry;
begin
  DispatchTable[0].lpServiceName := @Application.Name[1];
  DispatchTable[0].lpServiceProc := @ServiceMain;
  DispatchTable[1].lpServiceName := nil;
  DispatchTable[1].lpServiceProc := nil;
  Result := StartServiceCtrlDispatcher(DispatchTable[0]);
end;
{$ENDIF}

{ TPDGService }

{$IFNDEF CONSOLEAPP}
procedure TPDGService.InstallService;
var
  Service: THandle;
  SCManager: THandle;
  Path: array[0..511] of Char;
  Depend: PChar;
begin
   if (GetModuleFileName(0, Path, 512) = 0) then
   begin
     raise Exception.CreateFmt('Unable to install %s - %s',
       [FDisplayName, SysErrorMessage(GetLastError)]);
     Exit;
   end;
   SCManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT or SC_MANAGER_CREATE_SERVICE);
   if (SCManager <> 0) then
   begin
      if FDependencies <> '' then
        Depend := @FDependencies[1] else
        Depend := nil;
      Service := CreateService(SCManager, @FName[1], @FDisplayName[1],
        SERVICE_QUERY_STATUS, FServiceType, FStartType,
        SERVICE_ERROR_NORMAL, Path, nil, nil, depend, nil, nil);
      if (Service <> 0) then
        CloseServiceHandle(Service) else
        raise Exception.CreateFmt('CreateService failed - %s', [SysErrorMessage(GetLastError)]);
      CloseServiceHandle(SCManager);
   end
   else
     raise Exception.CreateFmt('OpenSCManager failed - %s', [SysErrorMessage(GetLastError)])
end;
{$ENDIF}

{$IFNDEF CONSOLEAPP}
procedure TPDGService.RemoveService;
var
  Service: THandle;
  SCManager: THandle;
  Status: TServiceStatus;
begin
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if (SCManager <> 0) then
  begin
     Service := OpenService(SCManager, @FName[1], _DELETE or SERVICE_STOP or
       SERVICE_QUERY_STATUS);
     if (Service <> 0) then
     begin
       if (ControlService(Service, SERVICE_CONTROL_STOP, Status)) then
       begin
         Sleep(1000);
         while (QueryServiceStatus(Service, Status)) do
           if (Status.dwCurrentState = SERVICE_STOP_PENDING) then
             Sleep(1000) else Break;
         if not (Status.dwCurrentState = SERVICE_STOPPED) then
           raise Exception.CreateFmt('%s failed to stop.', [FDisplayName]);
       end;
       if not DeleteService(Service) then
         raise Exception.CreateFmt('DeleteService failed - %s', [SysErrorMessage(GetLastError)]);
       CloseServiceHandle(Service);
     end
     else
       raise Exception.CreateFmt('OpenService failed - %s', [SysErrorMessage(GetLastError)]);
     CloseServiceHandle(SCManager);
  end
  else
    raise Exception.CreateFmt('OpenSCManager failed - %s', [SysErrorMessage(GetLastError)]);
end;
{$ENDIF}

procedure TPDGService.Run;
{$IFDEF CONSOLEAPP}
var
  Cmd: string;
  i: Integer;
label tryagain;
{$ENDIF}
begin
{$IFDEF CONSOLEAPP}
  Start;
  writeln('---------------------------------------');
  writeln(application.DisplayName);
  writeln('---------------------------------------');
  writeln('[COMMANDS]');
  writeln(' > EXIT   : Stop application.');
  writeln(' > PAUSE  : Pause application.');
  writeln(' > RESUME : Resume paused application.');
  writeln(' > CLEAR  : Disconnect all clients.');
{$ifdef MadExcept}
  writeln(' > RESTART: Restart application (madexcept).');
  writeln(' > REPORT : Generate a bug report (madexcept).');
{$endif}
  writeln('---------------------------------------');
tryagain:
  //write('> ');
  readln(cmd);
{$ifdef MadExcept}
  if comparetext(Cmd, 'restart') = 0 then RestartApplication else
  if comparetext(Cmd, 'report') = 0 then
  begin
    try
      raise Exception.Create('Thread Status');
    except
      on E: Exception do
        HandleException(etNormal, E);
    end;
  end else
{$endif}
  if comparetext(Cmd, 'exit') = 0 then
  begin
    Terminate;
    Exit;
  end else
  if comparetext(Cmd, 'pause') = 0 then Pause else
  if comparetext(Cmd, 'resume') = 0 then Resume else
  if comparetext(Cmd, 'clear') = 0 then
  begin
    FThreads.Lock;
    try
      FThreads.Resume;
      //sleep(100);
      for i := 0 to FThreads.ChildCount - 1 do
        if FThreads[i] is TSocketServer then
          FThreads[i].ChildClear;
    finally
      FThreads.UnLock;
    end;
  end else
    writeln('Unknow command');
  goto tryagain;
{$ELSE}
  if ParamCount > 0 then
  if CompareText(ParamStr(1), 'INSTALL') = 0 then
  begin
    InstallService;
    Terminate;
    Exit;
  end else
  if CompareText(ParamStr(1), 'UNINSTALL') = 0 then
  begin
    RemoveService;
    Terminate;
    Exit;
  end;
  if not StartService then
    Terminate;
{$ENDIF}
end;

constructor TPDGService.Create;
begin
  FThreads := TPDGThread.Create(nil);
{$IFNDEF CONSOLEAPP}
  FServiceType := SERVICE_WIN32_OWN_PROCESS;
  FStartType := SERVICE_DEMAND_START;
{$ENDIF}
end;

destructor TPDGService.Destroy;
begin
  FThreads.Free;
{$IFNDEF FPC}
  WSACleanup;
{$ENDIF}
  Application := nil;
  inherited;
end;

function TPDGService.Start: boolean;
{$IFNDEF FPC}
var
  Data: TWSAData;
{$ENDIF}
begin
{$IFNDEF FPC}
  WSAStartup($0202, Data);
{$ENDIF}
  Result := True;
  try
    FThreads.Start;
  except
    Result := False;
  end;
end;

procedure TPDGService.Pause;
begin
  FThreads.Pause;
end;

procedure TPDGService.Resume;
begin
  FThreads.Resume;
end;

function TPDGService.CreateServer(clazz: TSocketServerClass; Port: Word; const Bind: Longint): TSocketServer;
begin
  Result := clazz.CreateServer(FThreads, Port, Bind);
end;

function TPDGService.CreateThread(clazz: TPDGThreadClass): TPDGThread;
begin
  Result := clazz.Create(FThreads);
end;

initialization
  Application := TPDGService.Create;

end.
