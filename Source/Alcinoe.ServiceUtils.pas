unit Alcinoe.ServiceUtils;

interface

{$I Alcinoe.inc}

uses
  System.SysUtils,
  System.SyncObjs;

procedure ALInstallService(
            const AServiceName: string;
            const ADisplayName: string;
            const ADescription: string;
            const ACommandLine: string;
            const ADependencies: TArray<String>;
            const AAccountName: String;
            const APassword: String;
            const APreshutdownTimeout: integer = 0);
procedure ALUninstallService(const AServiceName: string);
Procedure ALStartService(const AServiceName: String);
procedure ALStopService(const AServiceName: String);
procedure ALStartServiceCtrlDispatcher(const AServiceName: String; const AServiceProc: TProcedure);

var
  ALStopServiceEvent: TEvent = nil;


implementation

uses
  Winapi.Windows,
  Winapi.WinSvc,
  Alcinoe.WinApi.Windows,
  Alcinoe.Common;

{*************************}
procedure ALInstallService(
            const AServiceName: string;
            const ADisplayName: string;
            const ADescription: string;
            const ACommandLine: string;
            const ADependencies: TArray<String>;
            const AAccountName: String;
            const APassword: String;
            const APreshutdownTimeout: integer = 0);
begin
  var LSCManager := ALCheckWinApiHandle(
                      'OpenSCManagerW',
                      OpenSCManagerW(
                        nil, // LPCWSTR lpMachineName,
                        nil, // LPCWSTR lpDatabaseName,
                        SC_MANAGER_ALL_ACCESS)); // DWORD   dwDesiredAccess
  try

    var LDependenciesMultiSz: string := '';
    for var I := low(ADependencies) to high(ADependencies) do
      LDependenciesMultiSz := LDependenciesMultiSz + ADependencies[I] + #0;
    if LDependenciesMultiSz <> '' then
      LDependenciesMultiSz := LDependenciesMultiSz + #0; // extra null terminator

    var LService := ALCheckWinApiHandle(
                      'CreateServiceW',
                      CreateServiceW(
                        LSCManager, // SC_HANDLE hSCManager,
                        PWideChar(AServiceName), // LPCWSTR   lpServiceName,
                        if ADisplayName <> '' then PWideChar(ADisplayName) else nil, // LPCWSTR   lpDisplayName,
                        SERVICE_ALL_ACCESS, // DWORD     dwDesiredAccess,
                        SERVICE_WIN32_OWN_PROCESS, // DWORD     dwServiceType,
                        SERVICE_AUTO_START, // DWORD     dwStartType,
                        SERVICE_ERROR_NORMAL, // DWORD     dwErrorControl,
                        PWideChar(ACommandLine), // LPCWSTR   lpBinaryPathName,
                        nil,  // LPCWSTR   lpLoadOrderGroup,
                        nil,  // LPDWORD   lpdwTagId,
                        if LDependenciesMultiSz <> '' then PWideChar(LDependenciesMultiSz) else nil, // LPCWSTR   lpDependencies,
                        if AAccountName <> '' then PWideChar(AAccountName) else nil, // LPCWSTR   lpServiceStartName,
                        if APassword <> '' then PWideChar(APassword) else nil)); // LPCWSTR   lpPassword
    try

      if ADescription <> '' then begin
        var LServiceDescription: SERVICE_DESCRIPTIONW;
        LServiceDescription.lpDescription := PWideChar(ADescription);
        ALCheckWinApiBoolean(
          'ChangeServiceConfig2W',
          ChangeServiceConfig2W(
            LService,
            SERVICE_CONFIG_DESCRIPTION,
            @LServiceDescription));
      end;

      if APreshutdownTimeout > 0 then begin
        var lServicePreshutdownInfo: SERVICE_PRESHUTDOWN_INFO;
        lServicePreshutdownInfo.dwPreshutdownTimeout := APreshutdownTimeout;
        ALCheckWinApiBoolean(
          'ChangeServiceConfig2W',
          ChangeServiceConfig2W(
            LService,
            SERVICE_CONFIG_PRESHUTDOWN_INFO,
            @lServicePreshutdownInfo));
      end;

    finally
      CloseServiceHandle(LService);
    end;

  finally
    CloseServiceHandle(LSCManager);
  end;
end;

{*******************************************************}
procedure ALUninstallService(const AServiceName: string);
begin
  var LSCManager := ALCheckWinApiHandle(
                      'OpenSCManagerW',
                      OpenSCManagerW(
                        nil, // LPCWSTR lpMachineName,
                        nil, // LPCWSTR lpDatabaseName,
                        SC_MANAGER_ALL_ACCESS)); // DWORD   dwDesiredAccess
  try

    var LService := ALCheckWinApiHandle(
                      'OpenServiceW',
                      OpenServiceW(
                        LSCManager, // SC_HANDLE hSCManager,
                        PWideChar(AServiceName), // LPCWSTR   lpServiceName,
                        SERVICE_ALL_ACCESS)); // DWORD     dwDesiredAccess
    try
      ALCheckWinApiBoolean(
        'DeleteService',
        DeleteService(LService));
    finally
      CloseServiceHandle(LService);
    end;

  finally
    CloseServiceHandle(LSCManager);
  end;
end;

{***************************************************}
procedure ALStartService(const AServiceName: String);
begin
  var LSCManager := ALCheckWinApiHandle(
                      'OpenSCManagerW',
                      OpenSCManagerW(
                        nil, // LPCWSTR lpMachineName,
                        nil, // LPCWSTR lpDatabaseName,
                        SC_MANAGER_ALL_ACCESS)); // DWORD   dwDesiredAccess
  try

    var LService := ALCheckWinApiHandle(
                      'OpenServiceW',
                      OpenServiceW(
                        LSCManager, // SC_HANDLE hSCManager,
                        PWideChar(AServiceName), // LPCWSTR   lpServiceName,
                        SERVICE_ALL_ACCESS)); // DWORD     dwDesiredAccess
    try

      var LServiceArgVectors: LPCWSTR := nil;
      StartServiceW(
        LService, // SC_HANDLE hService,
        0, // DWORD     dwNumServiceArgs,
        LServiceArgVectors);  // LPCWSTR   *lpServiceArgVectors

      var LServiceStatus: TServiceStatus;
      LServiceStatus.dwCurrentState := SERVICE_START_PENDING;
      while LServiceStatus.dwCurrentState = SERVICE_START_PENDING do begin
        Sleep(100);
        ALCheckWinApiBoolean(
          'QueryServiceStatus',
          QueryServiceStatus(
            LService, // SC_HANDLE        hService,
            LServiceStatus)); // LPSERVICE_STATUS lpServiceStatus
      end;

      if not (LServiceStatus.dwCurrentState = SERVICE_RUNNING) then
        Raise Exception.CreateFmt(
                'Failed to start service "%s" (current state: %d).',
                [AServiceName, LServiceStatus.dwCurrentState]);

    finally
      CloseServiceHandle(LService);
    end;

  finally
    CloseServiceHandle(LSCManager);
  end;
end;

{**************************************************}
procedure ALStopService(const AServiceName: String);
begin
  var LSCManager := ALCheckWinApiHandle(
                      'OpenSCManagerW',
                      OpenSCManagerW(
                        nil, // LPCWSTR lpMachineName,
                        nil, // LPCWSTR lpDatabaseName,
                        SC_MANAGER_ALL_ACCESS)); // DWORD   dwDesiredAccess
  try

    var LService := ALCheckWinApiHandle(
                      'OpenServiceW',
                      OpenServiceW(
                        LSCManager, // SC_HANDLE hSCManager,
                        PWideChar(AServiceName), // LPCWSTR   lpServiceName,
                        SERVICE_ALL_ACCESS)); // DWORD     dwDesiredAccess
    try

      var LServiceStatus: TServiceStatus;
      ControlService(
        LService, // SC_HANDLE        hService,
        SERVICE_CONTROL_STOP, // DWORD            dwControl,
        LServiceStatus); // LPSERVICE_STATUS lpServiceStatus

      while LServiceStatus.dwCurrentState = SERVICE_STOP_PENDING do begin
        Sleep(100);
        ALCheckWinApiBoolean(
          'QueryServiceStatus',
          QueryServiceStatus(
            LService, // SC_HANDLE        hService,
            LServiceStatus)); // LPSERVICE_STATUS lpServiceStatus
      end;

      if not (LServiceStatus.dwCurrentState = SERVICE_STOPPED) then
        Raise Exception.CreateFmt(
                'Failed to stop service "%s" (current state: %d).',
                [AServiceName, LServiceStatus.dwCurrentState]);

    finally
      CloseServiceHandle(LService);
    end;

  finally
    CloseServiceHandle(LSCManager);
  end;
end;

var
  _ServiceName: string = '';
  _ServiceProc: TProcedure = nil;
  _ServiceStatusHandle: SERVICE_STATUS_HANDLE = 0;

{**********************************************************************************************}
procedure _SetServiceStatus(const ACurrentState: DWORD; const AWin32ExitCode: DWORD = NO_ERROR);
begin
  if _ServiceStatusHandle = 0 then Exit;
  var LServiceStatus: SERVICE_STATUS;
  LServiceStatus.dwServiceType := SERVICE_WIN32_OWN_PROCESS;
  LServiceStatus.dwCurrentState := ACurrentState;
  if ACurrentState = SERVICE_RUNNING then
    LServiceStatus.dwControlsAccepted := SERVICE_ACCEPT_STOP or SERVICE_ACCEPT_PRESHUTDOWN or SERVICE_ACCEPT_SHUTDOWN
  else
    LServiceStatus.dwControlsAccepted := 0;
  LServiceStatus.dwWin32ExitCode := AWin32ExitCode;
  LServiceStatus.dwServiceSpecificExitCode := 0;
  LServiceStatus.dwCheckPoint := 0;
  LServiceStatus.dwWaitHint := 0;
  ALCheckWinApiBoolean(
    'SetServiceStatus',
    SetServiceStatus(
      _ServiceStatusHandle, // SERVICE_STATUS_HANDLE hServiceStatus,
      LServiceStatus)); // LPSERVICE_STATUS      lpServiceStatus
end;

{***************************************************************************************************************************}
function _ServiceCtrlHandlerEx(dwControl: DWORD; dwEventType: DWORD; lpEventData: LPVOID; lpContext: LPVOID): DWORD; stdcall;
begin
  case dwControl of

    // SERVICE_CONTROL_INTERROGATE
    SERVICE_CONTROL_INTERROGATE:
      Result := NO_ERROR;

    // SERVICE_CONTROL_STOP
    // SERVICE_CONTROL_SHUTDOWN
    SERVICE_CONTROL_STOP,
    SERVICE_CONTROL_PRESHUTDOWN,
    SERVICE_CONTROL_SHUTDOWN: begin
      _SetServiceStatus(SERVICE_STOP_PENDING);
      if ALStopServiceEvent <> nil then ALStopServiceEvent.SetEvent;
      Result := NO_ERROR;
    end;

    // NOT_IMPLEMENTED
    else
      Result := ERROR_CALL_NOT_IMPLEMENTED;

  end;
end;

{**************************************************************************************}
procedure _ServiceMain(dwNumServicesArgs: DWORD; lpServiceArgVectors: PLPWSTR); stdcall;
begin
  try
    ALStopServiceEvent := TEvent.Create(nil{EventAttributes}, false{ManualReset}, false{InitialState}, ''{Name});
    try
      _ServiceStatusHandle := ALCheckWinApiHandle(
                                'RegisterServiceCtrlHandlerExW',
                                RegisterServiceCtrlHandlerExW(
                                  PWideChar(_ServiceName), // LPCWSTR               lpServiceName,
                                  @_ServiceCtrlHandlerEx, // LPHANDLER_FUNCTION_EX lpHandlerProc,
                                  nil)); // LPVOID                lpContext
      _SetServiceStatus(SERVICE_START_PENDING);
      _SetServiceStatus(SERVICE_RUNNING);
      _ServiceProc();
      _SetServiceStatus(SERVICE_STOPPED);
    finally
      ALFreeAndNil(ALStopServiceEvent);
    end;
  Except
    on E: Exception do begin
      ALLog('Alcinoe.ServiceUtils._ServiceMain', E);
      _SetServiceStatus(SERVICE_STOPPED, ERROR_EXCEPTION_IN_SERVICE);
    end;
  end;
end;

{*************************************************************************************************}
procedure ALStartServiceCtrlDispatcher(const AServiceName: String; const AServiceProc: TProcedure);
begin
  if _ServiceStatusHandle <> 0 then
    raise Exception.Create('Service dispatcher already started');
  _ServiceName := AServiceName;
  _ServiceProc := AServiceProc;
  var LServiceTableEntries: array[0..1] of TServiceTableEntryW;
  LServiceTableEntries[0].lpServiceName := PWideChar(_ServiceName);
  LServiceTableEntries[0].lpServiceProc := @_ServiceMain;
  LServiceTableEntries[1].lpServiceName := nil;
  LServiceTableEntries[1].lpServiceProc := nil;
  ALCheckWinApiBoolean(
    'StartServiceCtrlDispatcherW',
    StartServiceCtrlDispatcherW(@LServiceTableEntries[0]));
end;

end.