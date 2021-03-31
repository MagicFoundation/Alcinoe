/// daemon managment classes for mORMot, including low-level Win NT Service
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotService;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Eric Grange
  - Leander007
  - Maciej Izak (hnb)

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

}

interface

{$I Synopse.inc}

uses
  {$ifdef MSWINDOWS}
  Windows,
  Messages,
  {$else}
  {$ifdef FPC}
  SynFPCLinux,
  BaseUnix,
  Unix,
  {$else}
  Types,
  LibC, // Kylix
  {$endif FPC}
  {$endif MSWINDOWS}
  Classes,
  SysUtils,
  {$ifndef LVCL}
  Contnrs,
  {$endif}
  SynCommons,
  SynTable,
  SynLog,
  SynCrypto, // for executable MD5/SHA256 hashes
  mORMot; // for TSynJsonFileSettings (i.e. JSON serialization)

{$ifdef MSWINDOWS}

{ *** some minimal Windows API definitions, replacing WinSvc.pas missing for FPC }

const
  CM_SERVICE_CONTROL_CODE = WM_USER+1000;

  SERVICE_QUERY_CONFIG         = $0001;
  SERVICE_CHANGE_CONFIG        = $0002;
  SERVICE_QUERY_STATUS         = $0004;
  SERVICE_ENUMERATE_DEPENDENTS = $0008;
  SERVICE_START                = $0010;
  SERVICE_STOP                 = $0020;
  SERVICE_PAUSE_CONTINUE       = $0040;
  SERVICE_INTERROGATE          = $0080;
  SERVICE_USER_DEFINED_CONTROL = $0100;
  SERVICE_ALL_ACCESS           = STANDARD_RIGHTS_REQUIRED or
                                 SERVICE_QUERY_CONFIG or
                                 SERVICE_CHANGE_CONFIG or
                                 SERVICE_QUERY_STATUS or
                                 SERVICE_ENUMERATE_DEPENDENTS or
                                 SERVICE_START or
                                 SERVICE_STOP or
                                 SERVICE_PAUSE_CONTINUE or
                                 SERVICE_INTERROGATE or
                                 SERVICE_USER_DEFINED_CONTROL;

  SC_MANAGER_CONNECT            = $0001;
  SC_MANAGER_CREATE_SERVICE     = $0002;
  SC_MANAGER_ENUMERATE_SERVICE  = $0004;
  SC_MANAGER_LOCK               = $0008;
  SC_MANAGER_QUERY_LOCK_STATUS  = $0010;
  SC_MANAGER_MODIFY_BOOT_CONFIG = $0020;
  SC_MANAGER_ALL_ACCESS         = STANDARD_RIGHTS_REQUIRED or
                                  SC_MANAGER_CONNECT or
                                  SC_MANAGER_CREATE_SERVICE or
                                  SC_MANAGER_ENUMERATE_SERVICE or
                                  SC_MANAGER_LOCK or
                                  SC_MANAGER_QUERY_LOCK_STATUS or
                                  SC_MANAGER_MODIFY_BOOT_CONFIG;

  SERVICE_CONFIG_DESCRIPTION    = $0001;

  SERVICE_WIN32_OWN_PROCESS     = $00000010;
  SERVICE_WIN32_SHARE_PROCESS   = $00000020;
  SERVICE_INTERACTIVE_PROCESS   = $00000100;

  SERVICE_BOOT_START            = $00000000;
  SERVICE_SYSTEM_START          = $00000001;
  SERVICE_AUTO_START            = $00000002;
  SERVICE_DEMAND_START          = $00000003;
  SERVICE_DISABLED              = $00000004;
  SERVICE_ERROR_IGNORE          = $00000000;
  SERVICE_ERROR_NORMAL          = $00000001;
  SERVICE_ERROR_SEVERE          = $00000002;
  SERVICE_ERROR_CRITICAL        = $00000003;

  SERVICE_CONTROL_STOP          = $00000001;
  SERVICE_CONTROL_PAUSE         = $00000002;
  SERVICE_CONTROL_CONTINUE      = $00000003;
  SERVICE_CONTROL_INTERROGATE   = $00000004;
  SERVICE_CONTROL_SHUTDOWN      = $00000005;
  SERVICE_STOPPED               = $00000001;
  SERVICE_START_PENDING         = $00000002;
  SERVICE_STOP_PENDING          = $00000003;
  SERVICE_RUNNING               = $00000004;
  SERVICE_CONTINUE_PENDING      = $00000005;
  SERVICE_PAUSE_PENDING         = $00000006;
  SERVICE_PAUSED                = $00000007;

type
  PServiceStatus = ^TServiceStatus;
  TServiceStatus = object
  public
    dwServiceType: DWORD;
    dwCurrentState: DWORD;
    dwControlsAccepted: DWORD;
    dwWin32ExitCode: DWORD;
    dwServiceSpecificExitCode: DWORD;
    dwCheckPoint: DWORD;
    dwWaitHint: DWORD;
  end;

  PServiceStatusProcess = ^TServiceStatusProcess;
  TServiceStatusProcess = object(TServiceStatus)
  public
    dwProcessId: DWORD;
    dwServiceFlags: DWORD;
  end;

  SC_HANDLE = THandle;
  SERVICE_STATUS_HANDLE = DWORD;
  TServiceTableEntry = record
    lpServiceName: PChar;
    lpServiceProc: procedure(ArgCount: DWORD; Args: PPChar); stdcall;
  end;
  PServiceTableEntry = ^TServiceTableEntry;
  {$Z4}
  SC_STATUS_TYPE = (SC_STATUS_PROCESS_INFO);
  {$Z1}

function OpenSCManager(lpMachineName, lpDatabaseName: PChar;
  dwDesiredAccess: DWORD): SC_HANDLE; stdcall; external advapi32
  name 'OpenSCManager'+{$ifdef UNICODE}'W'{$else}'A'{$endif};
function ChangeServiceConfig2(hService: SC_HANDLE; dwsInfoLevel: DWORD;
  lpInfo: Pointer): BOOL; stdcall; external advapi32 name 'ChangeServiceConfig2W';
function StartService(hService: SC_HANDLE; dwNumServiceArgs: DWORD;
  lpServiceArgVectors: Pointer): BOOL; stdcall; external advapi32
  name 'StartService'+{$ifdef UNICODE}'W'{$else}'A'{$endif};
function CreateService(hSCManager: SC_HANDLE; lpServiceName, lpDisplayName: PChar;
  dwDesiredAccess, dwServiceType, dwStartType, dwErrorControl: DWORD;
  lpBinaryPathName, lpLoadOrderGroup: PChar; lpdwTagId: LPDWORD; lpDependencies,
  lpServiceStartName, lpPassword: PChar): SC_HANDLE; stdcall; external advapi32
  name 'CreateService'+{$ifdef UNICODE}'W'{$else}'A'{$endif};
function OpenService(hSCManager: SC_HANDLE; lpServiceName: PChar;
  dwDesiredAccess: DWORD): SC_HANDLE; stdcall; external advapi32
  name 'OpenService'+{$ifdef UNICODE}'W'{$else}'A'{$endif};
function DeleteService(hService: SC_HANDLE): BOOL; stdcall; external advapi32;
function CloseServiceHandle(hSCObject: SC_HANDLE): BOOL; stdcall; external advapi32;
function QueryServiceStatus(hService: SC_HANDLE;
  var lpServiceStatus: TServiceStatus): BOOL; stdcall; external advapi32;
function QueryServiceStatusEx(hService: SC_HANDLE;
  InfoLevel: SC_STATUS_TYPE; lpBuffer: Pointer; cbBufSize: DWORD;
  var pcbBytesNeeded: DWORD): BOOL; stdcall; external advapi32;
function ControlService(hService: SC_HANDLE; dwControl: DWORD;
  var lpServiceStatus: TServiceStatus): BOOL; stdcall; external advapi32;
function SetServiceStatus(hServiceStatus: SERVICE_STATUS_HANDLE;
  var lpServiceStatus: TServiceStatus): BOOL; stdcall; external advapi32;
function RegisterServiceCtrlHandler(lpServiceName: PChar;
  lpHandlerProc: TFarProc): SERVICE_STATUS_HANDLE; stdcall; external advapi32
  name 'RegisterServiceCtrlHandler'+{$ifdef UNICODE}'W'{$else}'A'{$endif};
function StartServiceCtrlDispatcher(
  lpServiceStartTable: PServiceTableEntry): BOOL; stdcall; external advapi32
  name 'StartServiceCtrlDispatcher'+{$ifdef UNICODE}'W'{$else}'A'{$endif};


{ *** high level classes to define and manage Windows Services }

var
  /// you can set this global variable to TSynLog or TSQLLog to enable logging
  // - default is nil, i.e. disabling logging, since it may interfere with the
  // logging process of the service itself
  ServiceLog: TSynLogClass;

type
  /// all possible states of the service
  TServiceState =
    (ssNotInstalled, ssStopped, ssStarting, ssStopping, ssRunning,
     ssResuming, ssPausing, ssPaused, ssErrorRetrievingState);

  /// TServiceControler class is intended to create a new service instance or
  // to maintain (that is start, stop, pause, resume...) an existing service
  // - to provide the service itself, use the TService class
  TServiceController = class
  protected
    FSCHandle: THandle;
    FHandle: THandle;
    FStatus: TServiceStatus;
    FName: RawUTF8;
  private
    function GetStatus: TServiceStatus;
    function GetState: TServiceState;
  public
    /// Creates a new service and allows to control it and/or its configuration
    // - TargetComputer - set it to empty string if local computer is the target.
    // - DatabaseName - set it to empty string if the default database is supposed
    // ('ServicesActive').
    // - Name - name of a service.
    // - DisplayName - display name of a service.
    // - Path - a path to binary (executable) of the service created.
    // - OrderGroup - an order group name (unnecessary)
    // - Dependencies - string containing a list with names of services, which must
    // start before (every name should be separated with #0, entire
    // list should be separated with #0#0. Or, an empty string can be
    // passed if there is no dependancy).
    // - Username - login name. For service type SERVICE_WIN32_OWN_PROCESS, the
    // account name in the form of "DomainName\Username"; If the account
    // belongs to the built-in domain, ".\Username" can be specified;
    // Services of type SERVICE_WIN32_SHARE_PROCESS are not allowed to
    // specify an account other than LocalSystem. If '' is specified, the
    // service will be logged on as the 'LocalSystem' account, in which
    // case, the Password parameter must be empty too.
    // - Password - a password for login name. If the service type is
    // SERVICE_KERNEL_DRIVER or SERVICE_FILE_SYSTEM_DRIVER,
    // this parameter is ignored.
    // - DesiredAccess - a combination of following flags:
    // SERVICE_ALL_ACCESS (default value), SERVICE_CHANGE_CONFIG,
    // SERVICE_ENUMERATE_DEPENDENTS, SERVICE_INTERROGATE, SERVICE_PAUSE_CONTINUE,
    // SERVICE_QUERY_CONFIG, SERVICE_QUERY_STATUS, SERVICE_START, SERVICE_STOP,
    // SERVICE_USER_DEFINED_CONTROL
    // - ServiceType - a set of following flags:
    // SERVICE_WIN32_OWN_PROCESS (default value, which specifies a Win32 service
    // that runs in its own process), SERVICE_WIN32_SHARE_PROCESS,
    // SERVICE_KERNEL_DRIVER, SERVICE_FILE_SYSTEM_DRIVER,
    // SERVICE_INTERACTIVE_PROCESS (default value, which enables a Win32 service
    // process to interact with the desktop)
    // - StartType - one of following values:
    // SERVICE_BOOT_START, SERVICE_SYSTEM_START,
    // SERVICE_AUTO_START (which specifies a device driver or service started by
    // the service control manager automatically during system startup),
    // SERVICE_DEMAND_START (default value, which specifies a service started by
    // a service control manager when a process calls the StartService function,
    // that is the TServiceController.Start method), SERVICE_DISABLED
    // - ErrorControl - one of following:
    // SERVICE_ERROR_IGNORE, SERVICE_ERROR_NORMAL (default value, by which
    // the startup program logs the error and displays a message but continues
    // the startup operation), SERVICE_ERROR_SEVERE,
    // SERVICE_ERROR_CRITICAL
    constructor CreateNewService(const TargetComputer, DatabaseName,
      Name, DisplayName, Path: string;
      const OrderGroup: string = ''; const Dependencies: string = '';
      const Username: string = ''; const Password: string = '';
      DesiredAccess: DWORD = SERVICE_ALL_ACCESS;
      ServiceType: DWORD = SERVICE_WIN32_OWN_PROCESS or SERVICE_INTERACTIVE_PROCESS;
      StartType: DWORD = SERVICE_DEMAND_START; ErrorControl: DWORD = SERVICE_ERROR_NORMAL);
    /// wrapper around CreateNewService() to install the current executable as service
    class function Install(const Name,DisplayName,Description: string;
      AutoStart: boolean; ExeName: TFileName=''; Dependencies: string=''): TServiceState;
    /// Opens an existing service, in order  to control it or its configuration
    // from your application. Parameters (strings are unicode-ready since Delphi 2009):
    // - TargetComputer - set it to empty string if local computer is the target.
    // - DatabaseName - set it to empty string if the default database is supposed
    // ('ServicesActive').
    // - Name - name of a service.
    // - DesiredAccess - a combination of following flags:
    // SERVICE_ALL_ACCESS, SERVICE_CHANGE_CONFIG, SERVICE_ENUMERATE_DEPENDENTS,
    // SERVICE_INTERROGATE, SERVICE_PAUSE_CONTINUE, SERVICE_QUERY_CONFIG,
    // SERVICE_QUERY_STATUS, SERVICE_START, SERVICE_STOP, SERVICE_USER_DEFINED_CONTROL
    constructor CreateOpenService(const TargetComputer, DataBaseName, Name: String;
      DesiredAccess: DWORD = SERVICE_ALL_ACCESS);
    /// release memory and handles
    destructor Destroy; override;
    /// Handle of SC manager
    property SCHandle: THandle read FSCHandle;
    /// Handle of service opened or created
    // - its value is 0 if something failed in any Create*() method
    property Handle: THandle read FHandle;
    /// Retrieve the Current status of the service
    property Status: TServiceStatus read GetStatus;
    /// Retrieve the Current state of the service
    property State: TServiceState read GetState;
    /// Requests the service to stop
    function Stop: boolean;
    /// Requests the service to pause
    function Pause: boolean;
    /// Requests the paused service to resume
    function Resume: boolean;
    /// Requests the service to update immediately its current status information
    // to the service control manager
    function Refresh: boolean;
    /// Request the service to shutdown
    // - this function always return false
    function Shutdown: boolean;
    /// Removes service from the system, i.e. close the Service
    function Delete: boolean;
    /// starts the execution of a service with some specified arguments
    // - this version expect PChar pointers, either AnsiString (for FPC and old
    //  Delphi compiler), either UnicodeString (till Delphi 2009)
    function Start(const Args: array of PChar): boolean;
    /// try to define the description text of this service
    procedure SetDescription(const Description: string);
    /// this class method will check the command line parameters, and will let
    //  control the service according to it
    // - MyServiceSetup.exe /install will install the service
    // - MyServiceSetup.exe /start   will start the service
    // - MyServiceSetup.exe /stop    will stop the service
    // - MyServiceSetup.exe /uninstall will uninstall the service
    // - so that you can write in the main block of your .dpr:
    // !CheckParameters('MyService.exe',HTTPSERVICENAME,HTTPSERVICEDISPLAYNAME);
    // - if ExeFileName='', it will install the current executable
    // - optional Description and Dependencies text may be specified
    class procedure CheckParameters(const ExeFileName: TFileName;
      const ServiceName,DisplayName,Description: string; const Dependencies: string='');
  end;

  {$M+}
  TService = class;
  {$M-}

  /// callback procedure for Windows Service Controller
  TServiceControlHandler = procedure(CtrlCode: DWORD); stdcall;

  /// event triggered for Control handler
  TServiceControlEvent = procedure(Sender: TService; Code: DWORD) of object;

  /// event triggered to implement the Service functionality
  TServiceEvent = procedure(Sender: TService) of object;

  /// TService is the class used to implement a service provided by an application
  TService = class
  protected
    fSName: String;
    fDName: String;
    fStartType: DWORD;
    fServiceType: DWORD;
    fData: DWORD;
    fControlHandler: TServiceControlHandler;
    fOnControl: TServiceControlEvent;
    fOnInterrogate: TServiceEvent;
    fOnPause: TServiceEvent;
    fOnShutdown: TServiceEvent;
    fOnStart: TServiceEvent;
    fOnExecute: TServiceEvent;
    fOnResume: TServiceEvent;
    fOnStop: TServiceEvent;
    fStatusRec: TServiceStatus;
    fArgsList: array of string;
    fJumper: PByteArray;
    fStatusHandle: THandle;
    function GetArgCount: Integer;
    function GetArgs(Idx: Integer): String;
    function GetInstalled: boolean;
    procedure SetStatus(const Value: TServiceStatus);
    procedure CtrlHandle(Code: DWORD);
    function GetControlHandler: TServiceControlHandler;
    procedure SetControlHandler(const Value: TServiceControlHandler);
  public
    /// this method is the main service entrance, from the OS point of view
    // - it will call OnControl/OnStop/OnPause/OnResume/OnShutdown events
    // - and report the service status to the system (via ReportStatus method)
    procedure DoCtrlHandle(Code: DWORD); virtual;
    /// Creates the service
    // - the service is added to the internal registered services
    // - main application must call the global ServicesRun procedure to actually
    // start the services
    // - caller must free the TService instance when it's no longer used
    constructor Create(const aServiceName, aDisplayName: String); reintroduce; virtual;
    /// free memory and release handles
    destructor Destroy; override;
    /// Reports new status to the system
    function ReportStatus(dwState, dwExitCode, dwWait: DWORD): BOOL;
    /// Installs the service in the database
    // - return true on success
    // - create a local TServiceController with the current executable file,
    // with the supplied command line parameters
    function Install(const Params: string=''): boolean;
    /// Removes the service from database
    //  - uses a local TServiceController with the current Service Name
    procedure Remove;
    /// Starts the service
    //  - uses a local TServiceController with the current Service Name
    procedure Start;
    /// Stops the service
    // - uses a local TServiceController with the current Service Name
    procedure Stop;
    /// this is the main method, in which the Service should implement its run
    procedure Execute; virtual;

    /// Number of arguments passed to the service by the service controler
    property ArgCount: Integer read GetArgCount;
    /// List of arguments passed to the service by the service controler
    property Args[Idx: Integer]: String read GetArgs;
    /// Any data You wish to associate with the service object
    property Data: DWORD read FData write FData;
    /// Whether service is installed in DataBase
    // - uses a local TServiceController to check if the current Service Name exists
    property Installed: boolean read GetInstalled;
    /// Current service status
    // - To report new status to the system, assign another
    // value to this record, or use ReportStatus method (preferred)
    property Status: TServiceStatus read fStatusRec write SetStatus;
    /// Callback handler for Windows Service Controller
    // - if handler is not set, then auto generated handler calls DoCtrlHandle
    // (note that this auto-generated stubb is... not working yet - so you should
    // either set your own procedure to this property, or use TServiceSingle)
    // - a typical control handler may be defined as such:
    // ! var MyGlobalService: TService;
    // !
    // ! procedure MyServiceControlHandler(Opcode: LongWord); stdcall;
    // ! begin
    // !   if MyGlobalService<>nil then
    // !     MyGlobalService.DoCtrlHandle(Opcode);
    // ! end;
    // !
    // ! ...
    // ! MyGlobalService := TService.Create(...
    // ! MyGlobalService.ControlHandler := MyServiceControlHandler;
    property ControlHandler: TServiceControlHandler
      read GetControlHandler write SetControlHandler;
    /// Start event is executed before the main service thread (i.e. in the Execute method)
    property OnStart: TServiceEvent read fOnStart write fOnStart;
    /// custom Execute event
    // - launched in the main service thread (i.e. in the Execute method)
    property OnExecute: TServiceEvent read fOnExecute write fOnExecute;
    /// custom event triggered when a Control Code is received from Windows
    property OnControl: TServiceControlEvent read fOnControl write fOnControl;
    /// custom event triggered when the service is stopped
    property OnStop: TServiceEvent read fOnStop write fOnStop;
    /// custom event triggered when the service is paused
    property OnPause: TServiceEvent read fOnPause write fOnPause;
    /// custom event triggered when the service is resumed
    property OnResume: TServiceEvent read fOnResume write fOnResume;
    /// custom event triggered when the service receive an Interrogate
    property OnInterrogate: TServiceEvent read fOnInterrogate write fOnInterrogate;
    /// custom event triggered when the service is shut down
    property OnShutdown: TServiceEvent read fOnShutdown write fOnShutdown;
  published
    /// Name of the service. Must be unique
    property ServiceName: String read fSName;
    /// Display name of the service
    property DisplayName: String read fDName write fDName;
    /// Type of service
    property ServiceType: DWORD read fServiceType write fServiceType;
    /// Type of start of service
    property StartType: DWORD read fStartType write fStartType;
  end;

  /// inherit from this service if your application has a single service
  // - note that TService jumper does not work well - so use this instead
  TServiceSingle = class(TService)
  public
    /// will set a global function as service controller
    constructor Create(const aServiceName, aDisplayName: String); override;
    /// will release the global service controller
    destructor Destroy; override;
  end;


var
  /// the internal list of Services handled by this unit
  // - not to be accessed directly: create TService instances, and they will
  // be added/registered to this list
  // - then run the global ServicesRun procedure
  // - every TService instance is to be freed by the main application, when
  // it's no more used
  Services: TSynList = nil;

  /// the main TService instance running
  ServiceSingle: TServiceSingle = nil;

/// launch the registered Services execution
// - the registered list of service provided by the aplication is sent
// to the operating system
// - returns TRUE on success
// - returns FALSE on error (to get extended information, call GetLastError)
function ServicesRun: boolean;

/// convert the Control Code retrieved from Windows into a service state
// enumeration item
function CurrentStateToServiceState(CurrentState: DWORD): TServiceState;

/// return the ready to be displayed text of a TServiceState value
function ServiceStateText(State: TServiceState): string;

/// return service PID
function GetServicePid(const aServiceName: string): DWORD;

/// kill Windows process
function KillProcess(pid: DWORD; waitseconds: integer = 30): boolean;

{$else}

/// low-level function able to properly run or fork the current process
// then execute the start/stop methods of a TSynDaemon / TDDDDaemon instance
// - fork will create a local /run/[ProgramName]-[ProgramPathHash].pid file name
procedure RunUntilSigTerminated(daemon: TObject; dofork: boolean;
  const start, stop: TThreadMethod; log: TSynLog = nil; const servicename: string = '');

/// kill a process previously created by RunUntilSigTerminated(dofork=true)
// - will lookup a local /run/[ProgramName]-[ProgramPathHash].pid file name to
// retrieve the actual PID to be killed, then send a SIGTERM, and wait
// waitseconds for the .pid file to disapear
// - returns true on success, false on error (e.g. no valid .pid file or
// the file didn't disappear, which may mean that the daemon is broken)
function RunUntilSigTerminatedForKill(waitseconds: integer = 30): boolean;

/// local .pid file name as created by RunUntilSigTerminated(dofork=true)
function RunUntilSigTerminatedPidFile: TFileName;

var
  /// once SynDaemonIntercept has been called, this global variable
  // contains the SIGQUIT / SIGTERM / SIGINT received signal
  SynDaemonTerminated: integer;

/// enable low-level interception of executable stop signals
// - any SIGQUIT / SIGTERM / SIGINT signal will set appropriately the global
// SynDaemonTerminated variable, with an optional logged entry to log
// - as called e.g. by RunUntilSigTerminated()
// - you can call this method several times with no issue
procedure SynDaemonIntercept(log: TSynLog=nil);

{$endif MSWINDOWS}

type
  /// command line patterns recognized by ParseCommandArgs()
  TParseCommand = (
    pcHasRedirection, pcHasSubCommand, pcHasParenthesis,
    pcHasJobControl, pcHasWildcard, pcHasShellVariable,
    pcUnbalancedSingleQuote, pcUnbalancedDoubleQuote,
    pcTooManyArguments, pcInvalidCommand, pcHasEndingBackSlash);
  TParseCommands = set of TParseCommand;
  PParseCommands = ^TParseCommands;
  /// used to store references of arguments recognized by ParseCommandArgs()
  TParseCommandsArgs = array[0..31] of PAnsiChar;
  PParseCommandsArgs = ^TParseCommandsArgs;

const
  /// identifies some bash-specific processing
  PARSECOMMAND_BASH = [pcHasRedirection .. pcHasShellVariable];
  /// identifies obvious invalid content
  PARSECOMMAND_ERROR = [pcUnbalancedSingleQuote .. pcHasEndingBackSlash];

/// low-level parsing of a RunCommand() execution command
// - parse and fills argv^[0..argc^-1] with corresponding arguments, after
// un-escaping and un-quoting if applicable, using temp^ to store the content
// - if argv=nil, do only the parsing, not the argument extraction - could be
// used for fast validation of the command line syntax
// - you can force arguments OS flavor using the posix parameter - note that
// Windows parsing is not consistent by itself (e.g. double quoting or
// escaping depends on the actual executable called) so returned flags
// should be considered as indicative only with posix=false
function ParseCommandArgs(const cmd: RawUTF8; argv: PParseCommandsArgs = nil;
  argc: PInteger = nil; temp: PRawUTF8 = nil;
  posix: boolean = {$ifdef MSWINDOWS}false{$else}true{$endif}): TParseCommands;

function ToText(cmd: TParseCommands): shortstring; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// like SysUtils.ExecuteProcess, but allowing not to wait for the process to finish
// - optional env value follows 'n1=v1'#0'n2=v2'#0'n3=v3'#0#0 Windows layout
function RunProcess(const path, arg1: TFileName; waitfor: boolean;
  const arg2: TFileName=''; const arg3: TFileName=''; const arg4: TFileName='';
  const arg5: TFileName=''; const env: TFileName=''; envaddexisting: boolean=false): integer;

/// like fpSystem, but cross-platform
// - under POSIX, calls bash only if needed, after ParseCommandArgs() analysis
// - under Windows (especially Windows 10), creating a process can be dead slow
// https://randomascii.wordpress.com/2019/04/21/on2-in-createprocess
function RunCommand(const cmd: TFileName; waitfor: boolean;
  const env: TFileName=''; envaddexisting: boolean=false;
  parsed: PParseCommands=nil): integer;


{ *** cross-plaform high-level services/daemons }

type
  /// abstract parent containing information able to initialize a TSynDaemon class
  // - will handle persistence as JSON local files
  // - you may consider using TDDDAppSettingsAbstract from dddInfraSettings
  TSynDaemonSettings  = class(TSynJsonFileSettings)
  protected
    fServiceName: string;
    fServiceDisplayName: string;
    fLog: TSynLogInfos;
    fLogPath: TFileName;
    fLogRotateFileCount: integer;
    fLogClass: TSynLogClass;
    fServiceDependencies: string;
  public
    /// initialize and set the default settings
    constructor Create; override;
    /// define the log information into the supplied TSynLog class
    // - if you don't call this method, the logging won't be initiated
    // - is to be called typically in the overriden Create constructor of the
    // associated TSynDaemon class, just after "inherited Create"
    procedure SetLog(aLogClass: TSynLogClass);
    /// returns user-friendly description of the service, including version
    // information and company copyright (if available)
    function ServiceDescription: string;
    /// read-only access to the TSynLog class, if SetLog() has been called
    property LogClass: TSynLogClass read fLogClass;
    /// optional service dependencies
    // - not published by default: could be defined if needed, or e.g. set in
    // overriden constructor
    // - several depending services may be set by appending #0 between names
    property ServiceDependencies: string read fServiceDependencies write fServiceDependencies;
  published
    /// the service name, as used internally by Windows or the TSynDaemon class
    // - default is the executable name
    property ServiceName: string read fServiceName write fServiceName;
    /// the service name, as displayed by Windows or at the console level
    // - default is the executable name
    property ServiceDisplayName: string read fServiceDisplayName write fServiceDisplayName;
    /// if not void, will enable the logs (default is LOG_STACKTRACE)
    property Log: TSynLogInfos read fLog write fLog;
    /// allow to customize where the logs should be written
    property LogPath: TFileName read fLogPath write fLogPath;
    /// how many files will be rotated (default is 2)
    property LogRotateFileCount: integer read fLogRotateFileCount write fLogRotateFileCount;
  end;
  /// meta-class of TSynDaemon settings information
  TSynDaemonSettingsClass = class of TSynDaemonSettings;

  /// abstract parent to implements a daemon/service
  // - inherit from this abstract class and override Start and Stop methods
  // - you may consider using TDDDAdministratedDaemon from dddInfraApps
  TSynDaemon = class(TSynPersistent)
  protected
    fConsoleMode: boolean;
    fWorkFolderName: TFileName;
    fSettings: TSynDaemonSettings;
    function CustomCommandLineSyntax: string; virtual;
    {$ifdef MSWINDOWS}
    procedure DoStart(Sender: TService);
    procedure DoStop(Sender: TService);
    {$endif}
  public
    /// initialize the daemon, creating the associated settings
    // - TSynDaemonSettings instance will be owned and freed by the daemon
    // - any non supplied folder name will be replaced by a default value
    // (executable folder under Windows, or /etc /var/log on Linux)
    constructor Create(aSettingsClass: TSynDaemonSettingsClass;
      const aWorkFolder, aSettingsFolder, aLogFolder: TFileName;
      const aSettingsExt: TFileName = '.settings';
      const aSettingsName: TFileName = ''); reintroduce;
    /// main entry point of the daemon, to process the command line switches
    // - aAutoStart is used only under Windows
    procedure CommandLine(aAutoStart: boolean=true);
    /// inherited class should override this abstract method with proper process
    procedure Start; virtual; abstract;
    /// inherited class should override this abstract method with proper process
    // - should do nothing if the daemon was already stopped
    procedure Stop; virtual; abstract;
    /// call Stop, finalize the instance, and its settings
    destructor Destroy; override;
  published
    /// if this instance was run as /console or /verb
    property ConsoleMode: boolean read fConsoleMode;
    /// the settings associated with this daemon
    // - will be allocated in Create constructor, and released in Destroy
    property Settings: TSynDaemonSettings read fSettings;
  end;

{$ifdef MSWINDOWS}

  /// Enum synchronized with WinAPI
  // - see https://docs.microsoft.com/en-us/windows/desktop/secauthz/privilege-constants
  TWinSystemPrivilege = (wspCreateToken, wspAssignPrimaryToken, wspLockMemory,
    wspIncreaseQuota, wspUnsolicitedInput, wspMachineAccount, wspTCP, wspSecurity,
    wspTakeOwnership, wspLoadDriver, wspSystemProfile, wspSystemTime, wspProfSingleProcess,
    wspIncBasePriority, wspCreatePageFile, wspCreatePermanent, wspBackup, wspRestore,
    wspShutdown, wspDebug, wspAudit, wspSystemEnvironment, wspChangeNotify,
    wspRemoteShutdown, wspUndock, wspSyncAgent, wspEnableDelegation, wspManageVolume,
    wspImpersonate, wspCreateGlobal, wspTrustedCredmanAccess, wspRelabel,
    wspIncWorkingSet, wspTimeZone, wspCreateSymbolicLink);
  TWinSystemPrivileges = set of TWinSystemPrivilege;
  TPrivilegeTokenType = (pttProcess, pttThread);

  /// object dedicated to management of available privileges for Windows platform
  // - not all available privileges are active for process
  // - for usage of more advanced WinAPI, explicit enabling of privilege is
  // sometimes needed
  TSynWindowsPrivileges = object
  private
    fAvailable: TWinSystemPrivileges;
    fEnabled: TWinSystemPrivileges;
    fDefEnabled: TWinSystemPrivileges;
    function SetPrivilege(aPrivilege: Pointer; aEnablePrivilege: boolean): boolean;
    procedure LoadPrivileges;
  public
    /// handle to privileges token
    Token: THandle;
    /// initialize the object dedicated to management of available privileges
    // - aTokenPrivilege can be used for current process or current thread
    procedure Init(aTokenPrivilege: TPrivilegeTokenType = pttProcess);
    /// finalize the object and relese Token handle
    // - aRestoreInitiallyEnabled parameter can be used to restore initially
    // state of enabled privileges
    procedure Done(aRestoreInitiallyEnabled: boolean = true);
    /// enable privilege
    // - if aPrivilege is already enabled return true, if operation is not
    // possible (required privilege doesn't exist or API error) return false
    function Enable(aPrivilege: TWinSystemPrivilege): boolean;
    /// disable privilege
    // - if aPrivilege is already disabled return true, if operation is not
    // possible (required privilege doesn't exist or API error) return false
    function Disable(aPrivilege: TWinSystemPrivilege): boolean;
    /// set of available privileges for current process/thread
    property Available: TWinSystemPrivileges read fAvailable;
    /// set of enabled privileges for current process/thread
    property Enabled: TWinSystemPrivileges read fEnabled;
  end;

  TWinProcessAvailableInfos = set of (wpaiPID, wpaiBasic, wpaiPEB, wpaiCommandLine, wpaiImagePath);
  PWinProcessInfo = ^TWinProcessInfo;
  TWinProcessInfo = record
    AvailableInfo: TWinProcessAvailableInfos;
    PID: Cardinal;
    ParentPID: Cardinal;
    SessionID: Cardinal;
    PEBBaseAddress: Pointer;
    AffinityMask: Cardinal;
    BasePriority: LongInt;
    ExitStatus: LongInt;
    BeingDebugged: Byte;
    ImagePath: SynUnicode;
    CommandLine: SynUnicode;
  end;
  TWinProcessInfoDynArray = array of TWinProcessInfo;

procedure GetProcessInfo(aPid: Cardinal; out aInfo: TWinProcessInfo); overload;
procedure GetProcessInfo(const aPidList: TCardinalDynArray; out aInfo: TWinProcessInfoDynArray); overload;

{$endif MSWINDOWS}

const
  /// text identifier typically used before command line switches
  // - equals '/' on Windows, and '--' on POSIX systems
  CMDLINESWITCH = {$ifdef MSWINDOWS}'/'{$else}'--'{$endif};


implementation

{$ifdef MSWINDOWS}

{ TServiceController }

constructor TServiceController.CreateNewService(const TargetComputer,
  DatabaseName,Name,DisplayName,Path,OrderGroup,Dependencies,Username,Password: String;
  DesiredAccess,ServiceType,StartType,ErrorControl: DWORD);
var Exe: TFileName;
   backupError: cardinal;
begin
  inherited Create;
  if Path='' then begin
    ServiceLog.Add.Log(sllError,'CreateNewService("%","%") with Path=""',
      [Name,DisplayName]);
    Exit;
  end;
  if TargetComputer='' then
  if GetDriveType(pointer(ExtractFileDrive(Path)))=DRIVE_REMOTE then begin
    Exe := ExpandUNCFileName(Path);
    if (copy(Exe,1,12)<>'\\localhost\') or (Exe[14]<>'$') then begin
      ServiceLog.Add.Log(sllError,'CreateNewService("%","%") on remote drive: Path="%" is %',
        [Name,DisplayName,Path,Exe]);
      Exit;
    end;
    system.delete(Exe,1,12); // \\localhost\c$\... -> c:\...
    Exe[2] := ':';
  end else
    Exe := Path;
  StringToUTF8(Name,FName);
  FSCHandle := OpenSCManager(pointer(TargetComputer), pointer(DatabaseName),
    SC_MANAGER_ALL_ACCESS);
  if FSCHandle=0 then begin
    backupError := GetLastError;
    ServiceLog.Add.Log(sllLastError,'OpenSCManager(''%'',''%'') for [%]',
      [TargetComputer,DatabaseName,FName]);
    SetLastError(backupError);
    Exit;
  end;
  FHandle := CreateService(FSCHandle, pointer(Name), pointer(DisplayName),
               DesiredAccess, ServiceType, StartType, ErrorControl, pointer(Exe),
               pointer(OrderGroup), nil, pointer(Dependencies),
               pointer(Username), pointer(Password));
  if FHandle=0 then begin
    backupError := GetLastError;
    ServiceLog.Add.Log(sllLastError,'CreateService("%","%","%")',[Name,DisplayName,Path]);
    SetLastError(backupError);
  end;
end;

constructor TServiceController.CreateOpenService(const TargetComputer,
  DataBaseName, Name: String; DesiredAccess: DWORD);
var backupError: cardinal;
begin
  inherited Create;
  StringToUTF8(Name,FName);
  FSCHandle := OpenSCManager(pointer(TargetComputer), pointer(DatabaseName),
    GENERIC_READ);
  if FSCHandle = 0 then begin
    backupError := GetLastError;
    ServiceLog.Add.Log(sllLastError,'OpenSCManager(''%'',''%'') for [%]',
      [TargetComputer,DatabaseName,FName]);
    SetLastError(backupError);
    Exit;
  end;
  FHandle := OpenService(FSCHandle, pointer(Name), DesiredAccess);
  if FHandle=0 then begin
    backupError := GetLastError;
    ServiceLog.Add.Log(sllLastError,'OpenService("%")',[Name]);
    SetLastError(backupError);
  end;
end;

function TServiceController.Delete: boolean;
begin
  Result := FALSE;
  if FHandle <> 0 then
    if DeleteService(FHandle) then begin
      Result := CloseServiceHandle(FHandle);
      FHandle := 0;
    end
    else ServiceLog.Add.Log(sllLastError,'DeleteService("%")',[FName]);
end;

destructor TServiceController.Destroy;
begin
  if FHandle <> 0 then
    CloseServiceHandle(FHandle);
  if FSCHandle <> 0 then
    CloseServiceHandle(FSCHandle);
  inherited;
end;

function TServiceController.GetState: TServiceState;
begin
  if (self=nil) or (FSCHandle=0) or (FHandle=0) then
    result := ssNotInstalled else
    result := CurrentStateToServiceState(Status.dwCurrentState);
  ServiceLog.Add.Log(sllTrace,FName,TypeInfo(TServiceState),result,self);
end;

function TServiceController.GetStatus: TServiceStatus;
begin
  FillChar(FStatus, Sizeof(FStatus), 0);
  QueryServiceStatus(FHandle, FStatus);
  Result := FStatus;
end;

function TServiceController.Pause: boolean;
begin
  Result := ControlService(FHandle, SERVICE_CONTROL_PAUSE, FStatus);
end;

function TServiceController.Refresh: boolean;
begin
  Result := ControlService(FHandle, SERVICE_CONTROL_INTERROGATE, FStatus);
end;

function TServiceController.Resume: boolean;
begin
  Result := ControlService(FHandle, SERVICE_CONTROL_CONTINUE, FStatus);
end;

function TServiceController.Shutdown: boolean;
begin
  Result := ControlService(FHandle, SERVICE_CONTROL_SHUTDOWN, FStatus);
end;

function TServiceController.Start(const Args: array of PChar): boolean;
begin
  if length(Args)=0 then
    Result := StartService(FHandle, 0, nil) else
    Result := StartService(FHandle, length(Args), @Args[0]);
end;

function TServiceController.Stop: boolean;
begin
  Result := ControlService(FHandle, SERVICE_CONTROL_STOP, FStatus);
end;

procedure TServiceController.SetDescription(const Description: string);
var desc: SynUnicode;
begin
  if Description='' then
    exit;
  StringToSynUnicode(Description, desc);
  ChangeServiceConfig2(FHandle, SERVICE_CONFIG_DESCRIPTION, @desc);
end;

class procedure TServiceController.CheckParameters(const ExeFileName: TFileName;
  const ServiceName, DisplayName, Description, Dependencies: string);
var param: string;
    i: integer;
procedure ShowError(const Msg: RawUTF8);
begin
  ServiceLog.Add.Log(sllLastError,'During % for %',[Msg,param]);
  if not IsConsole then
    exit;
  {$I-} // ignore if no console has been allocated
  writeln(ServiceName,': Error "',Msg,'" for ',param);
  ioresult;
  {$I+}
end;
begin
  for i := 1 to ParamCount do begin
    param := SysUtils.LowerCase(paramstr(i));
    ServiceLog.Add.Log(sllInfo,'Controling % with command [%]',[ServiceName,param]);
    if param='/install' then
     TServiceController.Install(
       ServiceName,DisplayName,Description,true,ExeFileName,Dependencies) else
    with TServiceController.CreateOpenService('','',ServiceName) do
    try
      if State=ssErrorRetrievingState then
        ShowError('State') else
      if param='/uninstall' then begin
        if not Stop then
          ShowError('Stop');
        if not Delete then
          ShowError('Delete');
      end else
      if param='/stop' then begin
        if not Stop then
          ShowError('Stop');
      end else
      if param='/start' then begin
        if not Start([]) then
          ShowError('Start');
      end;
    finally
      Free;
    end;
  end;
end;

class function TServiceController.Install(const Name, DisplayName,
  Description: string; AutoStart: boolean; ExeName: TFileName; Dependencies: string): TServiceState;
var ctrl: TServiceController;
    start: DWORD;
begin
  if AutoStart then
    start := SERVICE_AUTO_START else
    start := SERVICE_DEMAND_START;
  if ExeName='' then
    ExeName := ExeVersion.ProgramFileName;
  ctrl := TServiceController.CreateNewService('','',Name,DisplayName,ExeName,
    '',Dependencies,'','',SERVICE_ALL_ACCESS,SERVICE_WIN32_OWN_PROCESS,start);
  try
    result := ctrl.State;
    if result<>ssNotInstalled then
      ctrl.SetDescription(Description);
  finally
    ctrl.Free;
  end;
end;


{ TService }

function FindServiceIndex(const Name: String): integer;
begin
  if Services<>nil then
  for result := 0 to Services.Count-1 do
    if TService(Services.List[result]).ServiceName=Name then
      exit;
  result := -1;
end;

constructor TService.Create(const aServiceName, aDisplayName: String);
begin
  if FindServiceIndex(aServiceName)>=0 then
    raise EServiceException.CreateUTF8('%.Create: Attempt to install a service ' +
      'with duplicated name: %', [self, aServiceName]);
  fSName := aServiceName;
  fDName := aDisplayName;
  if aDisplayName = '' then
    fDName := aServiceName;
  if Services=nil then
    GarbageCollectorFreeAndNil(Services,TSynList.Create);
  Services.Add(self);
  fServiceType := SERVICE_WIN32_OWN_PROCESS or SERVICE_INTERACTIVE_PROCESS;
  fStartType   := SERVICE_AUTO_START;
  fStatusRec.dwServiceType := fServiceType;
  fStatusRec.dwCurrentState := SERVICE_STOPPED;
  fStatusRec.dwControlsAccepted := 31;
  fStatusRec.dwWin32ExitCode := NO_ERROR;
  ServiceLog.Add.Log(sllInfo,'Create: % (%) running as [%]',
    [ServiceName,aDisplayName,ExeVersion.ProgramFullSpec],self);
end;

procedure TService.CtrlHandle(Code: DWORD);
begin
  DoCtrlHandle(Code);
end;

destructor TService.Destroy;
var i: integer;
begin
  if fsName<>'' then begin
    i := FindServiceIndex(fsName);
    if i<0 then
      raise EServiceException.CreateUTF8('%.Destroy: Cannot find service % to remove',
       [self, fsName]);
    Services.Delete(i);
    if fJumper<>nil then
      VirtualFree(fJumper, 0, MEM_RELEASE);
  end;
  inherited Destroy;
end;

procedure TService.DoCtrlHandle(Code: DWORD);
var log: ISynLog;
begin
  log := ServiceLog.Enter(self, 'DoCtrlHandle');
  if log<>nil then
    log.Log(sllInfo,'%: command % received from OS',[ServiceName,Code],self);
  try
    case Code of
    SERVICE_CONTROL_STOP: begin
     ReportStatus(SERVICE_STOP_PENDING, NO_ERROR, 0);
     try
       if Assigned(fOnStop) then
         fOnStop(Self);
       ReportStatus(SERVICE_STOPPED, NO_ERROR, 0);
     except
       ReportStatus(SERVICE_STOPPED, ERROR_CAN_NOT_COMPLETE, 0);
     end;
    end;
    SERVICE_CONTROL_PAUSE: begin
     ReportStatus(SERVICE_PAUSE_PENDING, NO_ERROR, 0);
     try
       if Assigned(fOnPause) then
         fOnPause(Self);
       ReportStatus(SERVICE_PAUSED, NO_ERROR, 0)
     except
       ReportStatus(SERVICE_PAUSED, ERROR_CAN_NOT_COMPLETE, 0)
     end;
    end;
    SERVICE_CONTROL_CONTINUE: begin
     ReportStatus(SERVICE_CONTINUE_PENDING, NO_ERROR, 0);
     try
       if Assigned(fOnResume) then
         fOnResume(Self);
       ReportStatus(SERVICE_RUNNING, NO_ERROR, 0);
     except
       ReportStatus(SERVICE_RUNNING, ERROR_CAN_NOT_COMPLETE, 0);
     end;
    end;
    SERVICE_CONTROL_SHUTDOWN: begin
     if Assigned(fOnShutdown) then
       fOnShutdown(Self);
     Code := 0;
    end;
    SERVICE_CONTROL_INTERROGATE: begin
     SetServiceStatus(FStatusHandle, fStatusRec);
     if Assigned(fOnInterrogate) then
       fOnInterrogate(Self);
    end;
    end;
    if Assigned(fOnControl) then
      fOnControl(Self, Code);
  except
  end;
end;

procedure TService.Execute;
begin
  try
    if Assigned(fOnStart) then
      fOnStart(@Self);
    ReportStatus(SERVICE_RUNNING, NO_ERROR, 0);
    if Assigned(fOnExecute) then
      fOnExecute(@Self);
  except
    ReportStatus(SERVICE_RUNNING, ERROR_CAN_NOT_COMPLETE, 0);
  end;
end;

function TService.GetArgCount: Integer;
begin
  result := length(FArgsList);
end;

function TService.GetArgs(Idx: Integer): String;
begin
  if cardinal(Idx)>cardinal(high(FArgsList)) then
    result := '' else // avoid GPF
    result := FArgsList[Idx];
end;


{$ifdef CPUX86}
  {.$define X86JUMPER} // this preliminary version is buggy so disabled
  // a single service per excecutable is fine enough for our daemons
  // also for proper Delphi 10.4 compilation with no hint
{$endif CPUX86}

{$ifdef X86JUMPER}
procedure JumpToService;
asm
  pop  eax
  mov  eax, [eax]           // retrieve TService self value
  mov  edx, [esp+4]
  call TService.CtrlHandle
  ret  4
end;
{$endif X86JUMPER}

function TService.GetControlHandler: TServiceControlHandler;
{$ifdef X86JUMPER}
var AfterCallAddr: Pointer;
    Offset: Integer;
{$endif X86JUMPER}
begin
  Result := fControlHandler;
  if not Assigned(Result) then
    ServiceLog.Add.Log(sllError,'%.GetControlHandler with fControlHandler=nil: '+
      'use TServiceSingle or set a custom ControlHandler',[self]);
  {$ifdef X86JUMPER}
  if not Assigned(Result) then
  begin
    raise EServiceException.Create('Automated jumper generation is not working: '+
     'use TServiceSingle or set a custom ControlHandler');
    if fJumper=nil then begin
      fJumper := VirtualAlloc(nil, 5+sizeof(Pointer), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
      if fJumper=nil then
        raise EServiceException.CreateUTF8('Cannot allocate memory for service jump gate: %',
          [fSName]);
      AfterCallAddr := Pointer(PtrUInt(fJumper)+5);
      Offset := PtrUInt(@JumpToService)-PtrUInt(AfterCallAddr);
      fJumper[0] := $E8; // call opcode
      PInteger(@fJumper[1])^ := Offset;       // points to JumpToService
      PPtrUInt(@fJumper[5])^ := PtrUInt(self); // will be set as EAX=self
    end;
    Result := Pointer(fJumper);
  end;
  {$endif X86JUMPER}
end;

function TService.GetInstalled: boolean;
begin
  with TServiceController.CreateOpenService('','',fSName,SERVICE_QUERY_STATUS) do
  try
    result := Handle<>0;
  finally
    Free;
  end;
end;

function TService.Install(const Params: string): boolean;
var schService: SC_HANDLE;
    schSCManager: SC_HANDLE;
    ServicePath: TFileName;
begin
  result := false;
  if installed then
    exit;
  ServicePath := ExeVersion.ProgramFileName;
  if Params<>'' then
    ServicePath := ServicePath+' '+Params;
  schSCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if (schSCManager>0) then begin
     schService := CreateService(schSCManager,
       pointer(fSName), pointer(fDName), SERVICE_ALL_ACCESS,
       fServiceType, fStartType, SERVICE_ERROR_NORMAL,
       pointer(ServicePath), nil, nil, nil, nil, nil);
     if (schService>0) then begin
       result := true;
       CloseServiceHandle(schService);
     end;
  end;
end;

procedure TService.Remove;
begin
  with TServiceController.CreateOpenService('','',fSName,SERVICE_ALL_ACCESS) do
  try
    if Handle=0 then exit;
    Stop;
    Delete;
  finally
    Free;
  end;
end;

function TService.ReportStatus(dwState, dwExitCode, dwWait: DWORD): BOOL;
var status: string;
begin
  status := ServiceStateText(CurrentStateToServiceState(dwState));
  ServiceLog.Add.Log(sllInfo,'% ReportStatus(%,%,%)',
    [ServiceName,status,dwExitCode,dwWait],self);
  if dwState = SERVICE_START_PENDING then
    fStatusRec.dwControlsAccepted := 0 else
    fStatusRec.dwControlsAccepted := 31;
  fStatusRec.dwCurrentState  := dwState;
  fStatusRec.dwWin32ExitCode := dwExitCode;
  fStatusRec.dwWaitHint := dwWait;
  if (dwState = SERVICE_RUNNING) or (dwState = SERVICE_STOPPED) then
    fStatusRec.dwCheckPoint := 0 else
    inc(fStatusRec.dwCheckPoint);
  result := SetServiceStatus(FStatusHandle, fStatusRec);
  if not result then
    ServiceLog.Add.Log(sllLastError,'% ReportStatus(%,%,%)',
      [ServiceName,status,dwExitCode,dwWait],self);
end;

procedure TService.SetControlHandler(const Value: TServiceControlHandler);
begin
  fControlHandler := Value;
  if fJumper<>nil then
    VirtualFree(fJumper, 0, MEM_RELEASE);
end;

procedure TService.SetStatus(const Value: TServiceStatus);
begin
  fStatusRec := Value;
  if FStatusHandle <> 0 then
    SetServiceStatus(FStatusHandle, fStatusRec);
end;

procedure TService.Start;
begin
  with TServiceController.CreateOpenService('','',fSName,SERVICE_ALL_ACCESS) do
  try
    Start([]);
  finally
    Free;
  end;
end;

procedure TService.Stop;
begin
  with TServiceController.CreateOpenService('','',fSName,SERVICE_ALL_ACCESS) do
  try
    Stop;
  finally
    Free;
  end;
end;

function CurrentStateToServiceState(CurrentState: DWORD): TServiceState;
begin
  case CurrentState of
    SERVICE_STOPPED:          result := ssStopped;
    SERVICE_START_PENDING:    result := ssStarting;
    SERVICE_STOP_PENDING:     result := ssStopping;
    SERVICE_RUNNING:          result := ssRunning;
    SERVICE_CONTINUE_PENDING: result := ssResuming;
    SERVICE_PAUSE_PENDING:    result := ssPausing;
    SERVICE_PAUSED:           result := ssPaused;
    else result := ssNotInstalled; // e.g. SERVICE_CONTROL_SHUTDOWN
  end;
end;

function ServiceStateText(State: TServiceState): string;
var P: PShortString;
begin
  P := GetEnumName(TypeInfo(TServiceState),ord(State));
  result := string(copy(P^,3,length(P^)-2));
end;

function GetServicePid(const aServiceName: string): DWORD;
var
  ssp: TServiceStatusProcess;
  scm: THandle;
  svc: THandle;
  size: DWORD;
begin
  result := 0;
  scm := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if scm <> 0 then
  try
    svc := OpenService(scm, pointer(aServiceName), SERVICE_QUERY_STATUS);
    if svc <> 0 then
    try
      if QueryServiceStatusEx(svc, SC_STATUS_PROCESS_INFO, @ssp, SizeOf(TServiceStatusProcess), size) then
        result := ssp.dwProcessId
      else
        ServiceLog.Add.Log(sllLastError);
    finally
      CloseServiceHandle(svc);
    end;
  finally
    CloseServiceHandle(scm);
  end;
end;

function KillProcess(pid: DWORD; waitseconds: integer): boolean;
var
  ph: THandle;
begin
  ph := OpenProcess(PROCESS_TERMINATE or SYNCHRONIZE, false, pid);
  result := ph <> 0;
  if result then begin
    try
      result := TerminateProcess(ph, 0) and (WaitForSingleObject(ph, waitseconds * 1000) <> WAIT_TIMEOUT);
    finally
      CloseHandle(ph);
    end;
  end;
end;

{  function that a service process specifies as the entry point function
  of a particular service. The function can have any application-defined name
  - Args points to an array of pointers that point to null-terminated
    argument strings. The first argument in the array is the name of the service,
    and subsequent arguments are any strings passed to the service by the process
    that called the StartService function to start the service.  }
procedure ServiceProc(ArgCount: DWORD; Args: PPChar); stdcall;
var i: integer;
    Srv: TService;
begin
  i := FindServiceIndex(Args^);
  if i<0 then
    exit; // avoid any GPF
  Srv := Services.Items[i];
  for i := 1 to ArgCount-1 do begin
    Inc(Args);
    SetLength(Srv.FArgsList, length(Srv.FArgsList)+1);
    Srv.FArgsList[high(Srv.FArgsList)] := Args^;
  end;
  Srv.FStatusHandle := RegisterServiceCtrlHandler(
    pointer(Srv.fSName), @Srv.ControlHandler);
  if Srv.FStatusHandle = 0 then begin
    Srv.ReportStatus(SERVICE_STOPPED, GetLastError, 0);
    Exit;
  end;
  Srv.ReportStatus(SERVICE_START_PENDING, 0, 0);
  Srv.Execute;
end;

function ServicesRun: boolean;
var S: array of TServiceTableEntry;
    service: TService;
    i: integer;
    {$ifndef NOEXCEPTIONINTERCEPT}
    dummy: TSynLog;
    {$endif}
begin
  if (Services=nil) or (Services.Count=0) then begin
    result := false;
    exit;
  end;
  for i := 0 to Services.Count-1 do begin
    service := Services.List[i];
    if not assigned(service.fControlHandler) then
      raise EServiceException.CreateUTF8('%.ControlHandler=nil (ServiceName="%"): '+
       'use TServiceSingle or set a custom ControlHandler',[service,service.ServiceName]);
  end;
  SetLength(S,Services.Count+1); // +1 so that the latest entry is nil
  for i := 0 to Services.Count-1 do begin
    S[i].lpServiceName := pointer(TService(Services.List[i]).ServiceName);
    S[i].lpServiceProc := ServiceProc;
  end;
  {$ifndef NOEXCEPTIONINTERCEPT}
  dummy := GlobalCurrentHandleExceptionSynLog;
  GlobalCurrentHandleExceptionSynLog := nil; // don't log any EExternalException
  try
  {$endif}
    result := StartServiceCtrlDispatcher(pointer(S));
  {$ifndef NOEXCEPTIONINTERCEPT}
  finally
    GlobalCurrentHandleExceptionSynLog := dummy;
  end;
  {$endif}
end;

{ TServiceSingle }

procedure SingleServiceControlHandler(Opcode: LongWord); stdcall;
begin
  if ServiceSingle<>nil then
    ServiceSingle.DoCtrlHandle(Opcode);
end;

constructor TServiceSingle.Create(const aServiceName,
  aDisplayName: String);
begin
  inherited Create(aServiceName,aDisplayName);
  if ServiceSingle<>nil then
    raise EServiceException.Create('Only one TServiceSingle is allowed at a time');
  ServiceSingle := self;
  ControlHandler := SingleServiceControlHandler;
end;

destructor TServiceSingle.Destroy;
begin
  try
    inherited;
  finally
    ServiceSingle := nil;
  end;
end;

// redefined here so that we can share code with FPC and Delphi
function CreateProcessW(lpApplicationName: PWideChar; lpCommandLine: PWideChar;
  lpProcessAttributes, lpThreadAttributes: PSecurityAttributes;
  bInheritHandles: BOOL; dwCreationFlags: DWORD; lpEnvironment: Pointer;
  lpCurrentDirectory: PWideChar; const lpStartupInfo: TStartupInfo;
  out lpProcessInformation: TProcessInformation): BOOL; stdcall; external kernel32;
function GetExitCodeProcess(hProcess: THandle; out lpExitCode: DWORD): BOOL; stdcall;
  external kernel32;

function RunProcess(const path, arg1: TFileName; waitfor: boolean;
  const arg2,arg3,arg4,arg5,env: TFileName; envaddexisting: boolean): integer;
begin
  result := RunCommand(FormatString('"%" % % % % %', [path, arg1, arg2, arg3, arg4, arg5]),
    waitfor, env, envaddexisting);
end;

var
  EnvironmentCache: SynUnicode;

function RunCommand(const cmd: TFileName; waitfor: boolean;
  const env: TFileName; envaddexisting: boolean; parsed: PParseCommands): integer;
var
  startupinfo: TStartupInfo; // _STARTUPINFOW or _STARTUPINFOA is equal here
  processinfo: TProcessInformation;
  path: TFileName;
  wcmd, wenv, wpath: SynUnicode;
  e, p: PWideChar;
  exitcode: DWORD;
  i: integer;
begin
  // https://support.microsoft.com/en-us/help/175986/info-understanding-createprocess-and-command-line-arguments
  result := -1;
  if cmd = '' then
    exit;
  // CreateProcess can alter the strings -> use local SynUnicode
  StringToSynUnicode(cmd, wcmd);
  if cmd[1] = '"' then begin
    path := copy(cmd, 2, maxInt);
    i := Pos('"', path);
    if i = 0 then
      exit;
    SetLength(path, i - 1); // unquote "path" string
  end
  else begin
    i := Pos(' ', cmd);
    if i <= 1 then
      exit;
    path := copy(cmd, 1, i - 1);
  end;
  path := ExtractFilePath(path);
  if path = '' then
    path := ExeVersion.ProgramFilePath;
  StringToSynUnicode(path, wpath);
  if env <> '' then begin
    StringToSynUnicode(env, wenv);
    if envaddexisting then begin
      GlobalLock;
      if EnvironmentCache = '' then begin
        e := GetEnvironmentStringsW;
        p := e;
        while p^ <> #0 do
          inc(p, StrLenW(p) + 1); // go to name=value#0 pairs end
        SetString(EnvironmentCache, e, (PtrUInt(p) - PtrUInt(e)) shr 1);
        FreeEnvironmentStringsW(e);
      end;
      wenv := EnvironmentCache + wenv;
      GlobalUnLock;
    end;
  end;
  FillCharFast(startupinfo, SizeOf(startupinfo), 0);
  startupinfo.cb := SizeOf(startupinfo);
  FillCharFast(processinfo, SizeOf(processinfo), 0);
  // https://docs.microsoft.com/pl-pl/windows/desktop/ProcThread/process-creation-flags
  if CreateProcessW(nil, pointer(wcmd), nil, nil, false, CREATE_UNICODE_ENVIRONMENT or
      CREATE_DEFAULT_ERROR_MODE or DETACHED_PROCESS or CREATE_NEW_PROCESS_GROUP,
      pointer(wenv), pointer(wpath), startupinfo, processinfo) then begin
    if waitfor then
      if WaitForSingleObject(processinfo.hProcess,INFINITE) = WAIT_FAILED then
        result := -GetLastError
      else if not GetExitCodeProcess(processinfo.hProcess, exitcode) then
        result := -GetLastError
      else
        result := exitcode
    else
      result := 0;
    CloseHandle(processinfo.hProcess);
    CloseHandle(processinfo.hThread);
  end else
    result := -GetLastError;
end;

{$else} // Linux/POSIX signal interception

var
  SynDaemonIntercepted: boolean;
  SynDaemonInterceptLog: TSynLogClass;

{$ifdef FPC}
procedure DoShutDown(Sig: Longint; Info: PSigInfo; Context: PSigContext); cdecl;
var level: TSynLogInfo;
    log: TSynLog;
    si_code: integer;
    text: TShort4;
begin // code below has no memory (re)allocation
  if SynDaemonInterceptLog <> nil then begin
    log := SynDaemonInterceptLog.Add;
    case Sig of
      SIGQUIT: text := 'QUIT';
      SIGTERM: text := 'TERM';
      SIGINT:  text := 'INT';
      SIGABRT: text := 'ABRT';
      else text := UInt3DigitsToShort(Sig);
    end;
    if Sig = SIGTERM then // polite quit
      level := sllInfo else
      level := sllExceptionOS;
    if Info=nil then
      si_code := 0 else
      si_code := Info^.si_code;
    log.Writer.CustomOptions := log.Writer.CustomOptions + [twoFlushToStreamNoAutoResize];
    log.Log(level, 'SynDaemonIntercepted received SIG%=% si_code=%', [text, Sig, si_code]);
    log.Flush({flushtodisk=}Sig <> SIGTERM); // ensure all log is safely written
  end;
  SynDaemonTerminated := Sig;
end;
{$else}
procedure DoShutDown(Sig: integer); cdecl;
begin
  SynDaemonTerminated := Sig;
end;
{$endif FPC}

procedure SynDaemonIntercept(log: TSynLog);
var
  saOld, saNew: {$ifdef FPC}SigactionRec{$else}TSigAction{$endif};
begin // note: SIGFPE/SIGSEGV/SIGBUS/SIGILL are handled by the RTL
  if SynDaemonIntercepted then
    exit;
  SynDaemonIntercepted := true;
  SynDaemonInterceptLog := log.LogClass;
  FillCharFast(saNew, SizeOf(saNew), 0);
  {$ifdef FPC}
  saNew.sa_handler := @DoShutDown;
  fpSigaction(SIGQUIT, @saNew, @saOld);
  fpSigaction(SIGTERM, @saNew, @saOld);
  fpSigaction(SIGINT, @saNew, @saOld);
  fpSigaction(SIGABRT, @saNew, @saOld);
  {$else} // Kylix
  saNew.__sigaction_handler := @DoShutDown;
  sigaction(SIGQUIT, @saNew, @saOld);
  sigaction(SIGTERM, @saNew, @saOld);
  sigaction(SIGINT, @saNew, @saOld);
  sigaction(SIGABRT, @saNew, @saOld);
  {$endif}
end;

function RunUntilSigTerminatedPidFile: TFileName;
begin
  result := FormatString('%.%.pid', [ExeVersion.ProgramFilePath, ExeVersion.ProgramName]);
end;

function RunUntilSigTerminatedForKill(waitseconds: integer): boolean;
var
  pid: PtrInt;
  pidfilename: TFileName;
  tix: Int64;
begin
  result := false;
  pidfilename := RunUntilSigTerminatedPidFile;
  pid := GetInteger(pointer(StringFromFile(pidfilename)));
  if pid <= 0 then
    exit;
  {$ifdef FPC}
  if fpkill(pid, SIGTERM) <> 0 then // polite quit
    if fpgeterrno<>ESysESRCH then
  {$else} // Kylix
  if kill(pid, SIGTERM) <> 0 then
    if errno<>ESRCH then
  {$endif}
      exit else // no such process -> try to delete the .pid file
      if DeleteFile(pidfilename) then begin
        result := true; // process crashed or hard reboot -> nothing to kill
        exit;
      end;
  if waitseconds <= 0 then begin
    result := true;
    exit;
  end;
  tix := GetTickCount64 + waitseconds * 1000;
  repeat // RunUntilSigTerminated() below should delete the .pid file
    sleep(100);
    if not FileExists(pidfilename) then
      result := true;
  until result or (GetTickCount64 > tix);
  if not result then
    {$ifdef FPC}fpkill{$else}kill{$endif}(pid, SIGKILL); // finesse
end;

procedure CleanAfterFork;
begin
  {$ifdef FPC}fpUMask{$else}umask{$endif}(0); // reset file mask
  chdir('/'); // avoid locking current directory
  Close(input);
  AssignFile(input, '/dev/null');
  ReWrite(input);
  Close(output);
  AssignFile(output, '/dev/null');
  ReWrite(output);
  {$ifdef FPC}Close{$else}__close{$endif}(stderr);
end;

procedure RunUntilSigTerminated(daemon: TObject; dofork: boolean;
  const start, stop: TThreadMethod; log: TSynLog; const servicename: string);
var
  pid, sid: {$ifdef FPC}TPID{$else}pid_t{$endif};
  pidfilename: TFileName;
const
  TXT: array[boolean] of string[4] = ('run', 'fork');
begin
  SynDaemonIntercept(log);
  if dofork then begin
    pidfilename := RunUntilSigTerminatedPidFile;
    pid := GetInteger(pointer(StringFromFile(pidfilename)));
    if pid > 0 then
      if ({$ifdef FPC}fpkill{$else}kill{$endif}(pid, 0) = 0) or not DeleteFile(pidfilename) then
        raise EServiceException.CreateUTF8('%.CommandLine Fork failed: % is already forked as pid=%',
          [daemon, ExeVersion.ProgramName, pid]);
    pid := {$ifdef FPC}fpFork{$else}fork{$endif};
    if pid < 0 then
      raise EServiceException.CreateUTF8('%.CommandLine Fork failed', [daemon]);
    if pid > 0 then  // main program - just terminate
      exit;
    // clean forked instance
    sid := {$ifdef FPC}fpSetSID{$else}setsid{$endif};
    if sid < 0 then // new session (process group) created?
      raise EServiceException.CreateUTF8('%.CommandLine SetSID failed', [daemon]);
    CleanAfterFork;
    // create local .[ExeVersion.ProgramName].pid file
    pid := {$ifdef FPC}fpgetpid{$else}getpid{$endif};
    FileFromString(Int64ToUtf8(pid), pidfilename);
  end;
  try
    if log <> nil then
      log.Log(sllNewRun, 'Start % /% %', [serviceName, TXT[dofork],
        ExeVersion.Version.DetailedOrVoid], daemon);
    start;
    while SynDaemonTerminated = 0 do
      {$ifdef FPC}fpPause{$else}pause{$endif};
  finally
    if log <> nil then
      log.Log(sllNewRun, 'Stop /% from Sig=%', [TXT[dofork], SynDaemonTerminated], daemon);
    try
      stop;
    finally
      if dofork and (pidfilename <> '') then begin
        DeleteFile(pidfilename);
        if log <> nil then
          log.Log(sllTrace, 'RunUntilSigTerminated: deleted file %', [pidfilename]);
      end;
    end;
  end;
end;

{$ifndef FPC} // Kylix doesn't have a proper WaitProcess
function WaitProcess(pid: pid_t): pid_t;
var r: pid_t;
    s: integer;
begin
  repeat
    r := WaitPid(pid, @s, 0);
    if (r = -1) and (errno = EINTR) then
      r := 0;
  until r <> 0;
  if r < 0 then // WaitPid() failed
    result := -1 else
    if WIFEXITED(s) then // returns the exit status code
      result := WEXITSTATUS(s) else
      result := -abs(s); // ensure returns a negative value for other errors
end;
{$endif FPC}

function RunInternal(args: PPAnsiChar; waitfor: boolean;
  const env: TFileName; envaddexisting: boolean): integer;
var
  pid: {$ifdef FPC}TPID{$else}pid_t{$endif};
  e: array[0..511] of PAnsiChar; // max 512 environment variables
  envpp: PPAnsiChar;
  P: PAnsiChar;
  n: PtrInt;
begin
  {$ifdef FPC}
  {$if (defined(BSD) or defined(SUNOS)) and defined(FPC_USE_LIBC)}
  pid := FpvFork;
  {$else}
  pid := FpFork;
  {$ifend}
  {$else}
  pid := fork; // Kylix
  {$endif FPC}
  if pid < 0 then begin
    result := -1; // fork failed
    exit;
  end;
  if pid = 0 then begin // we are in child process -> switch to new executable
    if not waitfor then
      CleanAfterFork; // don't share the same console
    envpp := envp;
    if env <> '' then begin
      n := 0;
      result := {$ifdef FPC}-ESysE2BIG{$else}-7{$endif};
      if envaddexisting and (envpp <> nil) then begin
        while envpp^ <> nil do begin
          if PosChar(envpp^, #10) = nil then begin // filter simple variables
            if n = high(e) - 1 then
              exit;
            e[n] := envpp^;
            inc(n);
          end;
          inc(envpp);
        end;
      end;
      P := pointer(env); // env follows Windows layout 'n1=v1'#0'n2=v2'#0#0
      while P^ <> #0 do begin
        if n = high(e) - 1 then
          exit;
        e[n] := P; // makes POSIX compatible
        inc(n);
        inc(P, StrLen(P) + 1);
      end;
      e[n] := nil; // end with null
      envpp := @e;
    end;
    {$ifdef FPC}
    FpExecve(args^, args, envpp);
    FpExit(127);
    {$else}
    execve(args^, args, envpp);
    _exit(127);
    {$endif}
  end;
  if waitfor then begin
    result := WaitProcess(pid);
    if result = 127 then
      result := -result; // execv() failed in child process
  end else
    result := 0; // fork success (don't wait for the child process to fail)
end;

function RunProcess(const path, arg1: TFileName; waitfor: boolean;
  const arg2,arg3,arg4,arg5,env: TFileName; envaddexisting: boolean): integer;
var
  a: array[0..6] of PAnsiChar;   // assume no UNICODE on BSD, i.e. as TFileName
begin
  a[0] := pointer(path);
  a[1] := pointer(arg1);
  a[2] := pointer(arg2);
  a[3] := pointer(arg3);
  a[4] := pointer(arg4);
  a[5] := pointer(arg5);
  a[6] := nil; // end with null
  result := RunInternal(@a, waitfor, env, envaddexisting);
end;

function RunCommand(const cmd: TFileName; waitfor: boolean;
  const env: TFileName; envaddexisting: boolean;
  parsed: PParseCommands): integer;
var
  temp: RawUTF8;
  err: TParseCommands;
  a: TParseCommandsArgs;
begin
  err := ParseCommandArgs(cmd, @a, nil, @temp);
  if parsed <> nil then
    parsed^ := err;
  if err = [] then
    // no need to spawn the shell for simple commands
    result := RunInternal(a, waitfor, env, envaddexisting)
  else if err * PARSECOMMAND_ERROR <> [] then
    // no system call for clearly invalid command line
    result := {$ifdef FPCLINUXNOTBSD}-ESysELIBBAD{$else}-80{$endif}
  else begin // execute complex commands via the shell
    a[0] := '/bin/sh';
    a[1] := '-c';
    a[2] := pointer(cmd);
    a[3] := nil;
    result := RunInternal(@a, waitfor, env, envaddexisting);
  end;
end;

{$endif MSWINDOWS}

function ToText(cmd: TParseCommands): shortstring;
begin
  if cmd = [] then
    result[0] := #0
  else
    GetSetNameShort(TypeInfo(TParseCommands), cmd, result, {trim=}true);
end;

function ParseCommandArgs(const cmd: RawUTF8; argv: PParseCommandsArgs;
  argc: PInteger; temp: PRawUTF8; posix: boolean): TParseCommands;
var
  n: PtrInt;
  state: set of (sWhite, sInArg, sInSQ, sInDQ, sSpecial, sBslash);
  c: AnsiChar;
  D, P: PAnsiChar;
begin
  result := [pcInvalidCommand];
  if argv <> nil then
    argv[0] := nil;
  if argc <> nil then
    argc^ := 0;
  if cmd = '' then
    exit;
  if argv = nil then
    D := nil
  else begin
    if temp = nil then
      exit;
    SetLength(temp^, length(cmd));
    D := pointer(temp^);
  end;
  state := [];
  n := 0;
  P := pointer(cmd);
  repeat
    c := P^;
    if D <> nil then
      D^ := c;
    inc(P);
    case c of
      #0: begin
        if sInSQ in state then
          include(result, pcUnbalancedSingleQuote);
        if sInDQ in state then
          include(result, pcUnbalancedDoubleQuote);
        exclude(result, pcInvalidCommand);
        if argv <> nil then
          argv[n] := nil;
        if argc <> nil then
          argc^ := n;
        exit;
      end;
      #1 .. ' ': begin
       if state = [sInArg] then begin
         state := [];
         if D <> nil then begin
           D^ := #0;
           inc(D);
         end;
         continue;
       end;
       if state * [sInSQ, sInDQ] = [] then
         continue;
      end;
      '\':
        if posix and (state * [sInSQ, sBslash] = []) then
          if sInDQ in state then begin
            case P^ of
              '"', '\', '$', '`': begin
                include(state, sBslash);
                continue;
              end;
            end;
          end else if P^ = #0 then begin
            include(result, pcHasEndingBackSlash);
            exit;
          end else begin
            if D <> nil then
              D^ := P^;
            inc(P);
          end;
      '^':
        if not posix and (state * [sInSQ, sInDQ, sBslash] = []) then
          if PWord(P)^ = $0a0d then begin
            inc(P, 2);
            continue;
          end
          else if P^ = #0 then begin
            include(result, pcHasEndingBackSlash);
            exit;
          end else begin
            if D <> nil then
              D^ := P^;
            inc(P);
          end;
      '''':
        if posix and not(sInDQ in state) then
          if sInSQ in state then begin
            exclude(state, sInSQ);
            continue;
          end else if state = [] then begin
            if argv <> nil then begin
              argv[n] := D;
              inc(n);
              if n = high(argv^) then
                exit;
            end;
            state := [sInSQ, sInArg];
            continue;
          end else if state = [sInArg] then begin
            state := [sInSQ, sInArg];
            continue;
          end;
      '"':
        if not(sInSQ in state) then
          if sInDQ in state then begin
            exclude(state, sInDQ);
            continue;
          end else if state = [] then begin
            if argv <> nil then begin
              argv[n] := D;
              inc(n);
              if n = high(argv^) then
                exit;
            end;
            state := [sInDQ, sInArg];
            continue;
          end else if state = [sInArg] then begin
            state := [sInDQ, sInArg];
            continue;
          end;
      '|', '<', '>':
        if state * [sInSQ, sInDQ] = [] then
          include(result, pcHasRedirection);
      '&', ';':
        if posix and (state * [sInSQ, sInDQ] = []) then begin
          include(state, sSpecial);
          include(result, pcHasJobControl);
        end;
      '`':
        if posix and (state * [sInSQ, sBslash] = []) then
           include(result, pcHasSubCommand);
      '(', ')':
        if posix and (state * [sInSQ, sInDQ] = []) then
          include(result, pcHasParenthesis);
      '$':
        if posix and (state * [sInSQ, sBslash] = []) then
          if p^ = '(' then
            include(result, pcHasSubCommand)
          else
            include(result, pcHasShellVariable);
      '*', '?':
        if posix and (state * [sInSQ, sInDQ] = []) then
          include(result, pcHasWildcard);
    end;
    exclude(state, sBslash);
    if state = [] then begin
      if argv <> nil then begin
        argv[n] := D;
        inc(n);
        if n = high(argv^) then
          exit;
      end;
      state := [sInArg];
    end;
    if D <> nil then
      inc(D);
  until false;
end;


{ *** cross-plaform high-level services }

{ TSynDaemonSettings }

constructor TSynDaemonSettings.Create;
begin
  inherited Create;
  fLog := LOG_STACKTRACE + [sllNewRun];
  fLogRotateFileCount := 2;
  fServiceName := UTF8ToString(ExeVersion.ProgramName);
  fServiceDisplayName := fServiceName;
end;

function TSynDaemonSettings.ServiceDescription: string;
var
  versionnumber: string;
begin
  result := ServiceDisplayName;
  with ExeVersion.Version do begin
    versionnumber := DetailedOrVoid;
    if versionnumber <> '' then
      result := result + ' ' + versionnumber;
    if CompanyName <> '' then
      result := FormatString('% - (c)% %', [result, BuildYear, CompanyName]);
  end;
end;

procedure TSynDaemonSettings.SetLog(aLogClass: TSynLogClass);
begin
  if (self <> nil) and (Log <> []) and (aLogClass <> nil) then
    with aLogClass.Family do begin
      DestinationPath := LogPath;
      PerThreadLog := ptIdentifiedInOnFile; // ease multi-threaded server debug
      RotateFileCount := LogRotateFileCount;
      if RotateFileCount > 0 then begin
        RotateFileSizeKB := 20 * 1024; // rotate by 20 MB logs
        FileExistsAction := acAppend;  // as expected in rotation mode
      end
      else
        HighResolutionTimestamp := true;
      Level := Log;
      fLogClass := aLogClass;
    end;
end;


{ TSynDaemon }

constructor TSynDaemon.Create(aSettingsClass: TSynDaemonSettingsClass;
  const aWorkFolder, aSettingsFolder, aLogFolder, aSettingsExt, aSettingsName: TFileName);
var
  fn: TFileName;
begin
  inherited Create;
  if aWorkFolder = '' then
    fWorkFolderName := ExeVersion.ProgramFilePath
  else
    fWorkFolderName := EnsureDirectoryExists(aWorkFolder, true);
  if aSettingsClass = nil then
    aSettingsClass := TSynDaemonSettings;
  fSettings := aSettingsClass.Create;
  fn := aSettingsFolder;
  if fn = '' then
    fn := {$ifdef MSWINDOWS}fWorkFolderName{$else}'/etc/'{$endif};
  fn :=  EnsureDirectoryExists(fn);
  if aSettingsName = '' then
    fn := fn + UTF8ToString(ExeVersion.ProgramName)
  else
    fn := fn + aSettingsName;
  fSettings.LoadFromFile(fn + aSettingsExt);
  if fSettings.LogPath = '' then
    if aLogFolder = '' then
      fSettings.LogPath := {$ifdef MSWINDOWS}fWorkFolderName{$else}GetSystemPath(spLog){$endif}
    else
      fSettings.LogPath := EnsureDirectoryExists(aLogFolder);
end;

destructor TSynDaemon.Destroy;
begin
  if fSettings <> nil then
    fSettings.SaveIfNeeded;
  Stop;
  inherited Destroy;
  FreeAndNil(fSettings);
end;

{$ifdef MSWINDOWS}
procedure TSynDaemon.DoStart(Sender: TService);
begin
  Start;
end;

procedure TSynDaemon.DoStop(Sender: TService);
begin
  Stop;
end;
{$endif MSWINDOWS}

function TSynDaemon.CustomCommandLineSyntax: string;
begin
  result := '';
end;

{$I-}

type
  TExecuteCommandLineCmd = (
     cNone, cVersion, cVerbose, cStart, cStop, cState, cSilentKill,
     cHelp, cInstall, cRun, cFork, cUninstall, cConsole, cKill);

procedure TSynDaemon.CommandLine(aAutoStart: boolean);
const CMD_CHR: array[cHelp .. cKill] of AnsiChar = ('H', 'I', 'R', 'F', 'U', 'C', 'K');
var
  cmd, c: TExecuteCommandLineCmd;
  p: PUTF8Char;
  ch: AnsiChar;
  param: RawUTF8;
  exe: RawByteString;
  log: TSynLog;
  {$ifdef MSWINDOWS}
  service: TServiceSingle;
  ctrl: TServiceController;
  {$endif MSWINDOWS}

  procedure WriteCopyright;
  var
    msg, name, copyright: string;
    i: integer;
  begin
    msg := fSettings.ServiceDescription;
    i := Pos(' - ', msg);
    if i = 0 then
      name := msg
    else begin
      name := copy(msg, 1, i - 1);
      copyright := copy(msg, i + 3, 1000);
    end;
    TextColor(ccLightGreen);
    writeln(' ', name);
    writeln(StringOfChar('-', length(name) + 2));
    TextColor(ccGreen);
    if copyright <> '' then
      writeln(' ', copyright);
    writeln;
    TextColor(ccLightGray);
  end;

  procedure Syntax;
  var
    spaces, custom: string;
  begin
    WriteCopyright;
    writeln('Try with one of the switches:');
    spaces := StringOfChar(' ', length(ExeVersion.ProgramName) + 4);
    {$ifdef MSWINDOWS}
    writeln('   ', ExeVersion.ProgramName, ' /console -c /verbose /help -h /version');
    writeln(spaces, '/install /uninstall /start /stop /state');
    {$else}
    writeln(' ./', ExeVersion.ProgramName, ' --console -c --verbose --help -h --version');
    writeln(spaces, '--run -r --fork -f --kill -k');
    {$endif MSWINDOWS}
    custom := CustomCommandLineSyntax;
    if custom <> '' then
      writeln(spaces, custom);
  end;

  function cmdText: RawUTF8;
  begin
    result := GetEnumNameTrimed(TypeInfo(TExecuteCommandLineCmd), cmd);
  end;

  procedure Show(Success: Boolean);
  var
    msg: RawUTF8;
    error: integer;
  begin
    WriteCopyright;
    if Success then begin
      msg := 'Successfully executed';
      TextColor(ccWhite);
    end
    else begin
      error := GetLastError;
      msg := FormatUTF8('Error % [%] occured with',
        [error, StringToUTF8(SysErrorMessage(error))]);
      TextColor(ccLightRed);
      ExitCode := 1; // notify error to caller batch
    end;
    msg := FormatUTF8('% [%] (%) on Service ''%''',
      [msg, param, cmdText, fSettings.ServiceName]);
    writeln(UTF8ToConsole(msg));
    TextColor(ccLightGray);
    log.Log(sllDebug, 'CommandLine: %', [msg], self);
  end;

begin
  if (self = nil) or (fSettings = nil) then
    exit;
  log := nil;
  param := trim(StringToUTF8(paramstr(1)));
  cmd := cNone;
  if (param <> '') and (param[1] in ['/', '-']) then begin
    p := @param[2];
    if p^ = '-' then
      inc(p); // allow e.g. --fork switch (idem to /f -f /fork -fork)
    ch := NormToUpper[p^];
    for c := low(CMD_CHR) to high(CMD_CHR) do
      if CMD_CHR[c] = ch then begin
        cmd := c;
        break;
      end;
    if cmd = cNone then
      byte(cmd) := ord(cVersion) +
        IdemPCharArray(p, ['VERS', 'VERB', 'START', 'STOP', 'STAT', 'SILENTK']);
    end;
  try
    case cmd of
    cHelp:
      Syntax;
    cVersion: begin
      WriteCopyright;
      exe := StringFromFile(ExeVersion.ProgramFileName);
      writeln(' ', fSettings.ServiceName,
        #13#10' Size: ', length(exe), ' bytes (', KB(exe), ')' +
        #13#10' Build date: ', ExeVersion.Version.BuildDateTimeString,
        #13#10' MD5: ', MD5(exe),
        #13#10' SHA256: ', SHA256(exe));
      if ExeVersion.Version.Version32 <> 0 then
        writeln(' Version: ', ExeVersion.Version.Detailed);
    end;
    cConsole, cVerbose: begin
        WriteCopyright;
        writeln('Launched in ', cmdText, ' mode'#10);
        TextColor(ccLightGray);
        log := fSettings.fLogClass.Add;
        if (cmd = cVerbose) and (log <> nil) then begin
          log.Family.Level := LOG_VERBOSE;
          log.Family.EchoToConsole := LOG_VERBOSE;
        end;
        try
          log.Log(sllNewRun, 'Start % /% %', [fSettings.ServiceName,cmdText,
            ExeVersion.Version.DetailedOrVoid], self);
          fConsoleMode := true;
          Start;
          writeln('Press [Enter] to quit');
          ioresult;
          readln;
          writeln('Shutting down server');
        finally
          ioresult;
          log.Log(sllNewRun, 'Stop /%', [cmdText], self);
          Stop;
        end;
    end;
    {$ifdef MSWINDOWS} // implement the daemon as a Windows Service
    else if fSettings.ServiceName = '' then
      if cmd = cNone then
        Syntax
      else begin
        TextColor(ccLightRed);
        writeln('No ServiceName specified - please fix the settings');
      end
    else
    case cmd of
      cNone:
        if param = '' then begin // executed as a background service
          service := TServiceSingle.Create(
            fSettings.ServiceName, fSettings.ServiceDisplayName);
          try
            service.OnStart := DoStart;
            service.OnStop := DoStop;
            service.OnShutdown := DoStop; // sometimes, is called without Stop
            if ServicesRun then // blocking until service shutdown
              Show(true)
            else if GetLastError = 1063 then
              Syntax
            else
              Show(false);
          finally
            service.Free;
          end;
        end
        else
          Syntax;
      cInstall:
        with fSettings do
          Show(TServiceController.Install(ServiceName, ServiceDisplayName,
            ServiceDescription, aAutoStart, '', ServiceDependencies) <> ssNotInstalled);
      cStart, cStop, cUninstall, cState: begin
        ctrl := TServiceController.CreateOpenService('', '', fSettings.ServiceName);
        try
          case cmd of
          cStart:
            Show(ctrl.Start([]));
          cStop:
            Show(ctrl.Stop);
          cUninstall:
            begin
              ctrl.Stop;
              Show(ctrl.Delete);
            end;
          cState:
            writeln(fSettings.ServiceName, ' State=', ServiceStateText(ctrl.State));
          end;
        finally
          ctrl.Free;
        end;
      end;
      else
        Syntax;
    end;
    {$else}
    cRun, cFork:
      RunUntilSigTerminated(self,(cmd=cFork),Start,Stop,fSettings.fLogClass.Add,fSettings.ServiceName);
    cKill, cSilentKill:
      if RunUntilSigTerminatedForKill then begin
        if cmd <> cSilentKill then
          writeln('Forked process ', ExeVersion.ProgramName, ' killed successfully');
      end
      else
        raise EServiceException.Create('No forked process found to be killed');
    else
      Syntax;
    {$endif MSWINDOWS}
    end;
  except
    on E: Exception do begin
      if cmd <> cSilentKill then
        ConsoleShowFatalException(E, true);
      ExitCode := 1; // indicates error
    end;
  end;
  if cmd <> cSilentKill then
    TextColor(ccLightGray);
  ioresult;
end;

{$I+}

{$ifdef MSWINDOWS}
const
  SE_CREATE_TOKEN_NAME           = 'SeCreateTokenPrivilege';
  SE_ASSIGNPRIMARYTOKEN_NAME     = 'SeAssignPrimaryTokenPrivilege';
  SE_LOCK_MEMORY_NAME            = 'SeLockMemoryPrivilege';
  SE_INCREASE_QUOTA_NAME         = 'SeIncreaseQuotaPrivilege';
  SE_UNSOLICITED_INPUT_NAME      = 'SeUnsolicitedInputPrivilege';
  SE_MACHINE_ACCOUNT_NAME        = 'SeMachineAccountPrivilege';
  SE_TCB_NAME                    = 'SeTcbPrivilege';
  SE_SECURITY_NAME               = 'SeSecurityPrivilege';
  SE_TAKE_OWNERSHIP_NAME         = 'SeTakeOwnershipPrivilege';
  SE_LOAD_DRIVER_NAME            = 'SeLoadDriverPrivilege';
  SE_SYSTEM_PROFILE_NAME         = 'SeSystemProfilePrivilege';
  SE_SYSTEMTIME_NAME             = 'SeSystemtimePrivilege';
  SE_PROF_SINGLE_PROCESS_NAME    = 'SeProfileSingleProcessPrivilege';
  SE_INC_BASE_PRIORITY_NAME      = 'SeIncreaseBasePriorityPrivilege';
  SE_CREATE_PAGEFILE_NAME        = 'SeCreatePagefilePrivilege';
  SE_CREATE_PERMANENT_NAME       = 'SeCreatePermanentPrivilege';
  SE_BACKUP_NAME                 = 'SeBackupPrivilege';
  SE_RESTORE_NAME                = 'SeRestorePrivilege';
  SE_SHUTDOWN_NAME               = 'SeShutdownPrivilege';
  SE_DEBUG_NAME                  = 'SeDebugPrivilege';
  SE_AUDIT_NAME                  = 'SeAuditPrivilege';
  SE_SYSTEM_ENVIRONMENT_NAME     = 'SeSystemEnvironmentPrivilege';
  SE_CHANGE_NOTIFY_NAME          = 'SeChangeNotifyPrivilege';
  SE_REMOTE_SHUTDOWN_NAME        = 'SeRemoteShutdownPrivilege';
  SE_UNDOCK_NAME                 = 'SeUndockPrivilege';
  SE_SYNC_AGENT_NAME             = 'SeSyncAgentPrivilege';
  SE_ENABLE_DELEGATION_NAME      = 'SeEnableDelegationPrivilege';
  SE_MANAGE_VOLUME_NAME          = 'SeManageVolumePrivilege';
  SE_IMPERSONATE_NAME            = 'SeImpersonatePrivilege';
  SE_CREATE_GLOBAL_NAME          = 'SeCreateGlobalPrivilege';
  SE_TRUSTED_CREDMAN_ACCESS_NAME = 'SeTrustedCredManAccessPrivilege';
  SE_RELABEL_NAME                = 'SeRelabelPrivilege';
  SE_INC_WORKING_SET_NAME        = 'SeIncreaseWorkingSetPrivilege';
  SE_TIME_ZONE_NAME              = 'SeTimeZonePrivilege';
  SE_CREATE_SYMBOLIC_LINK_NAME   = 'SeCreateSymbolicLinkPrivilege';

  MAX_SE_NAME_LENGTH = 31;

  WinSystemPrivilegeToSE_NAME: array[TWinSystemPrivilege] of string = (
    SE_CREATE_TOKEN_NAME,
    SE_ASSIGNPRIMARYTOKEN_NAME,
    SE_LOCK_MEMORY_NAME,
    SE_INCREASE_QUOTA_NAME,
    SE_UNSOLICITED_INPUT_NAME,
    SE_MACHINE_ACCOUNT_NAME,
    SE_TCB_NAME,
    SE_SECURITY_NAME,
    SE_TAKE_OWNERSHIP_NAME,
    SE_LOAD_DRIVER_NAME,
    SE_SYSTEM_PROFILE_NAME,
    SE_SYSTEMTIME_NAME,
    SE_PROF_SINGLE_PROCESS_NAME,
    SE_INC_BASE_PRIORITY_NAME,
    SE_CREATE_PAGEFILE_NAME,
    SE_CREATE_PERMANENT_NAME,
    SE_BACKUP_NAME,
    SE_RESTORE_NAME,
    SE_SHUTDOWN_NAME,
    SE_DEBUG_NAME,
    SE_AUDIT_NAME,
    SE_SYSTEM_ENVIRONMENT_NAME,
    SE_CHANGE_NOTIFY_NAME,
    SE_REMOTE_SHUTDOWN_NAME,
    SE_UNDOCK_NAME,
    SE_SYNC_AGENT_NAME,
    SE_ENABLE_DELEGATION_NAME,
    SE_MANAGE_VOLUME_NAME,
    SE_IMPERSONATE_NAME,
    SE_CREATE_GLOBAL_NAME,
    SE_TRUSTED_CREDMAN_ACCESS_NAME,
    SE_RELABEL_NAME,
    SE_INC_WORKING_SET_NAME,
    SE_TIME_ZONE_NAME,
    SE_CREATE_SYMBOLIC_LINK_NAME
  );

type
  PTOKEN_PRIVILEGES = ^TOKEN_PRIVILEGES;
  TOKEN_PRIVILEGES = packed record
    PrivilegeCount : DWORD;
    Privileges : array[0..0] of LUID_AND_ATTRIBUTES;
  end;

function OpenProcessToken(ProcessHandle: THandle; DesiredAccess: DWORD;
  var TokenHandle: THandle): BOOL; stdcall; external advapi32 name 'OpenProcessToken';
function LookupPrivilegeValue(lpSystemName, lpName: PChar;
  var lpLuid: TLargeInteger): BOOL; stdcall; external advapi32
  name {$ifdef UNICODE}'LookupPrivilegeValueW'{$else}'LookupPrivilegeValueA'{$endif};
function LookupPrivilegeNameA(lpSystemName: LPCSTR; var lpLuid: TLargeInteger;
  lpName: LPCSTR; var cbName: DWORD): BOOL; stdcall; external advapi32 name 'LookupPrivilegeNameA';
function AdjustTokenPrivileges(TokenHandle: THandle; DisableAllPrivileges: BOOL;
  const NewState: TOKEN_PRIVILEGES; BufferLength: DWORD;
  PreviousState: PTokenPrivileges; ReturnLength: PDWORD): BOOL; stdcall; external advapi32
  name 'AdjustTokenPrivileges';

{ TSynWindowsPrivileges }

procedure TSynWindowsPrivileges.Init(aTokenPrivilege: TPrivilegeTokenType);
var
  access: Cardinal;
begin
  Token := 0;
  fAvailable := [];
  fEnabled := [];
  fDefEnabled := [];
  access := TOKEN_QUERY or TOKEN_ADJUST_PRIVILEGES;
  if aTokenPrivilege = pttProcess then begin
    if not OpenProcessToken(GetCurrentProcess, access, Token) then
      raise ESynException.Create('TSynWindowsPrivileges cannot open process token');
  end
  else if not OpenThreadToken(GetCurrentThread, access, false, Token) then
    if GetLastError = ERROR_NO_TOKEN then begin
      if not ImpersonateSelf(SecurityImpersonation) or
       not OpenThreadToken(GetCurrentThread, access, false, Token) then
        raise ESynException.Create('TSynWindowsPrivileges cannot open thread token');
    end else
      raise ESynException.Create('TSynWindowsPrivileges cannot open thread token');
  LoadPrivileges;
end;

procedure TSynWindowsPrivileges.Done(aRestoreInitiallyEnabled: boolean = true);
var
  i: TWinSystemPrivilege;
  new: TWinSystemPrivileges;
begin
  if aRestoreInitiallyEnabled then begin
    new := fEnabled-fDefEnabled;
    for i := low(TWinSystemPrivilege) to high(TWinSystemPrivilege) do
      if i in new then
        Disable(i);
  end;
  CloseHandle(Token);
end;

function TSynWindowsPrivileges.Enable(aPrivilege: TWinSystemPrivilege): boolean;
begin
  result := aPrivilege in fEnabled;
  if result or not (aPrivilege in fAvailable) or not SetPrivilege(Pointer(WinSystemPrivilegeToSE_NAME[aPrivilege]), true) then
    exit;
  Include(fEnabled, aPrivilege);
  result := true;
end;

function TSynWindowsPrivileges.Disable(aPrivilege: TWinSystemPrivilege
  ): boolean;
begin
  result := not (aPrivilege in fEnabled);
  if result or not (aPrivilege in fAvailable) or not SetPrivilege(Pointer(WinSystemPrivilegeToSE_NAME[aPrivilege]), false) then
    exit;
  Exclude(fEnabled, aPrivilege);
  result := true;
end;

procedure TSynWindowsPrivileges.LoadPrivileges;
const
  UPCASE_SE_NAMES: array[TWinSystemPrivilege] of PAnsiChar = (
    'SECREATETOKENPRIVILEGE','SEASSIGNPRIMARYTOKENPRIVILEGE','SELOCKMEMORYPRIVILEGE',
    'SEINCREASEQUOTAPRIVILEGE','SEUNSOLICITEDINPUTPRIVILEGE','SEMACHINEACCOUNTPRIVILEGE',
    'SETCBPRIVILEGE','SESECURITYPRIVILEGE','SETAKEOWNERSHIPPRIVILEGE',
    'SELOADDRIVERPRIVILEGE','SESYSTEMPROFILEPRIVILEGE','SESYSTEMTIMEPRIVILEGE',
    'SEPROFILESINGLEPROCESSPRIVILEGE','SEINCREASEBASEPRIORITYPRIVILEGE',
    'SECREATEPAGEFILEPRIVILEGE','SECREATEPERMANENTPRIVILEGE','SEBACKUPPRIVILEGE',
    'SERESTOREPRIVILEGE','SESHUTDOWNPRIVILEGE','SEDEBUGPRIVILEGE','SEAUDITPRIVILEGE',
    'SESYSTEMENVIRONMENTPRIVILEGE','SECHANGENOTIFYPRIVILEGE','SEREMOTESHUTDOWNPRIVILEGE',
    'SEUNDOCKPRIVILEGE','SESYNCAGENTPRIVILEGE','SEENABLEDELEGATIONPRIVILEGE',
    'SEMANAGEVOLUMEPRIVILEGE','SEIMPERSONATEPRIVILEGE','SECREATEGLOBALPRIVILEGE',
    'SETRUSTEDCREDMANACCESSPRIVILEGE','SERELABELPRIVILEGE','SEINCREASEWORKINGSETPRIVILEGE',
    'SETIMEZONEPRIVILEGE','SECREATESYMBOLICLINKPRIVILEGE');
var
  buf: TSynTempBuffer;
  tp: PTOKEN_PRIVILEGES;
  len: Cardinal;
  i: integer;
  name: AnsiString;
  enumval: integer;
begin
  if Token = 0 then
    raise ESynException.Create('TSynWindowsPrivileges: invalid privileges token');
  buf.Init;
  try
    len := 0;
    if not GetTokenInformation(Token, TokenPrivileges, buf.buf, buf.len, len) then
      if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
        raise ESynException.Create('TSynWindowsPrivileges cannot get token information')
      else begin
        buf.Done;
        buf.Init(len);
        if not GetTokenInformation(Token, TokenPrivileges, buf.buf, buf.len, len) then
          raise ESynException.Create('TSynWindowsPrivileges cannot get token information')
      end;
    tp := buf.buf;
    SetLength(name, MAX_SE_NAME_LENGTH);
    for i := 0 to tp.PrivilegeCount-1 do
    begin
      len := Length(name);
      if not LookupPrivilegeNameA(nil,tp.Privileges[i].Luid,@name[1],len) then
        if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
          raise ESynException.CreateUTF8('TSynWindowsPrivileges cannot lookup privilege name for Luid (%)',
            [PInt64(@tp.Privileges[i].Luid)^]) // PInt64() to avoid URW699 on Delphi 6
        else begin
          SetLength(name, len);
          if not LookupPrivilegeNameA(nil,tp.Privileges[i].Luid,@name[1],len) then
            raise ESynException.CreateUTF8('TSynWindowsPrivileges cannot lookup privilege name for Luid (%)',
              [PInt64(@tp.Privileges[i].Luid)^])
        end;
      enumval := IdemPCharArray(@name[1], UPCASE_SE_NAMES);
      if (enumval >= ord(low(TWinSystemPrivilege))) and (enumval <= ord(high(TWinSystemPrivilege))) then begin
        Include(fAvailable, TWinSystemPrivilege(enumval));
        if tp.Privileges[i].Attributes and SE_PRIVILEGE_ENABLED <> 0 then
          Include(fDefEnabled, TWinSystemPrivilege(enumval));
      end;
    end;
    fEnabled := fDefEnabled;
  finally
    buf.Done;
  end;
end;

function TSynWindowsPrivileges.SetPrivilege(aPrivilege: Pointer;
  aEnablePrivilege: boolean): boolean;
var
  tp: TOKEN_PRIVILEGES;
  id: TLargeInteger;
  tpprev: TOKEN_PRIVILEGES;
  cbprev: DWORD;
begin
  result := false;
  cbprev := sizeof(TOKEN_PRIVILEGES);
  if not LookupPrivilegeValue(nil, aPrivilege, id) then
    exit;
  tp.PrivilegeCount := 1;
  tp.Privileges[0].Luid := PInt64(@id)^;
  tp.Privileges[0].Attributes := 0;
  AdjustTokenPrivileges(Token, false, tp, sizeof(TOKEN_PRIVILEGES), @tpprev, @cbprev);
  if GetLastError <> ERROR_SUCCESS then
    exit;
  tpprev.PrivilegeCount := 1;
  tpprev.Privileges[0].Luid := PInt64(@id)^;
  with tpprev.Privileges[0] do
    if aEnablePrivilege then
      Attributes := Attributes or SE_PRIVILEGE_ENABLED
    else
      Attributes := Attributes xor (SE_PRIVILEGE_ENABLED and Attributes);
  AdjustTokenPrivileges(Token, false, tpprev, cbprev, nil, nil);
  if GetLastError <> ERROR_SUCCESS then
    exit;
  result := true;
end;

const
  ntdll = 'NTDLL.DLL';

type
  _PPS_POST_PROCESS_INIT_ROUTINE = ULONG;

  PUNICODE_STRING = ^UNICODE_STRING;
  UNICODE_STRING = packed record
    Length: word;
    MaximumLength: word;
    {$ifdef CPUX64}
    _align: array[0..3] of byte;
    {$endif}
    Buffer: PWideChar;
  end;

  PMS_PEB_LDR_DATA = ^MS_PEB_LDR_DATA;
  MS_PEB_LDR_DATA = packed record
    Reserved1: array[0..7] of BYTE;
    Reserved2: array[0..2] of pointer;
    InMemoryOrderModuleList: LIST_ENTRY;
  end;

  PMS_RTL_USER_PROCESS_PARAMETERS = ^MS_RTL_USER_PROCESS_PARAMETERS;
  MS_RTL_USER_PROCESS_PARAMETERS = packed record
    Reserved1: array[0..15] of BYTE;
    Reserved2: array[0..9] of pointer;
    ImagePathName: UNICODE_STRING;
    CommandLine: UNICODE_STRING ;
  end;

  PMS_PEB = ^MS_PEB;
  MS_PEB = packed record
    Reserved1: array[0..1] of BYTE;
    BeingDebugged: BYTE;
    Reserved2: array[0..0] of BYTE;
    {$ifdef CPUX64}
    _align1: array[0..3] of byte;
    {$endif}
    Reserved3: array[0..1] of pointer;
    Ldr: PMS_PEB_LDR_DATA;
    ProcessParameters: PMS_RTL_USER_PROCESS_PARAMETERS;
    Reserved4: array[0..103] of BYTE;
    Reserved5: array[0..51] of pointer;
    PostProcessInitRoutine: _PPS_POST_PROCESS_INIT_ROUTINE; // for sure not pointer, otherwise SessionId is broken
    Reserved6: array[0..127] of BYTE;
    {$ifdef CPUX64}
    _align2: array[0..3] of byte;
    {$endif}
    Reserved7: array[0..0] of pointer;
    SessionId: ULONG;
    {$ifdef CPUX64}
    _align3: array[0..3] of byte;
    {$endif}
  end;

  PMS_PROCESS_BASIC_INFORMATION = ^MS_PROCESS_BASIC_INFORMATION;
  MS_PROCESS_BASIC_INFORMATION = packed record
    ExitStatus: Longint;
    {$ifdef CPUX64}
    _align1: array[0..3] of byte;
    {$endif}
    PebBaseAddress: PMS_PEB;
    AffinityMask: PtrUInt;
    BasePriority: Longint;
    {$ifdef CPUX64}
    _align2: array[0..3] of byte;
    {$endif}
    UniqueProcessId: PtrUInt;
    InheritedFromUniqueProcessId: PtrUInt;
  end;

  {$Z4}
  PROCESSINFOCLASS = (ProcessBasicInformation = 0, ProcessDebugPort = 7,
    ProcessWow64Information = 26, ProcessImageFileName = 27,
    ProcessBreakOnTermination = 29, ProcessSubsystemInformation = 75);
  {$Z1}

  NTSTATUS = LongInt;

function NtQueryInformationProcess(ProcessHandle: THandle;
  ProcessInformationClass: PROCESSINFOCLASS; ProcessInformation: pointer;
  ProcessInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  external ntdll name 'NtQueryInformationProcess';
function ReadProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;
  nSize: PTRUINT; var lpNumberOfBytesRead: PTRUINT): BOOL; external 'kernel32' name 'ReadProcessMemory';

function InternalGetProcessInfo(aPID: DWORD; out aInfo: TWinProcessInfo): boolean;
var
  bytesread: PtrUInt;
  sizeneeded: DWORD;
  pbi: PMS_PROCESS_BASIC_INFORMATION;
  peb: MS_PEB;
  peb_upp: MS_RTL_USER_PROCESS_PARAMETERS;
  prochandle: THandle;
  buf: TSynTempBuffer;
begin
  result := false;
  with aInfo do begin
    AvailableInfo := [];
    PID := 0;
    ParentPID := 0;
    SessionID := 0;
    PEBBaseAddress := nil;
    AffinityMask := 0;
    BasePriority := 0;
    ExitStatus := 0;
    BeingDebugged := 0;
    ImagePath := '';
    CommandLine := '';
  end;
  if APID = 0 then
    exit;
  prochandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, FALSE, aPid);
  if prochandle = INVALID_HANDLE_VALUE then
    exit;
  Include(aInfo.AvailableInfo, wpaiPID);
  aInfo.PID := aPid;
  buf.InitZero(SizeOf(MS_PROCESS_BASIC_INFORMATION));
  try
    sizeneeded := 0;
    if NtQueryInformationProcess(prochandle, ProcessBasicInformation, buf.buf, buf.len, @sizeneeded) < 0 then
      exit;
    if buf.len < integer(sizeneeded) then begin
      buf.Done;
      buf.InitZero(sizeneeded);
      if NtQueryInformationProcess(prochandle, ProcessBasicInformation, buf.buf, buf.len, @sizeneeded) < 0 then
        exit;
    end;
    Include(aInfo.AvailableInfo, wpaiBasic);
    pbi := buf.buf;
    with aInfo do begin
      PID := pbi^.UniqueProcessId;
      ParentPID := pbi^.InheritedFromUniqueProcessId;
      BasePriority := pbi^.BasePriority;
      ExitStatus := pbi^.ExitStatus;
      PEBBaseAddress := pbi^.PebBaseAddress;
      AffinityMask := pbi^.AffinityMask;
    end;
    // read PEB (Process Environment Block)
    if not Assigned(pbi.PebBaseAddress) then
      exit;
    bytesread := 0;
    FillCharFast(peb, sizeof(MS_PEB), 0);
    if not ReadProcessMemory(prochandle, pbi.PebBaseAddress, @peb, sizeof(MS_PEB), bytesread) then
      exit;
    Include(aInfo.AvailableInfo, wpaiPEB);
    aInfo.SessionID := peb.SessionId;
    aInfo.BeingDebugged := peb.BeingDebugged;
    FillCharFast(peb_upp, sizeof(MS_RTL_USER_PROCESS_PARAMETERS), 0);
    bytesread := 0;
    if not ReadProcessMemory(prochandle, peb.ProcessParameters, @peb_upp, sizeof(MS_RTL_USER_PROCESS_PARAMETERS), bytesread) then
      exit;
    // command line info
    if peb_upp.CommandLine.Length > 0 then begin
      SetLength(aInfo.CommandLine, peb_upp.CommandLine.Length div 2);
      bytesread := 0;
      if not ReadProcessMemory(prochandle, peb_upp.CommandLine.Buffer, @aInfo.CommandLine[1], peb_upp.CommandLine.Length, bytesread) then
        exit;
      Include(aInfo.AvailableInfo, wpaiCommandLine);
    end;
    // image info
    if(peb_upp.ImagePathName.Length > 0) then begin
      SetLength(aInfo.ImagePath, peb_upp.ImagePathName.Length div 2);
      bytesread := 0;
      if not ReadProcessMemory(prochandle, peb_upp.ImagePathName.Buffer, @aInfo.ImagePath[1], peb_upp.ImagePathName.Length, bytesread) then
        exit;
      Include(aInfo.AvailableInfo, wpaiImagePath);
    end;
    result := true;
  finally
    CloseHandle(prochandle);
    buf.Done;
  end;
end;

procedure GetProcessInfo(aPid: Cardinal; out aInfo: TWinProcessInfo);
var
  privileges: TSynWindowsPrivileges;
begin
  privileges.Init(pttThread);
  try
    privileges.Enable(wspDebug);
    InternalGetProcessInfo(aPid, aInfo);
  finally
    privileges.Done;
  end;
end;

procedure GetProcessInfo(const aPidList: TCardinalDynArray; out aInfo: TWinProcessInfoDynArray);
var
  privileges: TSynWindowsPrivileges;
  i: integer;
begin
  SetLength(aInfo, Length(aPidList));
  privileges.Init(pttThread);
  try
    privileges.Enable(wspDebug);
    for i := 0 to High(aPidList) do
      InternalGetProcessInfo(aPidList[i], aInfo[i]);
  finally
    privileges.Done;
  end;
end;

{$endif MSWINDOWS}

end.
