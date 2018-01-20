/// daemon managment classes for mORMot, including low-level Win NT Service
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotService;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2018 Arnaud Bouchez
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

  Portions created by the Initial Developer are Copyright (C) 2018
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Eric Grange
  - Leander007

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


      Daemon / Service managment classes
      ----------------------------------

    Version 1.3
    - TService debug and enhancements
    - can compile without SQLite3Commons dependency for smaller executables

    Version 1.4 - February 8, 2010
    - whole Synopse SQLite3 database framework released under the GNU Lesser
      General Public License version 3, instead of generic "Public Domain"

    Version 1.16
    - code refactoring after Leander007 proposals for better compatibility -
      see https://synopse.info/forum/viewtopic.php?id=584

    Version 1.18
    - renamed SQLite3Service.pas to mORMotService.pas
    - added FPC compatibility (including missing WinSvc.pas API unit)
    - changed ServicesRun to return an indicator of success - see [8666906039]
    - TServiceController.CreateOpenService() use lower rights - see [c3ebb6b5d6]
    - added TServiceSingle class and its global handler (to be used instead of
      TServer which does not work as expected)
    - added logging to the Service registration and command process
    - added TServiceController.CheckParameters() generic method to control
      a service from the command line
    - check the executable file in TServiceController.CreateNewService()
    - use private global ServiceLog instead of TSQLLog - see [779d773e966]
    - ensure TServiceController.CreateNewService() won't allow to install
      the service on a network drive - see [f487d3de45]
    - add an optional Description text when the service is installed
    - added TSynDaemon/TSynDaemonSettings cross-platform classses as a lighter
      alternative to dddInfraApps/dddInfraSettings

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
  SynLog,
  mORMot; // for TSynJsonFileSettings

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
  TServiceStatus = record
    dwServiceType: DWORD;
    dwCurrentState: DWORD;
    dwControlsAccepted: DWORD;
    dwWin32ExitCode: DWORD;
    dwServiceSpecificExitCode: DWORD;
    dwCheckPoint: DWORD;
    dwWaitHint: DWORD;
  end;
  SC_HANDLE = THandle;
  SERVICE_STATUS_HANDLE = DWORD;
  TServiceTableEntry = record
    lpServiceName: PChar;
    lpServiceProc: TFarProc;
  end;

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
function ControlService(hService: SC_HANDLE; dwControl: DWORD;
  var lpServiceStatus: TServiceStatus): BOOL; stdcall; external advapi32;
function SetServiceStatus(hServiceStatus: SERVICE_STATUS_HANDLE;
  var lpServiceStatus: TServiceStatus): BOOL; stdcall; external advapi32;
function RegisterServiceCtrlHandler(lpServiceName: PChar;
  lpHandlerProc: TFarProc): SERVICE_STATUS_HANDLE; stdcall; external advapi32
  name 'RegisterServiceCtrlHandler'+{$ifdef UNICODE}'W'{$else}'A'{$endif};
function StartServiceCtrlDispatcher(
  var lpServiceStartTable: TServiceTableEntry): BOOL; stdcall; external advapi32
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
    // passed if there are no dependances).
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
    // - an optional Description text for the service may be specified
    class procedure CheckParameters(const ExeFileName: TFileName;
      const ServiceName,DisplayName,Description: string);
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
  Services: TList = nil;

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

{$endif MSWINDOWS}


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
    fWorkFolderName: TFileName;
    fSettings: TSynDaemonSettings;
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
      const aSettingsExt: TFileName = '.settings'); reintroduce;
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
    /// the settings associated with this daemon
    // - will be allocated in Create constructor, and released in Destroy
    property Settings: TSynDaemonSettings read fSettings;
  end;

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
    ServiceLog.Add.Log(sllLastError,'OpenSCManager("%","%") for "%"',
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
    ServiceLog.Add.Log(sllLastError,'OpenSCManager("%","%") for "%"',
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
  ServiceLog.Add.Log(sllTrace,FName,TypeInfo(TServiceState),result);
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
  desc := StringToSynUnicode(Description);
  ChangeServiceConfig2(FHandle, SERVICE_CONFIG_DESCRIPTION, @desc);
end;

class procedure TServiceController.CheckParameters(const ExeFileName: TFileName;
  const ServiceName,DisplayName,Description: string);
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
    ServiceLog.Add.Log(sllInfo,'Controling % with command "%"',[ServiceName,param]);
    if param='/install' then
     TServiceController.Install(
       ServiceName,DisplayName,Description,true,ExeFileName) else
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
    raise Exception.CreateFmt('Attempt to install a service ' +
          'with duplicated name: %s', [aServiceName]);
  fSName := aServiceName;
  fDName := aDisplayName;
  if aDisplayName = '' then
    fDName := aServiceName;
  if Services=nil then
    GarbageCollectorFreeAndNil(Services,TList.Create);
  Services.Add(self);
  fServiceType := SERVICE_WIN32_OWN_PROCESS or SERVICE_INTERACTIVE_PROCESS;
  fStartType   := SERVICE_AUTO_START;
  fStatusRec.dwServiceType := fServiceType;
  fStatusRec.dwCurrentState := SERVICE_STOPPED;
  fStatusRec.dwControlsAccepted := 31;
  fStatusRec.dwWin32ExitCode := NO_ERROR;
  ServiceLog.Add.Log(sllInfo,'% (%) running as "%"',
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
      raise Exception.CreateFmt('Cannot find service %s to remove from the list',
        [fsName]);
    Services.Delete(i);
    if Assigned(fJumper) then
      VirtualFree(fJumper, 0, MEM_RELEASE);
  end;
  inherited;
end;

procedure TService.DoCtrlHandle(Code: DWORD);
begin
  ServiceLog.Enter(self);
  ServiceLog.Add.Log(sllInfo,'%: command % received from OS',[ServiceName,Code],self);
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
procedure JumpToService;
asm
  pop  eax
  mov  eax, [eax]           // retrieve self value
  mov  edx, [esp+4]
  call TService.CtrlHandle
  ret  4
end;
{$endif CPUX86}

function TService.GetControlHandler: TServiceControlHandler;
{$ifdef CPUX86}
var AfterCallAddr: Pointer;
    Offset: Integer;
{$endif}
begin
  Result := fControlHandler;
  if not Assigned(Result) then
    ServiceLog.Add.Log(sllError,'%.GetControlHandler with fControlHandler=nil: '+
      'use TServiceSingle or set a custom ControlHandler',[self]);
  {$ifdef CPUX86}
  exit;
  if not Assigned(Result) then
  begin
    raise Exception.Create('Automated jumper generation is not working: '+
     'use TServiceSingle or set a custom ControlHandler');
    if fJumper=nil then begin
      fJumper := VirtualAlloc(nil, 5+sizeof(Pointer), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
      if fJumper=nil then
        raise Exception.CreateFmt('Cannot allocate memory for service jump gate: %s',
          [fSName]);
      AfterCallAddr := Pointer(PtrUInt(fJumper)+5);
      Offset :=  PtrInt(@JumpToService)-PtrInt(AfterCallAddr);
      fJumper[0] := $E8; // call opcode
      PInteger(@fJumper[1])^ := Offset;       // points to JumpToService
      PPtrUInt(@fJumper[5])^ := PtrUInt(self); // will be set as EAX=self
    end;
    Result := Pointer(fJumper);
  end;
  {$endif CPUX86}
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
  if Assigned(fJumper) then
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
begin
  if (Services=nil) or (Services.Count=0) then begin
    result := false;
    exit;
  end;
  for i := 0 to Services.Count-1 do begin
    service := Services.List[i];
    if not assigned(service.fControlHandler) then
      raise ESynException.CreateUTF8('%.ControlHandler=nil (ServiceName="%"): '+
       'use TServiceSingle or set a custom ControlHandler',[service,service.ServiceName]);
  end;
  SetLength(S,Services.Count+1); // +1 so that the latest entry is nil
  for i := 0 to Services.Count-1 do begin
    S[i].lpServiceName := pointer(TService(Services.List[i]).ServiceName);
    S[i].lpServiceProc := @ServiceProc;
  end;
  result := StartServiceCtrlDispatcher(S[0]);
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
    raise Exception.Create('Only one TServiceSingle is allowed at a time');
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

{$endif MSWINDOWS}


{ *** cross-plaform high-level services }

{ TSynDaemonSettings }

constructor TSynDaemonSettings.Create;
begin
  inherited Create;
  fLog := LOG_STACKTRACE + [sllNewRun];
  fLogRotateFileCount := 2;
  fServiceName := UTF8ToString(ExeVersion.ProgramName);
  fServiceDisplayName := fServiceName;
  {$ifndef MSWINDOWS}
  fLogPath := '/var/log/';
  {$endif}
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
      result := format('%s - (c)%d %s', [result, BuildYear, CompanyName]);
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
  const aWorkFolder, aSettingsFolder, aLogFolder, aSettingsExt: TFileName);
var
  setf: TFileName;
begin
  inherited Create;
  if aWorkFolder = '' then
    fWorkFolderName := ExeVersion.ProgramFilePath
  else
    fWorkFolderName := EnsureDirectoryExists(aWorkFolder, true);
  if aSettingsClass = nil then
    aSettingsClass := TSynDaemonSettings;
  fSettings := aSettingsClass.Create;
  setf := aSettingsFolder;
  if setf = '' then
    setf := {$ifdef MSWINDOWS}fWorkFolderName{$else}'/etc/'{$endif};
  fSettings.LoadFromFile(format('%s%s%s', [setf, ExeVersion.ProgramName, aSettingsExt]));
  if fSettings.LogPath = '' then
    if aLogFolder = '' then
      fSettings.LogPath := {$ifdef MSWINDOWS}fWorkFolderName{$else}'/var/log/'{$endif}
    else
      fSettings.LogPath := EnsureDirectoryExists(aLogFolder);
end;

destructor TSynDaemon.Destroy;
begin
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

{$else} // Linux/POSIX signal interception

var
  SynDaemonTerminated: integer;

{$ifdef FPC}
procedure DoShutDown(Sig: Longint; Info: PSigInfo; Context: PSigContext); cdecl;
{$else}
procedure DoShutDown(Sig: integer); cdecl;
{$endif}
begin
  SynDaemonTerminated := Sig;
end;

procedure SigIntercept;
{$ifdef FPC}
var
  saOld, saNew: SigactionRec;
begin
  FillCharFast(saNew, SizeOf(saNew), 0);
  saNew.sa_handler := @DoShutDown;
  fpSigaction(SIGQUIT, @saNew, @saOld);
  fpSigaction(SIGTERM, @saNew, @saOld);
  fpSigaction(SIGINT, @saNew, @saOld);
{$else} // Kylix
var
  saOld, saNew: TSigAction;
begin
  FillCharFast(saNew, SizeOf(saNew), 0);
  saNew.__sigaction_handler := @DoShutDown;
  sigaction(SIGQUIT, @saNew, @saOld);
  sigaction(SIGTERM, @saNew, @saOld);
  sigaction(SIGINT, @saNew, @saOld);
{$endif}
end;

{$endif MSWINDOWS}

type
  TExecuteCommandLineCmd = (
     cNone, cInstall, cUninstall, cVersion, cConsole, cVerbose, cRun, cFork,
     cStart, cStop, cState, cHelp);

procedure TSynDaemon.CommandLine(aAutoStart: boolean);
var
  cmd: TExecuteCommandLineCmd;
  param: RawUTF8;
  log: TSynLog;
  {$ifdef MSWINDOWS}
  service: TServiceSingle;
  ctrl: TServiceController;
  {$else}

  procedure RunUntilSigTerminated(dofork: boolean);
  var
    pid, sid: {$ifdef FPC}TPID{$else}pid_t{$endif};
  begin
    SigIntercept;
    try
      if dofork then begin
        pid := {$ifdef FPC}fpFork{$else}fork{$endif};
        if pid < 0 then
          raise ESynException.CreateUTF8('%.CommandLine Fork failed', [self]);
        if pid > 0 then begin // main program - just terminate
          //{$ifdef FPC}fpExit{$else}__exit{$endif}(0);
          exit;
        end else begin // clean forked instance
          sid := {$ifdef FPC}fpSetSID{$else}setsid{$endif};
          if sid < 0 then // new session (process group) created?
            raise ESynException.CreateUTF8('%.CommandLine SetSID failed', [self]);
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
      end;
      log := fSettings.fLogClass.Add;
      try
        log.Log(sllNewRun, 'Start % %', [fSettings.ServiceName,
          ExeVersion.Version.DetailedOrVoid], self);
        Start;
        while SynDaemonTerminated = 0 do
          {$ifdef FPC}fpPause{$else}pause{$endif};
      finally
        log.Log(sllNewRun, 'Stop from Sig=%', [SynDaemonTerminated], self);
        Stop;
      end;
    except
      on E: Exception do begin
        if not dofork then
          ConsoleShowFatalException(E, true);
        Halt(1); // notify error to caller process
      end;
    end;
  end;
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
    spaces: string;
  begin
    WriteCopyright;
    writeln('Try with one of the switches:');
    writeln({$ifdef MSWINDOWS}' '{$else}' ./'{$endif}, ExeVersion.ProgramName,
      ' /console -c /verbose /help -h /version');
    spaces := StringOfChar(' ', length(ExeVersion.ProgramName) + 2);
    {$ifdef MSWINDOWS}
    writeln(spaces, '/install /uninstall /start /stop /state');
    {$else}
    writeln(spaces, ' /run -r /fork -f');
    {$endif}
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
      msg := FormatUTF8('Error % "%" occured with',
        [error, StringToUTF8(SysErrorMessage(error))]);
      TextColor(ccLightRed);
      ExitCode := 1; // notify error to caller batch
    end;
    msg := FormatUTF8('% "%" (%) on Service "%"',
      [msg, param, cmdText, fSettings.ServiceName]);
    writeln(UTF8ToConsole(msg));
    TextColor(ccLightGray);
    log.Log(sllDebug, 'CommandLine: %', [msg], self);
  end;

begin
  if (self = nil) or (fSettings = nil) then
    exit;
  log := nil;
  {$I-}
  param := trim(StringToUTF8(paramstr(1)));
  if (param = '') or not (param[1] in ['/', '-']) then
    cmd := cNone
  else
    case upcase(param[2]) of
    'C':
      cmd := cConsole;
    'R':
      cmd := cRun;
    'F':
      cmd := cFork;
    'H':
      cmd := cHelp;
    else
      byte(cmd) := ord(cInstall) + IdemPCharArray(@param[2], ['INST', 'UNINST',
        'VERS', 'CONS', 'VERB', 'RUN', 'FORK', 'START', 'STOP', 'STAT', 'HELP']);
    end;
  case cmd of
  cHelp:
    Syntax;
  cVersion: begin
    WriteCopyright;
    writeln(' ', fSettings.ServiceName,
      #13#10' Size: ', FileSize(ExeVersion.ProgramFileName), ' bytes' +
      #13#10' Build date: ', ExeVersion.Version.BuildDateTimeString);
    if ExeVersion.Version.Version32 <> 0 then
      writeln(' Version: ', ExeVersion.Version.Detailed);
  end;
  cConsole, cVerbose:
    try
      WriteCopyright;
      writeln('Launched in ', cmdText, ' mode'#10);
      TextColor(ccLightGray);
      log := fSettings.fLogClass.Add;
      if (cmd = cVerbose) and (log <> nil) then  // leave as in settings for -c
        log.Family.EchoToConsole := LOG_VERBOSE;
      try
        log.Log(sllNewRun, 'Start % %', [fSettings.ServiceName,
          ExeVersion.Version.DetailedOrVoid], self);
        Start;
        writeln('Press [Enter] to quit');
        ioresult;
        readln;
        writeln('Shutting down server');
      finally
        ioresult;
        log.Log(sllNewRun, 'Stop', self);
        Stop;
      end;
    except
      on E: Exception do
        ConsoleShowFatalException(E, true);
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
          ServiceDescription, aAutoStart) <> ssNotInstalled);
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
  cRun:
    RunUntilSigTerminated(false);
  cFork:
    RunUntilSigTerminated(true);
  else
    Syntax;
  {$endif MSWINDOWS}
  end;
  {$I+}
end;


end.
