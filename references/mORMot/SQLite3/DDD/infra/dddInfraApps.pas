/// shared DDD Infrastructure: Application/Daemon implementation classes
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit dddInfraApps;

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

  TODO:
   - store settings in database, or a centralized service?
   - allow to handle authentication via a centralized service or REST server

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$else}
  {$ifdef FPC}
  SynFPCLinux,
  {$endif}
  {$endif}
  mORMotService, // for running the daemon as a regular Windows Service
  SysUtils,
  Classes,
  Variants,
  SynCommons,
  SynTable,
  SynLog,
  SynCrypto,
  SynEcc,
  mORMot,
  mORMotDDD,
  dddInfraSettings,
  SynCrtSock,
  SynBidirSock,
  mORMotHttpServer, // for publishing a TSQLRestServer over HTTP
  mORMotHttpClient; // for publishing a TSQLRestClientURI over HTTP


{ ----- Implements Service/Daemon Applications }

type
  /// abstract class to handle any kind of service/daemon executable
  // - would implement either a Windows Service, a stand-alone remotely
  // administrated daemon, or a console application, according to the command line
  // - you should inherit from this class, then override the abstract NewDaemon
  // protected method to launch and return a IAdministratedDaemon instance
  {$M+}

  { TDDDDaemon }

  TDDDDaemon = class
  protected
    fSettings: TDDDAdministratedDaemonSettings;
    fSettingsRef: IUnknown;
    /// the service/daemon will be stopped when this interface is set to nil
    fDaemon: IAdministratedDaemon;
    /// this abstract method should be overriden to return a new service/daemon
    // instance, using the (inherited) fSettings as parameters
    function NewDaemon: TDDDAdministratedDaemon; virtual;
    /// returns some text to be supplied to the console for /help - '' by default
    function CustomHelp: string; virtual;
    procedure DoStart{$ifdef MSWINDOWS}(Sender: TService=nil){$endif};
    procedure DoStop{$ifdef MSWINDOWS}(Sender: TService=nil){$endif};
  public
    /// initialize the service/daemon application thanks to some information
    // - actual settings would inherit from TDDDAdministratedDaemonSettingsFile,
    // to define much more parameters, according to the service/daemon process
    // - the supplied settings will be owned by this TDDDDaemon instance
    constructor Create(aSettings: TDDDAdministratedDaemonSettings); virtual;
    /// finalize the service/daemon application, and release its resources
    destructor Destroy; override;
    /// interpret the command line to run the application as expected
    // - under Windows, /install /uninstall /start /stop would control the
    // daemon as a Windows Service - you should run the program with
    // administrator rights to be able to change the Services settings
    // - /console or /c would run the program as a console application
    // - /verbose would run the program as a console application, in verbose mode
    // - /daemon or /d would run the program as a remotely administrated
    // daemon, using a published IAdministratedDaemon service
    // - /version would show the current revision information of the application
    // - no command line argument will run the program as a Service dispatcher
    // under Windows (as a regular service), or display the syntax - or would
    // run the service in /verbose mode if ForceRun is TRUE (may be useful e.g.
    // to debug an associated .dll using this daemon as host application)
    // - any invalid switch, or no switch under Linux, will display the syntax
    // - this method will output any error or information to the console
    // - as a result, a project .dpr could look like the following:
    //!begin
    //!  with TMyApplicationDaemon.Create(TMyApplicationDaemonSettings.Create) do
    //!  try
    //!    ExecuteCommandLine;
    //!  finally
    //!    Free;
    //!  end;
    //!end.
    procedure ExecuteCommandLine(ForceRun: boolean=false);
    /// start the daemon, until the instance is released
    procedure Execute;
    /// wait for Daemon property to be set
    // - Execute method should have been previously caleld
    // - returns true if the daemon has been started in the specified time
    function WaitStarted(TimeoutMS: integer=10000): boolean;
    /// start the daemon and wait for Daemon property to be set
    // - returns true if the daemon has been started in the specified time
    function ExecuteAndWaitStarted(TimeoutMS: integer=10000): boolean;
    /// read-only access to the underlying daemon instance
    // - equals nil if the daemon is not started
    property Daemon: IAdministratedDaemon read fDaemon;
    /// returns the class instance implementing the underlying Daemon
    // - you should transtype the returned instance using e.g.
    // !  myDaemon := mainDaemon.DaemonInstance as TMyDaemonClass;
    function DaemonInstance: TObject;
  published
  end;
  {$M-}

  /// abstract class to implement a IAdministratedDaemon service via a TThread
  // - as hosted by TDDDDaemon service/daemon application
  TDDDThreadDaemon = class(TDDDAdministratedThreadDaemon)
  protected
    // returns the system memory info as current state
    function InternalRetrieveState(var Status: variant): boolean; override;
    function GetAdministrationHTTPServer: TSQLHttpServer;
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// IAdministratedDaemon command to subscribe to a set of events for
    // real-time remote monitoring of the specified log events
    // - this overriden method would disallow remote logs if low-level frames
    // logging is set (i.e. HttpClientFullWebSocketsLog / HttpServerFullWebSocketsLog)
    // to avoid an unexpected race condition
    procedure SubscribeLog(const Levels: TSynLogInfos; const Callback: ISynLogCallback;
      ReceiveExistingKB: cardinal); override;
    /// reference to the HTTP server publishing IAdministratedDaemon service
    // - may equal nil if TDDDAdministratedDaemonSettingsFile.AuthHttp.BindPort=''
    property AdministrationHTTPServer: TSQLHttpServer read GetAdministrationHTTPServer;
  end;

  /// abstract class to implement a IAdministratedDaemon service via a TSQLRestServer
  // - as hosted by TDDDDaemon service/daemon application
  TDDDRestDaemon = class(TDDDAdministratedRestDaemon, IAdministratedDaemon)
  protected
    fPreviousMonitorTix: Int64;
    function GetAdministrationHTTPServer: TSQLHttpServer;
      {$ifdef HASINLINE}inline;{$endif}
    // returns the current state from fRest.Stat() + system memory
    function InternalRetrieveState(var Status: variant): boolean; override;
    procedure InternalLogMonitoring; virtual;
  public
    /// IAdministratedDaemon command to subscribe to a set of events for
    // real-time remote monitoring of the specified log events
    // - this overriden method would disallow remote logs if low-level frames
    // logging is set (i.e. HttpClientFullWebSocketsLog / HttpServerFullWebSocketsLog)
    // to avoid an unexpected race condition
    procedure SubscribeLog(const Levels: TSynLogInfos; const Callback: ISynLogCallback;
      ReceiveExistingKB: cardinal); override;
    /// reference to the HTTP server publishing IAdministratedDaemon service
    // - may equal nil if TDDDAdministratedDaemonSettingsFile.AuthHttp.BindPort=''
    property AdministrationHTTPServer: TSQLHttpServer read GetAdministrationHTTPServer;
  end;

  /// abstract class to implement a IAdministratedDaemon service via a
  // TSQLRestServer, publishing its services as HTTP
  // - as hosted by TDDDDaemon service/daemon application
  TDDDRestHttpDaemon = class(TDDDRestDaemon)
  protected
    fHttpServer: TSQLHttpServer;
    fServicesLogRest: TSQLRest;
    function Settings: TDDDAdministratedDaemonHttpSettings;
      {$ifdef HASINLINE}inline;{$endif}
    // initialize HTTP Server into fHttpServer
    // (fRest should have been set by the overriden method)
    procedure InternalStart; override;
    // finalize HTTP Server and SOA log database
    procedure InternalStop; override;
  public
    /// generate API documentation corresponding to REST SOA interfaces
    procedure WrapperGenerate(const DestFile: TFileName;
      const Template: TFileName = 'API.adoc.mustache');
    /// reference to the main HTTP server publishing this daemon Services
    // - may be nil outside a Start..Stop range
    property HttpServer: TSQLHttpServer read fHttpServer;
    /// reference to the associated REST server storing the SOA log database
    // - may be nil if the daemon did not implement this feature 
    property ServicesLogRest: TSQLRest read fServicesLogRest;
  end;


{ ----- Implements ORM/SOA REST Client access }

type
  /// exception raised by TDDDRestClientSettings classes
  EDDDRestClient = class(EDDDException);

  /// advanced parameters for TDDDRestClientSettings definition
  TDDDRestClientDefinition = class(TSynPersistentWithPassword)
  protected
    fRoot: RawUTF8;
    fConnectRetrySeconds: integer;
  published
    /// the URI Root to be used for the REST Model
    property Root: RawUTF8 read fRoot write fRoot;
    /// the encrypted password to be used to connect with WebSockets
    property WebSocketsPassword: RawUTF8 read fPassWord write fPassWord;
    /// how many seconds the client may try to connect after open socket failure
    property ConnectRetrySeconds: integer read fConnectRetrySeconds write fConnectRetrySeconds;
  end;

  /// storage class for initializing an ORM/SOA REST Client class
  // - this class will contain some generic properties to initialize a
  // TSQLRestClientURI pointing to a remote server, using WebSockets by default
  // - WebSockets support is the reason why this class is defined in
  // dddInfraApps, and not dddInfraSettings
  TDDDRestClientSettings = class(TSynAutoCreateFields)
  protected
    fORM: TSynConnectionDefinition;
    fClient: TDDDRestClientDefinition;
    fTimeout, fWebSocketsLoopDelay: integer;
  public
    /// set the default values for Client.Root, ORM.ServerName,
    // Client.WebSocketsPassword and ORM.Password
    procedure SetDefaults(const Root, Port, WebSocketPassword, UserPassword: RawUTF8;
      const User: RawUTF8 = 'User'; const Server: RawUTF8 = 'localhost';
      ForceSetCredentials: boolean = false; ConnectRetrySeconds: integer = 0;
      WebSocketsLoopDelayMS: integer = 0);
    /// is able to instantiate a Client REST instance for the stored definition
    // - Definition.Kind is expected to specify a TSQLRestClient class to be
    // instantiated, not a TSQLRestServer instance
    // - will return nil if the supplied Definition is not correct
    // - note that the supplied Model.Root is expected to be the default root
    // URI, which will be overriden with this TDDDRestSettings.Root property
    // - will also set the TSQLRest.LogFamily.Level from LogLevels value,
    function NewRestClientInstance(aRootSettings: TDDDAppSettingsAbstract;
      aModel: TSQLModel = nil; aOptions: TDDDNewRestInstanceOptions = [riOwnModel,
      riCreateVoidModelIfNone, riHandleAuthentication, riRaiseExceptionIfNoRest]): TSQLRestClientURI; virtual;
    /// you may assign this method to a TSQLRestClientURI.OnAuthentificationFailed
    // property, so that the client would automatically try to re-connect
    function OnAuthentificationFailed(Retry: integer; var aUserName, aPassword: string;
      out aPasswordHashed: boolean): boolean;
    /// you can overload here the TCP timeout delay, in seconds
    property Timeout: integer read fTimeout write fTimeout;
    /// you can overload here the WebSockets internal loop delay, in milliseconds
    property WebSocketsLoopDelay: integer read fWebSocketsLoopDelay write fWebSocketsLoopDelay;
  published
    /// defines a mean of access to a TSQLRest instance
    // - using Kind/ServerName/DatabaseName/User/Password properties: Kind
    // would define the TSQLRest class to be instantiated by NewRestClientInstance()
    property ORM: TSynConnectionDefinition read fORM;
    /// advanced connection options
    // - ORM.Password defines the authentication main password, and
    // Client.WebSocketsPassword is used for WebSockets binary encryption
    property Client: TDDDRestClientDefinition read fClient;
  end;

  /// abstract client to connect to any daemon service via WebSockets
  // - will monitor the connection, to allow automatic reconnection, with proper
  // services resubscription 
  TDDDRestClientWebSockets = class(TSQLHttpClientWebsockets)
  protected
    fApplicationName: RawUTF8;
    fOwnedSettings: TDDDRestClientSettings;
    fConnected: boolean;
    fServicesRegistered: boolean;
    fOnConnect, fOnDisconnect: TOnRestClientNotify;
    // called after connection, before fOnConnect event - to register callbacks
    procedure AfterConnection; virtual;
    // called after disconnection, before fOnDisconnect event - unregister callbacks
    procedure AfterDisconnection; virtual;
    // call ClientDisconnect if HTTP_NOTIMPLEMENTED (i.e. websocket link broken)
    procedure ClientFailed(Sender: TSQLRestClientURI; E: Exception;
      Call: PSQLRestURIParams); virtual;
    // notify AfterDisconnection + fOnDisconnect if needed
    procedure ClientDisconnect;
    procedure WebSocketsClosed(Sender: TObject); virtual;
    // notify AfterDisconnection+fOnDisconnect if needed, then AfterConnection+fOnConnect
    procedure ClientSetUser(Sender: TSQLRestClientURI); virtual;
    // inherited classes should override those abstract methods
    procedure DefineApplication; virtual; abstract;
    procedure RegisterServices; virtual; abstract;
    function CreateModel(aSettings: TDDDRestClientSettings): TSQLModel; virtual;
  public
    /// initialize the client instance with the supplied settings
    constructor Create(aSettings: TDDDRestClientSettings;
      aOnConnect: TOnRestClientNotify = nil;
      aOnDisconnect: TOnRestClientNotify = nil); reintroduce; overload; virtual;
    /// finalize the client instance
    destructor Destroy; override;
    /// returns the server version, using timestamp/info method-based service
    property ApplicationVersion: RawUTF8 read GetSessionVersion;
    /// reflects the current WebSockets connection state
    property Connected: boolean read fConnected;
    /// human-friendly application name, as set by overriden DefineApplication
    property ApplicationName: RawUTF8 read fApplicationName;
  end;

  /// abstract client to connect to any daemon service via HTTP or HTTPS
  // - defines a simple REST client, without connection tracking
  TDDDRestClientHttp = class(TSQLHttpsClient)
  protected
    fApplicationName: RawUTF8;
    // inherited classes should override those abstract methods
    procedure DefineApplication; virtual; abstract;
    procedure RegisterServices; virtual; abstract;
  public
    /// returns the server version, using timestamp/info method-based service
    property ApplicationVersion: RawUTF8 read GetSessionVersion;
    /// human-friendly application name, as set by overriden DefineApplication
    property ApplicationName: RawUTF8 read fApplicationName;
  end;

/// create a client safe asynchronous connection to a IAdministratedDaemon service
function AdministratedDaemonClient(Definition: TDDDRestClientSettings;
   Model: TSQLModel = nil): TSQLHttpClientWebsockets;

{
/// create a client safe asynchronous connection to a IAdministratedDaemon service
// via a IAdministratedDaemonProxy service
function AdministratedDaemonClientFromProxy(Definition: TDDDRestClientSettings;
  const ProxyIPPort: RawUTF8; Model: TSQLModel = nil): IAdministratedDaemon;
}

/// create a WebSockets server instance, publishing a IAdministratedDaemon service
function AdministratedDaemonServer(Settings: TDDDAdministratedDaemonSettings;
  DaemonClass: TDDDAdministratedDaemonClass): TDDDAdministratedDaemon;


{ ----- Implements Thread Processing to access a TCP server }

type
  TDDDSocketThread = class;

  /// the current connection state of the TCP client associated to a
  // TDDDSocketThread thread
  TDDDSocketThreadState = (tpsDisconnected, tpsConnecting, tpsConnected);

  /// the monitoring information of a TDDDSocketThread thread
  TDDDSocketThreadMonitoring = class(TDDDAdministratedDaemonMonitor)
  protected
    fState: TDDDSocketThreadState;
    fOwner: TObject;
    function GetSocket: variant;
  public
    /// may be a TDDDSocketThread instance, or not (to maintain a global state
    // over several threads)
    property Owner: TObject read fOwner write fOwner;
  published
    /// how this thread is currently connected to its associated TCP server
    property State: TDDDSocketThreadState read fState write fState;
    /// information about the associated socket
    property Socket: variant read GetSocket;
  end;

  /// interface allowing to customize/mock a socket connection
  IDDDSocket = interface
    /// connect to the host via the (mocked) socket
    // - should raise an exception on error
    procedure Connect;
    /// returns instance identifier
    // - e.g. the TCrtSocket.Sock number as text
    function Identifier: RawUTF8;
    /// get some low-level information about the last occurred error
    // - e.g. TCrtSocket.LastLowSocketError value
    function LastError: RawUTF8;
    /// returns the number of bytes pending in the (mocked) socket
    // - call e.g. TCrtSocket.SockInPending() with aSocketForceCheck=true,
    // to return bytes both in the instance memory buffer and the socket API
    function DataInPending(aTimeOut: integer): integer;
    /// get Length bytes from the (mocked) socket
    // - returns the number of bytes read into the Content buffer
    // - call e.g. TCrtSocket.SockInRead() method
    function DataIn(Content: PAnsiChar; ContentLength: integer): integer;
    /// send Length bytes to the (mocked) socket
    // - returns false on any error, true on success
    // - call e.g. TCrtSocket.TrySndLow() method
    function DataOut(Content: PAnsiChar; ContentLength: integer): boolean;
    /// returns the low-level handle of this connection
    // - is e.g. the socket file description
    function Handle: integer;
  end;

  /// implements IDDDSocket using a SynCrtSock.TCrtSocket instance
  // - used e.g. by TDDDSocketThread for its default network communication
  // - this class will also create two mutexes, one for DataIn/DataInPending,
  // another for DataOut thread-safe process
  TDDDSynCrtSocket = class(TInterfacedObjectLocked, IDDDSocket)
  protected
    fSocket: TCrtSocket;
    fOwner: TThread;
    fHost, fPort: SockString;
    fInternalBufferSize: integer;
    fOutput: TSynLocker; // input lock is TInterfacedObjectLocked.Safe
  public
    /// initialize the internal TCrtSocket instance
    constructor Create(aOwner: TThread; const aHost, aPort: SockString;
      aSocketTimeout, aInternalBufferSize: integer); reintroduce; virtual;
    /// finalize the internal TCrtSocket instance
    destructor Destroy; override;
    /// call TCrtSocket.OpenBind
    procedure Connect;
    /// returns TCrtSocket.Sock number as text
    function Identifier: RawUTF8;
    /// get information from TCrtSocket.LastLowSocketError
    function LastError: RawUTF8;
    /// call TCrtSocket.SockInPending() method
    // - with aSocketForceCheck=true, to return bytes both in the instance
    // memory buffer and the socket API
    function DataInPending(aTimeOut: integer): integer;
    /// call TCrtSocket.SockInRead() method
    function DataIn(Content: PAnsiChar; ContentLength: integer): integer;
    /// call TCrtSocket.TrySndLow() method
    function DataOut(Content: PAnsiChar; ContentLength: integer): boolean;
    /// returns TCrtsocket.Sock
    function Handle: integer;
    /// read-only access to the associated processing thread
    // - not published, to avoid stack overflow since TDDDSocketThreadMonitoring
    // would point to this instance
    property Owner: TThread read fOwner;
  published
    /// read-only access to the associated processing socket
    property Socket: TCrtSocket read fSocket;
  end;

  /// defines the potential mocked actions for TDDDMockedSocket.MockException()
  TDDDMockedSocketException = (msaConnectRaiseException, msaDataInPendingTimeout,
    msaDataInPendingFails, msaDataInRaiseException, msaDataOutRaiseException,
    msaDataOutReturnsFalse);

  /// defines a set of mocked actions for TDDDMockedSocket.MockException()
  TDDDMockedSocketExceptions = set of TDDDMockedSocketException;

  /// defines the potential mocked actions for TDDDMockedSocket.MockLatency()
  TDDDMockedSocketLatency = (mslConnect, mslDataIn, mslDataOut);

  /// defines a set of mocked actions for TDDDMockedSocket.MockLatency()
  TDDDMockedSocketLatencies = set of TDDDMockedSocketLatency;

  /// the default exception class raised by TDDDMockedSocket
  EDDDMockedSocket = class(EDDDInfraException);

  /// implements IDDDSocket using a fake/mocked in-memory input/ouput storage
  // - may be supplied to TDDDSocketThread to bypass its default network communication
  // - you could fake input/output of TCP/IP packets by calling MockDataIn() and
  // MockDataOut() methods - incoming and outgoing packets would be merged in
  // the internal in-memory buffers, as with a regular Socket
  // - you could fake exception, for any upcoming method call, via MockException()
  // - you could emulate network latency, for any upcoming method call, via
  // MockLatency() - to emulate remote/wireless access, or thread pool contention
  // - this implementation is thread-safe, so multiple threads could access
  // the same IDDDSocket instance, and settings be changed in real time
  TDDDMockedSocket = class(TInterfacedObjectLocked, IDDDSocket)
  protected
    fInput, fOutput: RawByteString;
    fExceptionActions: TDDDMockedSocketExceptions;
    fExceptionMessage: string;
    fExceptionClass: ExceptClass;
    fLatencyActions: TDDDMockedSocketLatencies;
    fLatencyMS: integer;
    fOwner: TThread;
    function GetPendingInBytes: integer;
    function GetPendingOutBytes: integer;
    procedure CheckLatency(Action: TDDDMockedSocketLatency);
    procedure CheckRaiseException(Action: TDDDMockedSocketException);
  public
    /// initialize the mocked socket instance
    constructor Create(aOwner: TThread); reintroduce; virtual; 
    /// add some bytes to the internal fake input storage
    // - would be made accessible to the DataInPending/DataIn methods
    // - the supplied buffer would be gathered to any previous MockDataIn()
    // call, which has not been read yet by the DataIn() method
    procedure MockDataIn(const Content: RawByteString);
    /// return the bytes from the internal fake output storage
    // - as has be previously set by the DataOut() method
    // - will gather all data from several DataOut() calls in a single buffer
    function MockDataOut: RawByteString;
    /// the specified methods would raise an exception
    // - only a single registration is memorized: once raised, any further
    // method execution would continue as usual
    // - optional Exception.Message which should be raised with the exception
    // - also optional exception class instead of default EDDDMockedSocket
    // - msaDataOutReturnsFalse won't raise any exception, but let DataOut
    // method return false (which is the normal way of indicating a socket
    // error) - in this case, ExceptionMessage would be available from LastError
    // - msaDataInPendingTimeout won't raise any exception, but let DataInPending
    // sleep for the timeout period, and return 0
    // - msaDataInPendingFails won't raise any exception, but let DataInPending
    // fails immediately, and return -1 (emulating a broken socket)
    // - you may use ALL_DDDMOCKED_EXCEPTIONS to set all possible actions
    // - you could reset any previous registered exception by calling
    // ! MockException([]);
    procedure MockException(NextActions: TDDDMockedSocketExceptions;
      const ExceptionMessage: string = ''; ExceptionClass: ExceptClass = nil);
    /// will let the specified methods to wait for a given number of milliseconds
    // - allow to emulate network latency, on purpose
    // - you may use ALL_DDDMOCKED_LATENCIES to slow down all possible actions
    procedure MockLatency(NextActions: TDDDMockedSocketLatencies; MilliSeconds: integer);
  public 
    /// IDDDSocket method to connect to the host via the mocked socket
    // - won't raise any exception unless ConnectShouldCheckRaiseException is set
    procedure Connect;
    /// IDDDSocket method to return a fake instance identifier
    // - in fact, the hexa pointer of the TDDDMockedSocket instance
    function Identifier: RawUTF8;
    /// IDDDSocket method to  get some low-level information about the last error
    // - i.e. the latest ExceptionMessage value as set by MockException()
    function LastError: RawUTF8;
    /// IDDDSocket method to return the number of bytes pending
    // - note that the total length of all pending data is returned as once,
    // i.e. all previous calls to MockDataIn() would be sum as a single count
    // - this method will emulate blocking process, just like a regular socket:
    // if there is no pending data, it will wait up to aTimeOut milliseconds
    function DataInPending(aTimeOut: integer): integer;
    /// IDDDSocket method to get Length bytes from the mocked socket
    // - returns the number of bytes read into the Content buffer
    // - note that all pending data is returned as once, i.e. all previous
    // calls to MockDataIn() would be gathered in a single buffer
    function DataIn(Content: PAnsiChar; ContentLength: integer): integer;
    /// IDDDSocket method to send Length bytes to the mocked socket
    // - returns false on any error, true on success
    // - then MockDataOut could be used to retrieve the sent data
    function DataOut(Content: PAnsiChar; ContentLength: integer): boolean;
    /// returns 0 (no associated file descriptor)
    function Handle: integer;
    /// read-only access to the associated processing thread
    // - not published, to avoid stack overflow since TDDDSocketThreadMonitoring
    // would point to this instance
    property Owner: TThread read fOwner;
  published
    /// how many bytes are actually in the internal input buffer
    property PendingInBytes: integer read GetPendingInBytes;
    /// how many bytes are actually in the internal output buffer
    property PendingOutBytes: integer read GetPendingOutBytes;
  end;

  /// a generic TThread able to connect and reconnect to a TCP server
  // - initialize and own a TCrtSocket instance for TCP transmission
  // - allow automatic reconnection
  // - inherit from TSQLRestThread, so should be associated with a REST instance
  TDDDSocketThread = class(TSQLRestThread)
  protected
    fSettings: TDDDSocketThreadSettings;
    fMonitoring: TDDDSocketThreadMonitoring;
    fPreviousMonitorTix: Int64;
    fSocket: IDDDSocket;
    fPerformConnection: boolean;
    fHost, fPort: SockString;
    fSocketInputBuffer: RawByteString;
    fShouldDisconnect: boolean;
    fSocketDisable: boolean;
    procedure InternalExecute; override;
    procedure ExecuteConnect;
    procedure ExecuteDisconnect;
    procedure ExecuteDisconnectAfterError;
    procedure ExecuteSocket;
    function TrySend(const aFrame: RawByteString;
      ImmediateDisconnectAfterError: boolean = true): Boolean; virtual;
    // inherited classes could override those methods for process customization
    procedure InternalExecuteConnected; virtual;
    procedure InternalExecuteDisconnect; virtual;
    procedure InternalExecuteIdle; virtual;
    procedure InternalExecuteSocket; virtual; abstract; // process FSocketInputBuffer
    procedure InternalLogMonitoring; virtual;
  public
    /// initialize the thread for a given REST instance
    constructor Create(aSettings: TDDDSocketThreadSettings; aRest: TSQLRest;
      aMonitoring: TDDDSocketThreadMonitoring);
    /// finalize the thread process, and its associted REST instance
    destructor Destroy; override;
    /// returns the Monitoring and Rest statistics as a JSON object
    // - resulting format is
    // $ {...MonitoringProperties...,"Rest":{...RestStats...}}
    function StatsAsJson: RawUTF8;
    /// will pause any communication with the associated socket
    // - could be used before stopping the service for cleaner shutdown
    procedure Shutdown(andTerminate: boolean); virtual;
    /// the parameters used to setup this thread process
    property Settings: TDDDSocketThreadSettings read fSettings;
  published
    /// the IP Host name used to connect with TCP
    property Host: SockString read fHost;
    /// the IP Port value used to connect with TCP
    property Port: SockString read fPort;
  end;

const
  /// map realistic exceptions steps for a mocked socket
  // - could be used to simulate a global socket connection drop
  ALL_DDDMOCKED_EXCEPTIONS = [msaConnectRaiseException, msaDataInPendingFails,
    msaDataInRaiseException, msaDataOutReturnsFalse];

  /// map realistic latencies steps for a mocked socket
  // - could be used to simulate a slow network
  ALL_DDDMOCKED_LATENCIES = [Low(TDDDMockedSocketLatency)..high(TDDDMockedSocketLatency)];

  /// a Mustache template of a .rc version information
  // - could be used to compile a custom .res version file in an automated way
  // - if you use this generated .res, ensure your "Version Info" is disabled
  // (unchecked) in the Delphi IDE project options
  EXEVERSION_RCTEMPLATE: RawUTF8 =
  '1 VERSIONINFO'#13#10 +
  'FILEVERSION {{maj}},{{min}},{{rel}},{{build}}'#13#10 +
  'PRODUCTVERSION {{maj}},{{min}},{{rel}},{{build}}'#13#10 +
  'FILEOS 0x4'#13#10 +
  'FILETYPE 0x1'#13#10 +
  '{'#13#10 +
  'BLOCK "StringFileInfo"'#13#10 +
  '{'#13#10 +
  '	BLOCK "040904E4"'#13#10 +
  '	{'#13#10 +
  '		VALUE "CompanyName", "{{compname}}\0"'#13#10 +
  '		VALUE "FileDescription", "{{compname}}, {{product}} {{name}}{{#isDaemon}} Daemon{{/isDaemon}}\0"'#13#10 +
  '		VALUE "FileVersion", "{{maj}}.{{min}}.{{rel}}.{{build}}\0"'#13#10 +
  '		VALUE "InternalName", "{{name}}\0"'#13#10 +
  '		VALUE "LegalCopyright", "(c){{year}} {{compname}}\0"'#13#10 +
  '		VALUE "LegalTrademarks", "All rights reserved to {{compname}}\0"'#13#10 +
  '		VALUE "OriginalFilename", "{{name}}\0"'#13#10 +
  '		VALUE "ProductName", "{{product}} {{name}}\0"'#13#10 +
  '		VALUE "ProductVersion", "{{maj}}.{{min}}.{{rel}}.{{build}}\0"'#13#10 +
  '	}'#13#10 +
  '}'#13#10 +
  #13#10 +
  'BLOCK "VarFileInfo"'#13#10 +
  '{'#13#10 +
  '	VALUE "Translation", 0x0409 0x04E4'#13#10 +
  '}'#13#10 +
  '}';

var
  /// you could set a text to this global variable at runtime, so that
  // it would be displayed as copyright older name for the console
  GlobalCopyright: string = '';

function ToText(st: TDDDSocketThreadState): PShortString; overload;
function ToText(exc: TDDDMockedSocketException): PShortString; overload;


{ ----- Applications Securization }

type
  /// result codes of the ECCAuthorize() function
  TECCAuthorize = (eaSuccess, eaInvalidSecret, eaMissingUnlockFile, 
    eaInvalidUnlockFile, eaInvalidJson);

/// any sensitive, or licensed program, could call this method to check for
// authorized execution for a given user on a given computer, using very secure
// asymmetric ECC cryptography
// - applock.public/.private keys pair should have been generated, applock.public
// stored as aAppLockPublic64 in the executables, and applock.private kept secret
// - will search for encrypted authorization in a local user@host.unlock file
// - if no user@host.unlock file is found, will create local user@host.public
// and user@host.secret files and return eaMissingUnlockFile: user should then send
// user@host.public to the product support to receive its user@host.unlock file
// (a dedicated UI may be developped, or an uncrypted email can be used for
// transfer with the support team, thanks to asymmetric cryptography)
// - local user@host.secret file is encrypted via DPAPI/CryptDataForCurrentUser
// for the specific computer and user (to avoid .unlock reuse on another PC)
// - support team should create a user@host.json file matching aContent: TObject
// published properties, containing all application-specific settings and
// authorization scope; then it could create the unlock file using e.g. an
// unlock.bat file running the ECC tool over secret applock.private keys:
// $ @echo off
// $ echo Usage:  unlock user@host
// $ echo.
// $ ecc sign -file %1.json -auth applock -pass applockprivatepassword -rounds 60000
// $ ecc crypt -file %1.json -out %1.unlock -auth %1 -saltpass decryptsalt -saltrounds 10000
// $ del %1.json.sign
// - returns eaInvalidUnlockFile if the local user@host.unlock file is not
// correctly signed and encrypted for this user (e.g. corrupted or deprecated)
// - eaInvalidJson will indicate some error in the .json created by support team,
// i.e. if it does not match aContent: TObject published properties
// - eaSuccess should let the application execute, on the returned scope
// - returns eaSuccess if a local user@host.unlock file has been successfully
// decrypted and validated (using ECDSA over aAppLockPublic64) and successfully
// unserialized from JSON into aContent object instance
// - user@host.* files are searched in the executable folder if aSearchFolder='',
// but you may specify a custom location, e.g. use ECCKeyFileFolder
// - aSecretPass could be entered by the end-user, to authenticate its identity;
// you may specify a string constant if local applock.public/.private key files
// is enough secure for your application
// - will use the supplied aDPAPI/aDecryptSalt parameters to restrict
// this authorization to a specific product (i.e. isolate the execution context
// to reduce forensic scope), for dedicated applock.public/.private keys pair -
// just pass some application-specific string constant to those parameters
// - aSecretInfo^ could be set to retrieve the user@host.secret information
// (e.g. validity dates), and aLocalFile^ the'<fullpath>user@host' file prefix 
function ECCAuthorize(aContent: TObject; aSecretDays: integer; const aSecretPass,
  aDPAPI, aDecryptSalt, aAppLockPublic64: RawUTF8; const aSearchFolder: TFileName = '';
  aSecretInfo: PECCCertificateSigned = nil; aLocalFile: PFileName = nil): TECCAuthorize;

function ToText(auth: TECCAuthorize): PShortString; overload;


implementation

{ ----- Implements Service/Daemon Applications }

function ToText(st: TDDDSocketThreadState): PShortString;
begin
  result := GetEnumName(TypeInfo(TDDDSocketThreadState),ord(st));
end;

function ToText(exc: TDDDMockedSocketException): PShortString;
begin
  result := GetEnumName(TypeInfo(TDDDMockedSocketException),ord(exc));
end;


{ TDDDDaemon }

constructor TDDDDaemon.Create(aSettings: TDDDAdministratedDaemonSettings);
begin
  inherited Create;
  if aSettings = nil then
    raise EDDDInfraException.CreateUTF8('%.Create(settings=nil)', [self]);
  fSettings := aSettings;
  fSettingsRef := aSettings;
end;

function TDDDDaemon.DaemonInstance: TObject;
begin
  result := ObjectFromInterface(fDaemon);
end;

destructor TDDDDaemon.Destroy;
begin
  fDaemon := nil;
  inherited;
end;

procedure TDDDDaemon.DoStart{$ifdef MSWINDOWS}(Sender: TService){$endif};
var log: ISynLog;
    res: TCQRSResult;
begin
  {$ifdef WITHLOG}
  {$ifdef MSWINDOWS}
  SQLite3Log.Add.LogThreadName('Service Start Handler', true);
  {$endif}
  log := SQLite3Log.Enter(self, 'DoStart');
  if log<>nil then
    with ExeVersion do
      log.Log(sllNewRun, 'Daemon Start svc=% ver=% usr=%',
        [fSettings.ServiceName, Version.Detailed, LowerCase(User)], self);
  {$endif}
  fDaemon := NewDaemon;
  res := fDaemon.Start;
  if log <> nil then
    log.Log(sllTrace, 'fDaemon.Start=%', [ToText(res)^], self);
end;

procedure TDDDDaemon.DoStop{$ifdef MSWINDOWS}(Sender: TService){$endif};
{$ifdef WITHLOG}
var log: ISynLog;
begin
  {$ifdef MSWINDOWS}
  SQLite3Log.Add.LogThreadName('Service Stop Handler', true);
  {$endif}
  log := SQLite3Log.Enter(self, 'DoStop');
  if log<>nil then
    log.Log(sllNewRun, 'Daemon Stop svc=% ver=% usr=%', [fSettings.ServiceName,
      ExeVersion.Version.Detailed, LowerCase(ExeVersion.User)], self);
{$else}
begin
{$endif}
  fDaemon := nil; // will stop the daemon
end;

function TDDDDaemon.NewDaemon: TDDDAdministratedDaemon;
begin
  if Assigned(fSettings) then begin
    if fSettings.Log.LowLevelWebSocketsFrames then begin
      {$ifdef WITHLOG}
      WebSocketLog := SQLite3Log;
      {$endif}
      HttpServerFullWebSocketsLog := true;
      HttpClientFullWebSocketsLog := true;
    end;
    SQLite3Log.Family.AutoFlushTimeOut := fSettings.Log.AutoFlushTimeOut; // after /fork
    {$ifdef MSWINDOWS} // Windows 7+
    SetAppUserModelID(fSettings.AppUserModelID);
    {$endif}
  end;
  result := nil;
end;

procedure TDDDDaemon.Execute;
{$ifdef WITHLOG}
var log: ISynLog;
begin
  log := SQLite3Log.Enter(self, 'Execute');
{$else}
begin
{$endif}
  fDaemon := NewDaemon;
  fDaemon.Start;
end;

function TDDDDaemon.WaitStarted(TimeoutMS: integer): boolean;
var
  tix: Int64;
begin
  tix := GetTickCount64 + TimeoutMS;
  repeat
    SleepHiRes(50);
    result := Daemon <> nil;
  until result or (GetTickCount64 > tix);
end;

function TDDDDaemon.ExecuteAndWaitStarted(TimeoutMS: integer): boolean;
begin
  try
    Execute;
    result := WaitStarted(TimeoutMS);
  except
    result := false;
  end;
end;

type
  TExecuteCommandLineCmd = (
    cNone, cInstall, cUninstall, cStart, cStop, cState,
    cVersion, cVerbose, cHelp, cHardenPasswords, cPlainPasswords,
    cConsole, cDaemon, cRun, cFork, cKill);

procedure TDDDDaemon.ExecuteCommandLine(ForceRun: boolean);
var
  name, param: RawUTF8;
  p: PUTF8Char;
  passwords, exe: RawByteString;
  cmd: TExecuteCommandLineCmd;
  daemon: TDDDAdministratedDaemon;
  {$ifdef MSWINDOWS}
  service: TServiceSingle;
  ctrl: TServiceController;
  depend: string;
  i: integer;
  {$endif}
{$I-} // no IO error for writeln() below

  function cmdText: RawUTF8;
  begin
    result := GetEnumNameTrimed(TypeInfo(TExecuteCommandLineCmd), cmd);
  end;

  procedure HardenSettings(const folder: TFileName; P: PUTF8Char);
  var B: PUTF8Char;
      fn, bak: TFileName;
      pass, appsec, plain, new: RawUTF8;
      doc: TDocVariantData;
      modified, any: boolean;
      v: PVariant;
  begin
    TextColor(ccLightCyan);
    writeln('Executing /', cmdText, ' with User "', ExeVersion.User, '"'#13#10);
    any := false;
    repeat
      B := pointer(GetNextLine(P,P));
      if B=nil then
        exit;
      fn := FormatString('%%.settings', [folder, GetNextItem(B, '=')]);
      doc.InitJSONFromFile(fn, JSON_OPTIONS_FAST, true);
      modified := false;
      if doc.Count > 0 then
        while B <> nil do begin
          GetNextItem(B, '@', appsec);
          v := doc.GetPVariantByPath(GetNextItem(B));
          if v = nil then
            continue;
          if VariantToUTF8(v^, pass) and (pass <> '') then begin
            plain := TSynPersistentWithPassword.ComputePlainPassword(pass, 0, appsec);
            if plain = '' then
              continue; // may occur with unexpected user
            case cmd of
            cHardenPasswords:
              new := FormatUTF8('%:%', [ExeVersion.User,
                BinToBase64(CryptDataForCurrentUser(plain, appsec, true))]);
            cPlainPasswords:
              new := TSynPersistentWithPassword.ComputePassword(plain);
            else
              exit;
            end;
            FillZero(plain);
            if new <> pass then begin
              RawUTF8ToVariant(new, v^); // replace
              modified := true;
            end;
          end;
        end;
      if modified then begin
        bak := ChangeFileExt(fn, '.bak');
        DeleteFile(bak);
        RenameFile(fn, bak);
        FileFromString(doc.ToJSON('', '', jsonHumanReadable), fn);
        any := true;
        TextColor(ccLightCyan);
      end
      else
        TextColor(ccDarkGray);
      writeln('  ', fn);
      doc.Clear;
    until P=nil;
    if any and (cmd = cHardenPasswords) then begin
      TextColor(ccYellow);
      writeln(#13#10' Warning:'#13#10' /', cmdText,
        ' new .settings will work only with user "', ExeVersion.User, '"');
    end;
  end;

  procedure Show(Success: Boolean);
  var
    msg: RawUTF8;
    error: integer;
  begin
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
    msg := FormatUTF8('% [%] (%) on Service [%]',
      [msg, param, cmdText, fSettings.ServiceName]);
    writeln(msg);
    AppendToTextFile(ExeVersion.User + ' ' +msg,
      ChangeFileExt(ExeVersion.ProgramFileName, '.txt'));
  end;

  procedure Syntax;
  var spaces: string;
  begin
    writeln('Try with one of the switches:');
    writeln({$ifdef MSWINDOWS}'   '{$else}' ./'{$endif}, ExeVersion.ProgramName,
      ' /console -c /verbose /daemon -d /help -h /version');
    spaces := StringOfChar(' ', length(ExeVersion.ProgramName) + 4);
    {$ifdef MSWINDOWS}
    writeln(spaces, '/install /uninstall /start /stop /state');
    {$else}
    writeln(spaces, '/run -r /fork -f /kill -k');
    {$endif}
    if passwords <> '' then
      writeln(spaces, '/hardenpasswords /plainpasswords');
    writeln(spaces, CustomHelp);
  end;

begin
  try
    if fSettings.ServiceDisplayName = '' then begin
      fDaemon := NewDaemon; // should initialize the default .settings
      fDaemon := nil;
    end;
    ResourceSynLZToRawByteString('passwords', passwords);
    TextColor(ccLightGreen);
    name := StringToUTF8(fSettings.ServiceDisplayName);
    if name = '' then // perhaps the settings file is still void
      name := ExeVersion.ProgramName;
    if ExeVersion.Version.Version32 <> 0 then
      name := FormatUTF8('% %', [name, ExeVersion.Version.Detailed]);
    writeln(#10' ', name);
    writeln(StringOfChar('-', length(name) + 2));
    TextColor(ccGreen);
    if fSettings.Description <> '' then
      writeln(fSettings.Description);
    if GlobalCopyright <> '' then
      writeln('(c)', CurrentYear, ' ', GlobalCopyright);
    writeln;
    TextColor(ccLightCyan);
    if ForceRun then
      param := '/verbose' else
      param := trim(StringToUTF8(paramstr(1)));
    if (param = '') or not (param[1] in ['/', '-']) then
      cmd := cNone
    else begin
      p := @param[2];
      if p^ = '-' then // allow e.g. --fork switch (idem to /f -f /fork -fork)
        inc(p);
      case NormToUpper[p^] of
        'C':
          cmd := cConsole;
        'D':
          cmd := cDaemon;
        'R':
          cmd := cRun;
        'F':
          cmd := cFork;
        'K':
          cmd := cKill;
      else
        byte(cmd) := ord(cInstall) + IdemPCharArray(p,
          ['INST', 'UNINST', 'START', 'STOP', 'STAT', 'VERS', 'VERB', 'HELP',
           'HARDEN', 'PLAIN']);
      end;
    end;
    case cmd of
      cHelp:
        Syntax;
      cVersion:
        begin
          exe := StringFromFile(ExeVersion.ProgramFileName);
          writeln(' ', ExeVersion.ProgramFileName,
            #13#10' Size: ', length(exe), ' bytes (', KB(exe), ')' +
            #13#10' Build date: ', ExeVersion.Version.BuildDateTimeString,
            #13#10' MD5: ', MD5(exe),
            #13#10' SHA256: ', SHA256(exe));
          if ExeVersion.Version.Version32 <> 0 then
            writeln(' Version: ', ExeVersion.Version.Detailed);
          TextColor(ccCyan);
          writeln(#10'Powered by Synopse mORMot ' + SYNOPSE_FRAMEWORK_VERSION);
        end;
      cConsole, cDaemon, cVerbose:
        begin
          writeln('Launched in ', cmdText, ' mode'#10);
          TextColor(ccLightGray);
          {$ifdef WITHLOG}
          case cmd of
            cVerbose: // leave as in settings for -c (cConsole)
              SQLite3Log.Family.EchoToConsole := LOG_VERBOSE;
          end;
          SQLite3Log.Add.Log(sllNewRun, 'Start % /% %', [fSettings.ServiceName,cmdText,
            ExeVersion.Version.DetailedOrVoid], self);
          {$endif}
          daemon := NewDaemon;
          try
            fDaemon := daemon;
            {$ifdef WITHLOG}
            if cmd = cDaemon then
              if (daemon.AdministrationServer = nil) or not ({$ifdef MSWINDOWS}
                daemon.AdministrationServer.ExportedAsMessageOrNamedPipe or {$endif}
                (daemon.InheritsFrom(TDDDThreadDaemon) and
                 (TDDDThreadDaemon(daemon).fAdministrationHTTPServer <> nil))) then
                daemon.Log.Synlog.Log(sllWarning, 'ExecuteCommandLine as Daemon ' +
                  'without external admnistrator acccess', self);
            {$endif}
            daemon.Execute(cmd = cDaemon);
          finally
            SQLite3Log.Add.Log(sllNewRun, 'Stop /%', [cmdText], self);
            fDaemon := nil; // will stop the daemon
          end;
        end;
      cHardenPasswords, cPlainPasswords:
        if passwords <> ''  then
          HardenSettings(IncludeTrailingPathDelimiter(fSettings.SettingsFolder),
            pointer(passwords))
        else begin
          TextColor(ccLightRed);
          writeln('No "passwords" resource bound to ', ExeVersion.ProgramFullSpec);
        end;
    {$ifdef MSWINDOWS} // implement the daemon as a Windows Service
    else with fSettings do
        if ServiceName = '' then
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
                service := TServiceSingle.Create(ServiceName, ServiceDisplayName);
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
              begin
                if ServiceDependencies <> nil then begin
                  depend := ServiceDependencies[0];
                  for i := 1 to high(ServiceDependencies) do
                    depend := depend + #0 + ServiceDependencies[i];
                end else if (ParamCount >= 3) and SameText(ParamStr(2), '/depend') then begin
                  depend := ParamStr(3);
                  for i := 4 to ParamCount do
                    depend := depend + #0 + ParamStr(i);
                end;
                if depend <> '' then
                  depend := depend + #0; // ensure ends with dual #0
                Show(TServiceController.Install(ServiceName, ServiceDisplayName,
                  Description, ServiceAutoStart, '', depend) <> ssNotInstalled);
              end;
          else
            begin
              ctrl := TServiceController.CreateOpenService('', '', ServiceName);
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
                    writeln(ServiceName, ' State=', ServiceStateText(ctrl.State));
                else
                  Show(false);
                end;
              finally
                ctrl.Free;
              end;
            end;
          end;
    {$else}
    cRun, cFork:
      RunUntilSigTerminated(self, (cmd=cFork), DoStart, DoStop
        {$ifdef WITHLOG},SQLite3Log.Add, fSettings.ServiceName{$endif});
    cKill:
      if not RunUntilSigTerminatedForKill then
        raise EServiceException.Create('No forked process found to be killed');
    else
      Syntax;
    {$endif MSWINDOWS}
    end;
  except
    on E: Exception do begin
      ConsoleShowFatalException(E);
      ExitCode := 1; // indicates error
    end;
  end;
  TextColor(ccLightGray);
  ioresult;
end;
{$I+}

function TDDDDaemon.CustomHelp: string;
begin
  result := '';
end;


{ TDDDThreadDaemon }

function TDDDThreadDaemon.GetAdministrationHTTPServer: TSQLHttpServer;
begin
  result := fAdministrationHTTPServer as TSQLHttpServer;
end;

function TDDDThreadDaemon.InternalRetrieveState(var Status: variant): boolean;
begin
  Status := _ObjFast(['SystemMemory', TSynMonitorMemory.ToVariant]);
  result := true;
end;

function CanSubscribeLog(const Callback: ISynLogCallback): Boolean;
begin
  result := false;
  if Assigned(Callback) then
    if {$ifdef WITHLOG}(WebSocketLog<>nil) or{$endif}
       HttpClientFullWebSocketsLog or HttpServerFullWebSocketsLog then begin
      Callback.Log(sllError, FormatUTF8(
        '%00%  SubscribeLog is not allowed when low-level WebSockets ' +
        'frame logging is enabled (otherwise a race condition happens)',
        [NowToString(False), LOG_LEVEL_TEXT[sllError]]));
      exit;
    end
    else
      result := true;
end;

procedure TDDDThreadDaemon.SubscribeLog(const Levels: TSynLogInfos;
  const Callback: ISynLogCallback; ReceiveExistingKB: cardinal);
begin
  if CanSubscribeLog(Callback) then
    inherited SubscribeLog(Levels, Callback, ReceiveExistingKB);
end;


{ TDDDRestDaemon }

function TDDDRestDaemon.GetAdministrationHTTPServer: TSQLHttpServer;
begin
  result := TSQLHttpServer(fAdministrationHTTPServer);
end;

procedure TDDDRestDaemon.InternalLogMonitoring;
var
  status: variant;
begin
  {$ifdef WITHLOG}
  if fLog <> nil then
    if (sllMonitoring in fLog.Level) and InternalRetrieveState(status) then
      fLog.SynLog.Log(sllMonitoring, '%', [status], Self);
  {$endif}
end;

function TDDDRestDaemon.InternalRetrieveState(var Status: variant): boolean;
begin
  if fRest <> nil then begin
    Status := _ObjFast(['Rest', fRest.StatsAsDocVariant,
      'SystemMemory', TSynMonitorMemory.ToVariant]);
    result := true;
  end
  else
    result := false;
end;

procedure TDDDRestDaemon.SubscribeLog(const Levels: TSynLogInfos;
  const Callback: ISynLogCallback; ReceiveExistingKB: cardinal);
begin
  if CanSubscribeLog(Callback) then
    inherited SubscribeLog(Levels, Callback, ReceiveExistingKB);
end;


{ TDDDRestHttpDaemon }

function TDDDRestHttpDaemon.Settings: TDDDAdministratedDaemonHttpSettings;
begin
  result := TDDDAdministratedDaemonHttpSettings(fInternalSettings);
end;

procedure TDDDRestHttpDaemon.InternalStart;
begin
  if Settings.Http.BindPort <> '' then
    fHttpServer := TSQLHttpServer.Create(fRest, Settings.Http);
end;

procedure TDDDRestHttpDaemon.InternalStop;
begin
  try
    if Assigned(fRest) then
      fRest.Shutdown; // no incoming TSQLRestServer.URI allowed from now on
    FreeAndNil(fHttpServer);
    FreeAndNil(fServicesLogRest);
  finally
    inherited InternalStop; // FreeAndNil(fRest)
  end;
end;


{ ----- Implements Thread Processing to access a TCP server }

procedure TDDDRestHttpDaemon.WrapperGenerate(const DestFile, Template: TFileName);
begin
  Settings.Rest.WrapperGenerate(Rest, GetInteger(pointer(HttpServer.Port)), DestFile, Template);
end;


{ TDDDSocketThreadMonitoring }

function TDDDSocketThreadMonitoring.GetSocket: variant;
begin
  if (fOwner = nil) or not fOwner.InheritsFrom(TDDDSocketThread) or
     (TDDDSocketThread(fOwner).fSocket = nil) then
    SetVariantNull(result)
  else
    ObjectToVariant(ObjectFromInterface(TDDDSocketThread(fOwner).fSocket), result);
end;


{ TDDDSocketThread }

constructor TDDDSocketThread.Create(aSettings: TDDDSocketThreadSettings;
  aRest: TSQLRest; aMonitoring: TDDDSocketThreadMonitoring);
begin
  if aSettings = nil then
    raise EDDDInfraException.CreateUTF8('%.Create(aSettings=nil)', [self]);
  fSettings := aSettings;
  if aMonitoring = nil then
    raise EDDDInfraException.CreateUTF8('%.Create(aMonitoring=nil)', [self]);
  fMonitoring := aMonitoring;
  if fMonitoring.fOwner=nil then
    fMonitoring.fOwner := self;
  if fSettings.Host = '' then
    fSettings.Host := '127.0.0.1';
  fHost := fSettings.Host;
  if fSettings.Port = 0 then
    raise EDDDInfraException.CreateUTF8('%.Create(Port=0)', [self]);
  fPort := UInt32ToUtf8(fSettings.Port);
  if fSettings.SocketLoopPeriod < 10 then
    fSettings.SocketLoopPeriod := 10;
  if fSettings.SocketTimeout < fSettings.SocketLoopPeriod then
    fSettings.SocketTimeout := 2000;
  fPerformConnection := true;
  inherited Create(aRest, true, true); // aOwnRest=true, aCreateSuspended=true
end;

destructor TDDDSocketThread.Destroy;
var
  timeOut: Int64;
begin
  {$ifdef WITHLOG}
  if fLog<>nil then // no log after NotifyThreadEnded call
    fLog.Family.SynLog.Log(sllTrace,'Destroy %:%',[fHost,fPort],self);
  {$endif}
  Terminate;
  timeOut := GetTickCount64 + 10000;
  repeat // wait until properly disconnected from remote TCP server
    SleepHiRes(10);
  until (fMonitoring.State = tpsDisconnected) or (GetTickCount64 > timeOut);
  inherited Destroy;
  if fMonitoring.fOwner=self then
    FreeAndNil(fMonitoring);
end;

procedure TDDDSocketThread.ExecuteConnect;
{$ifdef WITHLOG}
var log: ISynLog;
begin
  log := FLog.Enter('ExecuteConnect %:%',[fHost,fPort],self);
{$else}
begin
{$endif}
  if fSocket <> nil then
    raise EDDDInfraException.CreateUTF8('%.ExecuteConnect: fSocket<>nil', [self]);
  if fMonitoring.State <> tpsDisconnected then
    raise EDDDInfraException.CreateUTF8('%.ExecuteConnect: State=%',
      [self, ToText(fMonitoring.State)^]);
  fMonitoring.State := tpsConnecting;
  try
    if fSettings.SocketBufferBytes <= 0 then
      fSettings.SocketBufferBytes := 32768;
    if Assigned(fSettings.OnIDDDSocketThreadCreate) then
      fSettings.OnIDDDSocketThreadCreate(self, fSocket)
    else
      fSocket := TDDDSynCrtSocket.Create(self, fHost, fPort,
        fSettings.SocketTimeout, fSettings.SocketBufferBytes);
    fSocket.Connect;
    fMonitoring.State := tpsConnected; // to be done ASAP to allow sending
    InternalExecuteConnected;
    {$ifdef WITHLOG}
    if log<>nil then
      log.Log(sllTrace, 'ExecuteConnect: Connected via Socket % - %',
        [fSocket.Identifier, fMonitoring], self);
    {$endif}
  except
    on E: Exception do begin
      fMonitoring.ProcessException(E);
      {$ifdef WITHLOG}
      if log<>nil then
        log.Log(sllTrace, 'ExecuteConnect: Impossible to Connect to %:% (%) %',
          [Host, Port, E.ClassType, fMonitoring], self);
      {$endif}
      fSocket := nil;
      fMonitoring.State := tpsDisconnected;
    end;
  end;
  if (fMonitoring.State <> tpsConnected) and not Terminated then
    if fSettings.ConnectionAttemptsInterval < 0 then
      Terminate
    else if fSettings.ConnectionAttemptsInterval > 0 then // on error, retry
      if SleepOrTerminated(fSettings.ConnectionAttemptsInterval * 1000) then
      {$ifdef WITHLOG} begin
        if log<>nil then
          log.Log(sllTrace, 'ExecuteConnect: thread terminated', self)
      end
      else if log<>nil then
        log.Log(sllTrace, 'ExecuteConnect: wait finished -> retry connect', self)
      {$endif};
end;

procedure TDDDSocketThread.ExecuteDisconnect;
var
  info: RawUTF8;
{$ifdef WITHLOG}
  log: ISynLog;
begin
  log := FLog.Enter('ExecuteDisconnect %:%',[fHost,fPort],self);
{$else}
begin
{$endif}
  try
    fSafe.Lock;
    try
      fShouldDisconnect := false;
      fMonitoring.State := tpsDisconnected;
      try
        if fSocket = nil then
          info := '[Unknown]'
        else
          info := fSocket.Identifier;
        InternalExecuteDisconnect;
      finally
        fSocket := nil;
      end;
      {$ifdef WITHLOG}
      if log<>nil then
        log.Log(sllTrace, 'Socket % disconnected', [info], self);
      {$endif}
      InternalLogMonitoring;
    finally
      fSafe.UnLock;
    end;
  except
    on E: Exception do begin
      fMonitoring.ProcessException(E);
      {$ifdef WITHLOG}
      if log<>nil then
        log.Log(sllTrace, 'Socket disconnection error (%)', [E.ClassType], self);
      {$endif}
    end;
  end;
end;

procedure TDDDSocketThread.ExecuteDisconnectAfterError;
begin
  {$ifdef WITHLOG}
  if fSocket <> nil then
    FLog.Log(sllError, '%.ExecuteDisconnectAfterError: Sock=% LastError=%',
      [ClassType, fSocket.Identifier, fSocket.LastError], self);
  {$endif}
  ExecuteDisconnect;
  FSocketInputBuffer := '';
  if fSettings.AutoReconnectAfterSocketError then
    FPerformConnection := true;
end;

procedure TDDDSocketThread.ExecuteSocket;
var
  pending, len: integer;
begin
  if Terminated or fSocketDisable then
    exit;
  if (fSettings.SocketMaxBufferBytes = 0) or
     (length(FSocketInputBuffer) < fSettings.SocketMaxBufferBytes) then begin
    pending := fSocket.DataInPending(fSettings.SocketLoopPeriod);
    if Terminated or (pending = 0) then
      exit;
    if pending < 0 then begin
      ExecuteDisconnectAfterError;
      exit;
    end;
    len := length(FSocketInputBuffer);
    SetLength(FSocketInputBuffer, len + pending);
    if fSocket.DataIn(@PByteArray(FSocketInputBuffer)[len], pending) <> pending then begin
      ExecuteDisconnectAfterError;
      exit;
    end;
    FMonitoring.Server.AddSize(pending, 0);
  end;
  InternalExecuteSocket;
end;

procedure TDDDSocketThread.InternalExecute;
begin
  fPreviousMonitorTix := GetTickCount64;
  try
    repeat
      if fMonitoring.State = tpsConnected then
        ExecuteSocket
      else if fPerformConnection then
        ExecuteConnect
      else
        SleepHiRes(100);
      if Terminated then
        break;
      try
        if Elapsed(fPreviousMonitorTix, fSettings.MonitoringLogInterval) then
          InternalLogMonitoring;
        InternalExecuteIdle;
      except
        on E: Exception do begin
          fMonitoring.ProcessException(E);
          {$ifdef WITHLOG}
          FLog.Log(sllWarning, 'Skipped % exception in %.InternalExecuteIdle',
            [E, ClassType], self);
          {$endif}
        end;
      end;
    until Terminated;
  finally
    ExecuteDisconnect;
  end;
end;

procedure TDDDSocketThread.InternalExecuteConnected;
begin
end;

procedure TDDDSocketThread.InternalExecuteDisconnect;
begin
end;

procedure TDDDSocketThread.InternalExecuteIdle;
begin
  fSafe.Lock;
  try
    if fShouldDisconnect then
      ExecuteDisconnectAfterError;
  finally
    fSafe.UnLock;
  end;
end;

procedure TDDDSocketThread.InternalLogMonitoring;
var
  cached, flushed: integer;
begin // CachedMemory method will also purge any outdated cached entries
  cached := fRest.CacheOrNil.CachedMemory(@flushed);
  {$ifdef WITHLOG}
  FLog.Log(sllMonitoring, '% CachedMemory=% Flushed=%',
    [FMonitoring, cached, flushed], Self);
  {$endif}
  fPreviousMonitorTix := GetTickCount64;
end;

procedure TDDDSocketThread.Shutdown(andTerminate: boolean);
begin
  if (self <> nil) and not fSocketDisable then begin
    fSocketDisable := true;
    fRest.LogClass.Add.Log(sllDebug, 'Shutdown: % will stop any communication ' +
      'with %:%', [ClassType, fHost, fPort], self);
    if andTerminate and not Terminated then begin
      SleepHiRes(100);
      Terminate;
      WaitFor;
    end;
  end;
end;

function TDDDSocketThread.StatsAsJson: RawUTF8;
begin
  with TJSONSerializer.CreateOwnedStream do
  try
    WriteObject(FMonitoring);
    CancelLastChar('}');
    if fRest.InheritsFrom(TSQLRestServer) then begin
      AddShort(',"Rest":');
      AddNoJSONEscapeUTF8(TSQLRestServer(fRest).StatsAsJson);
    end;
    Add(',"Version":"%","DateTime":"%"}',
      [ExeVersion.Version.Detailed, NowUTCToString(True, 'T')]);
    SetText(result);
  finally
    Free;
  end;
end;

function TDDDSocketThread.TrySend(const aFrame: RawByteString;
  ImmediateDisconnectAfterError: boolean): Boolean;
var
  tmpSock: IDDDSocket; // avoid GPF if fSocket=nil after fSafe.UnLock (unlikely)
begin
  if fSocketDisable then begin
    result := false;
    exit;
  end;
  fSafe.Lock;
  try
    result := (aFrame <> '') and (fSocket <> nil) and
      (fMonitoring.State = tpsConnected) and not fShouldDisconnect;
    if result then
      tmpSock := fSocket;
  finally
    fSafe.UnLock;
  end;
  if not result then
    exit;
  result := tmpSock.DataOut(pointer(aFrame), length(aFrame));
  if result then
    fMonitoring.Server.AddSize(0, length(aFrame))
  else if ImmediateDisconnectAfterError then
    ExecuteDisconnectAfterError
  else begin
    fSafe.Lock;
    try
      fShouldDisconnect := true; // notify for InternalExecuteIdle
    finally
      fSafe.UnLock;
    end;
  end;
end;


{ TDDDSynCrtSocket }

constructor TDDDSynCrtSocket.Create(aOwner: TThread; const aHost, aPort: SockString;
  aSocketTimeout, aInternalBufferSize: integer);
begin
  inherited Create;
  fOwner := aOwner;
  fHost := aHost;
  fPort := aPort;
  fSocket := TCrtSocket.Create(aSocketTimeout);
  if aInternalBufferSize < 512 then
    aInternalBufferSize := 512;
  fInternalBufferSize := aInternalBufferSize;
  fOutput.Init; // uses two locks to avoid race condition on multi-thread
end;

destructor TDDDSynCrtSocket.Destroy;
begin
  FreeAndNil(fSocket);
  fOutput.Done;
  inherited;
end;

procedure TDDDSynCrtSocket.Connect;
begin
  fSocket.OpenBind(fHost, fPort, False);
  fSocket.CreateSockIn(tlbsCRLF, fInternalBufferSize); // use SockIn safe buffer
end;

function TDDDSynCrtSocket.DataIn(Content: PAnsiChar; ContentLength: integer): integer;
begin
  fSafe.Lock;
  try
    result := fSocket.SockInRead(Content, ContentLength, false);
  finally
    fSafe.UnLock;
  end;
end;

function TDDDSynCrtSocket.DataInPending(aTimeOut: integer): integer;
begin
  fSafe.Lock;
  try
    result := fSocket.SockInPending(aTimeOut,{aPendingAlsoInSocket=}true);
  finally
    fSafe.UnLock;
  end;
end;

function TDDDSynCrtSocket.DataOut(Content: PAnsiChar; ContentLength: integer): boolean;
begin
  fOutput.Lock;
  try
    result := fSocket.TrySndLow(Content, ContentLength);
  finally
    fOutput.UnLock;
  end;
end;

function TDDDSynCrtSocket.Identifier: RawUTF8;
begin
  result := Int32ToUtf8(fSocket.Sock);
end;

function TDDDSynCrtSocket.LastError: RawUTF8;
begin
  if fSocket.LastLowSocketError=0 then
    result := '' else
    StringToUTF8(SocketErrorMessage(fSocket.LastLowSocketError),result);
end;

function TDDDSynCrtSocket.Handle: integer;
begin
  result := fSocket.Sock;
end;


{ TDDDMockedSocket }

constructor TDDDMockedSocket.Create(aOwner: TThread);
begin
  inherited Create;
  fOwner := aOwner;
end;

procedure TDDDMockedSocket.Connect;
begin
  CheckLatency(mslConnect);
  fSafe.Lock;
  try
    CheckRaiseException(msaConnectRaiseException);
  finally
    fSafe.UnLock;
  end;
end;

function TDDDMockedSocket.DataIn(Content: PAnsiChar; ContentLength: integer): integer;
begin
  CheckLatency(mslDataIn);
  fSafe.Lock;
  try
    CheckRaiseException(msaDataInRaiseException);
    result := length(fInput);
    if ContentLength < result then
      result := ContentLength;
    if result <= 0 then
      exit;
    MoveFast(pointer(fInput)^, Content^, result);
    Delete(fInput, 1, result);
  finally
    fSafe.UnLock;
  end;
end;

function TDDDMockedSocket.DataInPending(aTimeOut: integer): integer;
var
  forcedTimeout: boolean;
  endTix: Int64;
begin
  forcedTimeout := false;
  endTix := GetTickCount64 + aTimeOut;
  repeat
    fSafe.Lock;
    try
      CheckRaiseException(msaDataInRaiseException);
      if msaDataInPendingFails in fExceptionActions then begin
        fExceptionActions := [];
        result := -1; // cspSocketError
        exit;
      end;
      if not forcedTimeout then
        if msaDataInPendingTimeout in fExceptionActions then begin
          fExceptionActions := [];
          forcedTimeout := true;
        end;
      if forcedTimeout then
        result := 0
      else
        result := length(fInput);
    finally
      fSafe.UnLock; // wait outside the instance lock
    end;
    if (result <> 0) or ((fOwner <> nil) and TSQLRestThread(fOwner).Terminated)
      or (aTimeOut = 0) then
      break;
    SleepHiRes(1); // emulate blocking process, just like a regular socket
    if (fOwner <> nil) and TSQLRestThread(fOwner).Terminated then
      break;
  until GetTickCount64 > endTix; // warning: 10-16 ms resolution under Windows
end;

function TDDDMockedSocket.DataOut(Content: PAnsiChar; ContentLength: integer): boolean;
var
  previous: integer;
begin
  CheckLatency(mslDataOut);
  fSafe.Lock;
  try
    CheckRaiseException(msaDataOutRaiseException);
    if msaDataOutReturnsFalse in fExceptionActions then begin
      fExceptionActions := [];
      result := false;
      exit;
    end;
    result := true;
    if ContentLength <= 0 then
      exit;
    previous := length(fOutput);
    SetLength(fOutput, previous + ContentLength);
    MoveFast(Content^, PByteArray(fOutput)^[previous], ContentLength);
  finally
    fSafe.UnLock;
  end;
end;

function TDDDMockedSocket.Identifier: RawUTF8;
begin
  result := PointerToHex(self);
end;

function TDDDMockedSocket.LastError: RawUTF8;
begin
  fSafe.Lock;
  try
    StringToUTF8(fExceptionMessage, result);
    fExceptionMessage := '';
  finally
    fSafe.UnLock;
  end;
end;

procedure TDDDMockedSocket.MockDataIn(const Content: RawByteString);
begin
  fSafe.Lock;
  try
    fInput := fInput + Content;
  finally
    fSafe.UnLock;
  end;
end;

function TDDDMockedSocket.MockDataOut: RawByteString;
begin
  fSafe.Lock;
  try
    result := fOutput;
    fOutput := '';
  finally
    fSafe.UnLock;
  end;
end;

procedure TDDDMockedSocket.MockException(NextActions: TDDDMockedSocketExceptions;
  const ExceptionMessage: string; ExceptionClass: ExceptClass);
begin
  fSafe.Lock;
  try
    fExceptionActions := NextActions;
    fExceptionMessage := ExceptionMessage;
    fExceptionClass := ExceptionClass;
  finally
    fSafe.UnLock;
  end;
end;

procedure TDDDMockedSocket.CheckRaiseException(Action: TDDDMockedSocketException);
begin
  if not (Action in fExceptionActions) then
    exit;
  fExceptionActions := [];
  if fExceptionMessage = '' then
    FormatString('Mocked Exception for %', [ToText(Action)^], fExceptionMessage);
  if fExceptionClass = nil then
    fExceptionClass := EDDDMockedSocket;
  raise fExceptionClass.Create(fExceptionMessage);
end;

procedure TDDDMockedSocket.MockLatency(NextActions: TDDDMockedSocketLatencies;
  MilliSeconds: integer);
begin
  fSafe.Lock;
  try
    fLatencyActions := NextActions;
    fLatencyMS := MilliSeconds;
  finally
    fSafe.UnLock;
  end;
end;

procedure TDDDMockedSocket.CheckLatency(Action: TDDDMockedSocketLatency);
var
  waitMS: integer;
begin
  fSafe.Lock;
  try
    if Action in fLatencyActions then
      waitMS := fLatencyMS
    else
      waitMS := 0;
  finally
    fSafe.UnLock; // wait outside the instance lock
  end;
  while (waitMS > 0) and not ((fOwner <> nil) and TSQLRestThread(fOwner).Terminated) do begin
    SleepHiRes(1); // do not use GetTickCount64 (poor resolution under Windows)
    dec(waitMS);
  end;
end;

function TDDDMockedSocket.GetPendingInBytes: integer;
begin
  fSafe.Lock;
  try
    result := Length(fInput);
  finally
    fSafe.UnLock;
  end;
end;

function TDDDMockedSocket.GetPendingOutBytes: integer;
begin
  fSafe.Lock;
  try
    result := Length(fOutput);
  finally
    fSafe.UnLock;
  end;
end;

function TDDDMockedSocket.Handle: integer;
begin
  result := 0;
end;


{ ----- Implements ORM/SOA REST Client access }

{ TDDDRestClientSettings }

function TDDDRestClientSettings.NewRestClientInstance(aRootSettings:
  TDDDAppSettingsAbstract; aModel: TSQLModel; aOptions: TDDDNewRestInstanceOptions): TSQLRestClientURI;
var
  pass: RawUTF8;
begin
  if aModel = nil then
    if riCreateVoidModelIfNone in aOptions then begin
      aModel := TSQLModel.Create([], '');
      include(aOptions, riOwnModel);
    end
    else
      raise EDDDRestClient.CreateUTF8('%.NewRestClientInstance(aModel=nil)', [self]);
  if fClient.Root = '' then // supplied TSQLModel.Root is the default root URI
    fClient.Root := aModel.Root
  else
    aModel.Root := fClient.Root;
  if fORM.Kind = '' then
    fORM.Kind := 'TSQLHttpClientWebsockets'; // assume we need HTTP + callbacks
  if fORM.ServerName = '' then
    fORM.ServerName := 'localhost';
  result := nil;
  try
    try
      result := TSQLRest.CreateTryFrom(aModel, ORM, // will call SetUser()
        riHandleAuthentication in aOptions) as TSQLRestClientURI;
      if result = nil then
        exit; // no match or wrong parameters
      pass := fClient.PasswordPlain;
      if pass <> '' then
        (result as TSQLHttpClientWebsockets).WebSocketsConnect(pass)
      else if not result.ServerTimestampSynchronize then
        raise EDDDRestClient.CreateUTF8('%.Create: HTTP access failure on %/%',
          [self, ORM.ServerName, aModel.Root]);
      result.OnAuthentificationFailed := OnAuthentificationFailed;
    except
      FreeAndNil(result);
    end;
  finally
    if riOwnModel in aOptions then
      if result = nil then // avoid memory leak
        aModel.Free
      else
        aModel.Owner := result;
    if (result = nil) and (riRaiseExceptionIfNoRest in aOptions) then
      raise EDDDRestClient.CreateUTF8('Impossible to initialize % on %/%', [fORM.Kind,
        fORM.ServerName, fClient.Root]);
  end;
end;

function TDDDRestClientSettings.OnAuthentificationFailed(Retry: integer;
  var aUserName, aPassword: string; out aPasswordHashed: boolean): boolean;
begin
  if (Retry = 1) and (fORM.User <> '') then begin
    aUserName := UTF8ToString(fORM.User);
    aPassword := UTF8ToString(fORM.PasswordPlain);
    aPasswordHashed := true;
    result := true;
  end
  else
    result := false;
end;

procedure TDDDRestClientSettings.SetDefaults(const Root, Port, WebSocketPassword,
  UserPassword, User, Server: RawUTF8; ForceSetCredentials: boolean;
  ConnectRetrySeconds, WebSocketsLoopDelayMS: integer);
begin
  if fClient.Root = '' then
    fClient.Root := Root;
  if fORM.Kind = '' then
    if WebSocketPassword <> '' then
      fORM.Kind := 'TSQLHttpClientWebsockets'
    else
      fORM.Kind := TSQLHttpClient.ClassName;
  if ForceSetCredentials or (fORM.ServerName = '') then begin
    if fORM.ServerName = '' then
      if Port = '' then
        fORM.ServerName := Server else
        if Server = '' then
          fORM.ServerName := 'http://localhost:' + Port else
          if PosExChar(':', Server) = 0 then
            fORM.ServerName := 'http://' + Server + ':' + Port else
            fORM.ServerName := Server;
    if fClient.WebSocketsPassword = '' then
      fClient.WebSocketsPassword := WebSocketPassword;
    fWebSocketsLoopDelay := WebSocketsLoopDelayMS;
    fClient.ConnectRetrySeconds := ConnectRetrySeconds;
    if UserPassword <> '' then begin
      fORM.User := User;
      fORM.PasswordPlain := UserPassword;
    end;
  end;
end;

function AdministratedDaemonClient(Definition: TDDDRestClientSettings; Model: TSQLModel): TSQLHttpClientWebsockets;
begin
  result := Definition.NewRestClientInstance(nil, Model) as TSQLHttpClientWebsockets;
  try
    result.ServiceDefine(IAdministratedDaemon, sicShared);
  except
    result.Free;
    raise;
  end;
end;


{ TDDDAdministratedDaemonAsProxy }

{ FIXME: Proxy feature not finished yet (but not needed any more) }

type
  TDDDAdministratedDaemonAsProxy = class(TCQRSService, IAdministratedDaemonAsProxy)
  protected
    fProxy: IAdministratedDaemon;
    fProxyClient: TSQLHttpClientWebsockets;
    /// IAdministratedDaemon methods
    function RetrieveState(out Status: variant): TCQRSResult;
    function Start: TCQRSResult;
    function Stop(out Information: variant): TCQRSResult;
    function Halt(out Information: variant): TCQRSResult;
    function DatabaseList: TRawUTF8DynArray;
    function DatabaseTables(const DatabaseName: RawUTF8): TRawUTF8DynArray;
    function DatabaseExecute(const DatabaseName, SQL: RawUTF8): TServiceCustomAnswer;
    procedure SubscribeLog(const Levels: TSynLogInfos; const Callback:
      ISynLogCallback; ReceiveExistingKB: cardinal);
    procedure CallbackReleased(const callback: IInvokable; const interfaceName: RawUTF8);
    function StartProxy(const aDDDRestClientSettings: variant): TCQRSResult;
  public
    destructor Destroy; override;
  end;

procedure TDDDAdministratedDaemonAsProxy.CallbackReleased(const callback:
  IInvokable; const interfaceName: RawUTF8);
begin
  fProxy.CallbackReleased(callback, interfaceName);
end;

function TDDDAdministratedDaemonAsProxy.DatabaseExecute(const DatabaseName, SQL:
  RawUTF8): TServiceCustomAnswer;
begin
  result := fProxy.DatabaseExecute(DatabaseName, SQL);
end;

function TDDDAdministratedDaemonAsProxy.DatabaseList: TRawUTF8DynArray;
begin
  result := fProxy.DatabaseList;
end;

function TDDDAdministratedDaemonAsProxy.DatabaseTables(const DatabaseName:
  RawUTF8): TRawUTF8DynArray;
begin
  result := fProxy.DatabaseTables(DatabaseName);
end;

destructor TDDDAdministratedDaemonAsProxy.Destroy;
begin
  fProxy := nil;
  FreeAndNil(fProxyClient);
  inherited Destroy;
end;

function TDDDAdministratedDaemonAsProxy.Halt(out Information: variant): TCQRSResult;
begin
  result := fProxy.Halt(Information);
end;

function TDDDAdministratedDaemonAsProxy.RetrieveState(out Status: variant): TCQRSResult;
begin
  result := fProxy.RetrieveState(Status);
end;

function TDDDAdministratedDaemonAsProxy.Start: TCQRSResult;
begin
  result := fProxy.Start;
end;

function TDDDAdministratedDaemonAsProxy.StartProxy(const aDDDRestClientSettings:
  variant): TCQRSResult;
var
  def: TDDDRestClientSettings;
  valid: boolean;
begin
  CqrsBeginMethod(qaNone, result);
  if (fProxy <> nil) or (fProxyClient <> nil) then begin
    CqrsSetResult(cqrsAlreadyExists,result);
    exit;
  end;
  def := TDDDRestClientSettings.Create;
  try
    JSONToObject(def, pointer(VariantSaveJSON(aDDDRestClientSettings)), valid);
    if not valid then begin
      CqrsSetResultMsg(cqrsBadRequest, '%.StartProxy(%): invalid def', [self,
        aDDDRestClientSettings],result);
      exit;
    end;
    try
      fProxyClient := AdministratedDaemonClient(def);
      if not fProxyClient.Services.Resolve(IAdministratedDaemon, fProxy) then
        raise EDDDRestClient.CreateUTF8('%.StartProxy(%): IAdministratedDaemon not supported',
          [self, aDDDRestClientSettings]);
      CqrsSetResult(cqrsSuccess,result);
    except
      on E: Exception do begin
        fProxy := nil;
        FreeAndNil(fProxyClient);
        CqrsSetResult(E,result);
      end;
    end;
  finally
    def.Free;
  end;
end;

function TDDDAdministratedDaemonAsProxy.Stop(out Information: variant): TCQRSResult;
begin
  result := fProxy.Stop(Information);
end;

procedure TDDDAdministratedDaemonAsProxy.SubscribeLog(const Levels: TSynLogInfos;
  const Callback: ISynLogCallback; ReceiveExistingKB: cardinal);
begin
  fProxy.SubscribeLog(Levels, Callback, ReceiveExistingKB);
end;

function AdministratedDaemonClientFromProxy(Definition: TDDDRestClientSettings;
  const ProxyIPPort: RawUTF8; Model: TSQLModel = nil): IAdministratedDaemon;
var
  proxy: TDDDRestClientSettings;
  client: TSQLHttpClientWebsockets;
  temp: IAdministratedDaemonAsProxy;
  res: TCQRSResult;
begin
  proxy := TDDDRestClientSettings.Create;
  try
    CopyObject(Definition, proxy); // share same connection credentials
    proxy.ORM.ServerName := ProxyIPPort;
    client := proxy.NewRestClientInstance(nil, Model) as TSQLHttpClientWebsockets;
  finally
    proxy.Free;
  end;
  try
    client.ServiceDefine(IAdministratedDaemonAsProxy, sicClientDriven);
    client.Services.Resolve(IAdministratedDaemonAsProxy, temp);
    res := temp.StartProxy(_ObjFast(Definition));
    if res <> cqrsSuccess then
      raise EDDDRestClient.CreateUTF8(
        'AdministratedDaemonClientFromProxy(%) was not able to proxy via %: %',
        [Definition.ORM.ServerName, ProxyIPPort, ToText(res)^]);
    result := temp;
  except
    client.Free;
    raise;
  end;
end;

function AdministratedDaemonServer(Settings: TDDDAdministratedDaemonSettings;
  DaemonClass: TDDDAdministratedDaemonClass): TDDDAdministratedDaemon;
begin
  if DaemonClass = nil then
    raise EDDDInfraException.Create('AdministratedDaemonServer(DaemonClass=nil)');
  if Settings = nil then
    raise EDDDInfraException.Create('AdministratedDaemonServer(Settings=nil)');
  with Settings.RemoteAdmin do
    result := DaemonClass.Create(AuthUserName, AuthHashedPassword, AuthRootURI, AuthNamedPipeName);
  result.InternalSettings := Settings;
  if Settings.Storage is TDDDAppSettingsStorageFile then
    result.InternalSettingsFolder := ExtractFilePath(
      TDDDAppSettingsStorageFile(Settings.Storage).SettingsJsonFileName);
  {$ifdef WITHLOG}
  result.Log.SynLog.Log(sllTrace, '%.Create(%)', [DaemonClass, Settings], result);
  {$endif}
  with Settings.RemoteAdmin do
    if AuthHttp.BindPort <> '' then
      result.AdministrationHTTPServer := TSQLHttpServer.Create(
        result.AdministrationServer, AuthHttp);
end;


{ ----- Applications Securization }

function ToText(auth: TECCAuthorize): PShortString;
begin
  result := GetEnumName(TypeInfo(TECCAuthorize), ord(auth));
end;

function ECCAuthorize(aContent: TObject; aSecretDays: integer; const aSecretPass,
  aDPAPI, aDecryptSalt, aAppLockPublic64: RawUTF8; const aSearchFolder: TFileName;
  aSecretInfo: PECCCertificateSigned; aLocalFile: PFileName): TECCAuthorize;
var
  fileroot, fileunlock, filesecret, filepublic: TFileName;
  priv, new: TECCCertificateSecret;
  auth: TECCCertificate;
  signature: TECCSignatureCertifiedContent;
  unlock, secret, temp, json: RawByteString;
  decrypt: TECCDecrypt;
  hash: THash256;
  valid: TECCValidity;
  issuer: TECCCertificateIssuer;
  privok, jsonok: boolean;
begin
  with ExeVersion do
    fileroot := SysUtils.LowerCase(FormatString('%@%', [User, Host]));
  if aSearchFolder = '' then
    fileroot := ExeVersion.ProgramFilePath + fileroot else
    fileroot := IncludeTrailingPathDelimiter(aSearchFolder) + fileroot;
  if aLocalFile <> nil then
    aLocalFile^ := fileroot;
  fileunlock := fileroot + '.unlock';
  filepublic := fileroot + ECCCERTIFICATEPUBLIC_FILEEXT;
  filesecret := fileroot + '.secret'; // DPAPI-encrypted .private file
  try
    unlock := StringFromFile(fileunlock);
    secret := StringFromFile(filesecret);
    temp := CryptDataForCurrentUser(secret, aDPAPI, false);
    priv := TECCCertificateSecret.Create;
    try
      result := eaInvalidSecret;      
      {$ifdef ENHANCEDRTL} {$ifdef VER150}
      if crc32cinlined($D26BE33F,@ECCAuthorize,14)<>$29743A4B then
        exit; // avoid stubbing (Delphi 7 ERTL only - just to show how it works)
      {$endif} {$endif}
      ECCIssuer(ExeVersion.User,Issuer);
      privok := priv.LoadFromSecureBinary(temp, aSecretPass, 100) and
        IsEqual(priv.Content.Signed.Issuer, Issuer);
      if aSecretInfo <> nil then
        aSecretInfo^ := priv.Content.Signed;
      if not privok or not ECCCheckDate(priv.Content) then begin
        new := TECCCertificateSecret.CreateNew(nil, ExeVersion.User, aSecretDays);
        try
          new.ToFile(filepublic);
          temp := new.SaveToSecureBinary(aSecretPass, 7, 100);
          secret := CryptDataForCurrentUser(temp, aDPAPI, true);
          FileFromString(secret, filesecret);
        finally
          new.Free;
        end;
        exit;
      end;
      result := eaMissingUnlockFile;
      if unlock = '' then
        exit;
      result := eaInvalidUnlockFile;
      decrypt := priv.Decrypt(unlock, json, @signature, nil, nil, aDecryptSalt, 10000);
      if decrypt <> ecdDecryptedWithSignature then
        exit;
      auth := TECCCertificate.CreateFromBase64(aAppLockPublic64);
      try
        hash := SHA256Digest(pointer(json), length(json));
        valid := ECCVerify(signature, hash, auth.Content);
        if not (valid in ECC_VALIDSIGN) then
          exit;
      finally
        auth.Free;
      end;
    finally
      priv.Free;
    end;
    RemoveCommentsFromJSON(pointer(json));
    JSONToObject(aContent, pointer(json), jsonok, nil, JSONTOOBJECT_TOLERANTOPTIONS);
    if jsonok then
      result := eaSuccess
    else
      result := eaInvalidJson;
  finally
    FillZero(hash);
    FillZero(json);
    FillZero(unlock);
    FillZero(temp);
    FillZero(secret);
  end;
end;


{ TDDDRestClientWebSockets }

constructor TDDDRestClientWebSockets.Create(aSettings: TDDDRestClientSettings;
  aOnConnect, aOnDisconnect: TOnRestClientNotify);
var
  u: TURI;
  t: integer;
  log: ISynLog;
begin
  DefineApplication; // should fill fApplicationName
  fOnFailed := ClientFailed;
  fOnSetUser := ClientSetUser;
  fOnConnect := aOnConnect;
  fOnDisconnect := aOnDisconnect;
  fOnWebSocketsClosed := WebSocketsClosed;
  if aSettings = nil then
    raise EDDDRestClient.CreateUTF8('%.Create(%) aSettings=nil', [fApplicationName, self]);
  if not u.From(aSettings.ORM.ServerName) then
    raise EDDDRestClient.CreateUTF8('%.Create(%): invalid ORM.ServerName=%',
      [self, fApplicationName, aSettings.ORM.ServerName]);
  log := SQLite3Log.Enter('Create(%): connect to %', [fApplicationName, aSettings.ORM.ServerName], self);
  t := aSettings.Timeout;
  fConnectRetrySeconds := aSettings.Client.ConnectRetrySeconds;
  inherited Create(u.Server, u.Port, CreateModel(aSettings), u.Https, '', '', t, t, t);
  Model.Owner := self; // just allocated by CreateModel()
  if aSettings.Client.WebSocketsPassword <> '' then begin
    fWebSocketLoopDelay := aSettings.WebSocketsLoopDelay;
    WebSocketsConnect(aSettings.Client.PasswordPlain);
  end;
  OnAuthentificationFailed := aSettings.OnAuthentificationFailed;
  if aSettings.ORM.Password = '' then
    RegisterServices // plain REST connection without authentication
  else
    if not SetUser(aSettings.ORM.User, aSettings.ORM.PasswordPlain, true) then
      raise EDDDRestClient.CreateUTF8('%.Create(%): invalid User=%',
        [self, fApplicationName, aSettings.ORM.User]);
end;

destructor TDDDRestClientWebSockets.Destroy;
begin
  with fLogClass.Enter(self, 'Destroy') do
  try
    ClientDisconnect;
  finally
    inherited Destroy;
    fOwnedSettings.Free;
  end;
end;

procedure TDDDRestClientWebSockets.AfterConnection;
begin // do nothing by default
end;

procedure TDDDRestClientWebSockets.AfterDisconnection;
begin // do nothing by default
end;

procedure TDDDRestClientWebSockets.ClientDisconnect;
var
  log: ISynLog;
begin
  if not fConnected then
    exit; // notify once
  log := fLogClass.Enter('ClientDisconnect(%)', [fApplicationName], self);
  fConnected := false;
  if log<>nil then
    log.Log(sllTrace, 'ClientDisconnect -> AfterDisconnection', self);
  try
    AfterDisconnection;
  except
    if log<>nil then
      log.Log(sllWarning, 'Ignored AfterDisconnection exception', self);
  end;
  if Assigned(fOnDisconnect) then
    try
      if log<>nil then
        log.Log(sllTrace, 'ClientDisconnect -> OnDisconnect = %',
          [ToText(TMethod(fOnDisconnect))], self);
      fOnDisconnect(self);
    except
      if log<>nil then
        log.Log(sllWarning, 'Ignored OnDisconnect exception', self);
    end;
end;

procedure TDDDRestClientWebSockets.ClientFailed(Sender: TSQLRestClientURI;
  E: Exception; Call: PSQLRestURIParams);
begin
  if Assigned(Call) and (Call^.OutStatus = HTTP_NOTIMPLEMENTED) then
    ClientDisconnect;
end;

procedure TDDDRestClientWebSockets.ClientSetUser(Sender: TSQLRestClientURI);
var
  log, log2: ISynLog;
begin
  if Assigned(Sender) and Assigned(Sender.SessionUser) then begin
    log := fLogClass.Enter('ClientSetUser(%) %',
      [fApplicationName, Sender.SessionUser], self);
    ClientDisconnect;
    fConnected := true;
    fSessionVersion := '';
    if not fServicesRegistered then begin
      log2 := fLogClass.Enter('RegisterServices', [], self);
      RegisterServices;
      log2 := nil;
      fServicesRegistered := true;
    end;
    if log<>nil then
      log.Log(sllTrace, 'ClientSetUser -> AfterConnection', self);
    try
      AfterConnection;
    except
      if log<>nil then
        log.Log(sllWarning, 'Ignored AfterConnection exception', self);
    end;
    if Assigned(fOnConnect) then
    try
      if log<>nil then
        log.Log(sllTrace, 'ClientSetUser -> OnConnect = %',
          [ToText(TMethod(fOnConnect))], self);
      fOnConnect(self);
    except
      if log<>nil then
        log.Log(sllWarning, 'Ignored OnConnect exception', self);
    end;
  end;
end;

function TDDDRestClientWebSockets.CreateModel(aSettings: TDDDRestClientSettings): TSQLModel;
begin // create a void data model by default
  result := TSQLModel.Create([], aSettings.Client.Root);
end;

procedure TDDDRestClientWebSockets.WebSocketsClosed(Sender: TObject);
begin
  ClientDisconnect;
end;


initialization
  TJSONSerializer.RegisterObjArrayForJSON(
    [TypeInfo(TECCCertificateObjArray),TECCCertificate]);
  {$ifdef DEBUG}
  {$ifdef EnableMemoryLeakReporting}
  {$ifdef HASFASTMM4} // FastMM4 integrated in Delphi 2006 (and up)
  ReportMemoryLeaksOnShutdown := True;
  {$endif}
  {$endif}
  {$endif}
end.

