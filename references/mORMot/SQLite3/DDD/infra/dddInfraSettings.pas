/// shared DDD Infrastructure: Application/Daemon settings classes
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit dddInfraSettings;

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
   - store settings in database, or in a centralized service?

}

{$I Synopse.inc} // define HASINLINE DDDNOSYNDB DDDNOMONGODB WITHLOG

{.$define DDDNOSYNDB}
// if defined, SynDB / external SQL DB won't be linked to the executable
{.$define DDDNOMONGODB}
// if defined, the Mongo DB client won't be linked to the executable

interface

uses
  {$ifdef MSWINDOWS}
  Windows, // for DeleteFile() inlining under Delphi 2006/2007
  {$endif}
  SysUtils,
  Classes,
  SynCommons,
  SynTable,
  SynLog,
  SynCrypto,
  mORMot,
  mORMotDDD,
  SynCrtSock,
  SynSQLite3,
  mORMotSQLite3,   // for internal SQlite3 database
  {$ifndef DDDNOSYNDB}
  SynDB,
  mORMotDB,        // for TDDDRestSettings on external SQL database
  {$endif}
  {$ifndef DDDNOMONGODB}
  SynMongoDB,
  mORMotMongoDB,   // for TDDDRestSettings on external NoSQL database
  {$endif}
  mORMotWrappers;  // for TDDDRestSettings to publish wrapper methods


{ ----- Manage Service/Daemon settings }

type
  /// settings used to define how logging take place
  // - will map the most used TSynLogFamily parameters
  TDDDLogSettings = class(TSynPersistent)
  protected
    fLevels: TSynLogInfos;
    fConsoleLevels: TSynLogInfos;
    fAutoFlush: integer;
    fStackTraceViaAPI: boolean;
    fLowLevelWebSocketsFrames: boolean;
    fDestinationPath: TFileName;
    fCustomFileName: TFileName;
    fRotateFileCount: cardinal;
    fRotateFileSize: cardinal;
    fRotateFileAtHour: integer;
    fSyslogLevels: TSynLogInfos;
    fSyslogFacility: TSyslogFacility;
    fSyslogServer: RawUTF8;
  public
    /// initialize the settings to their (TSynLogFamily) default values
    constructor Create; override;
  published
    /// the log levels to be used for the log file
    // - i.e. a combination of none or several logging event
    // - if "*" is serialized, unneeded sllNone won't be part of the set
    property Levels: TSynLogInfos read fLevels write fLevels;
    /// the optional log levels to be used for the console
    // - by default, only errors would be logged to the console
    // - you can specify here another set of levels, e.g. '*' for a verbose
    // console output - note that console is very slow to write, so usually
    // you should better not set a verbose definition here, unless you are
    // in debugging mode
    property ConsoleLevels: TSynLogInfos read fConsoleLevels write fConsoleLevels;
    /// if low-level WebSockets frames should be logged
    // - disabled by default, to minimize logged content
    // - may be enabled to monitor most (asynchronous) activity, especially
    // in background threads
    property LowLevelWebSocketsFrames: boolean read fLowLevelWebSocketsFrames write fLowLevelWebSocketsFrames;
    /// the time (in seconds) after which the log content must be written on
    // disk, whatever the current content size is
    // - by default, the log file will be written for every 4 KB of log (see
    // TSynLogFamily.BufferSize property) - this will ensure that the main
    // application won't be slow down by logging
    // - in order not to loose any log, a background thread can be created
    // and will be responsible of flushing all pending log content every
    // period of time (e.g. every 10 seconds)
    property AutoFlushTimeOut: integer read fAutoFlush write fAutoFlush;
    /// by default (false), logging will use manual stack trace browsing
    // - if you experiment unexpected EAccessViolation, try to set this setting
    // to TRUE so that the RtlCaptureStackBackTrace() API would be used instead
    property StackTraceViaAPI: boolean read FStackTraceViaAPI write FStackTraceViaAPI;
    /// allows to customize where the log files will be stored
    property DestinationPath: TFileName read FDestinationPath write FDestinationPath;
    /// allows to customize the log file name
    property CustomFileName: TFileName read fCustomFileName write fCustomFileName;
    /// auto-rotation of logging files
    // - set to 0 by default, meaning no rotation
    property RotateFileCount: cardinal read fRotateFileCount write fRotateFileCount;
    /// maximum size of auto-rotated logging files, in kilo-bytes (per 1024 bytes)
    property RotateFileSizeKB: cardinal read fRotateFileSize write fRotateFileSize;
    /// fixed hour of the day where logging files rotation should be performed
    property RotateFileDailyAtHour: integer read fRotateFileAtHour write fRotateFileAtHour;
    /// the optional log levels to be used for remote UDP syslog server sending
    // - works in conjunction with SyslogServer property
    // - default will transmit all warnings, errors and exceptions
    property SyslogLevels: TSynLogInfos read fSyslogLevels write fSyslogLevels;
    /// the optional log levels to be used for remote UDP syslog server sending
    // - works in conjunction with SyslogServer/SyslogLevels properties
    // - default is sfLocal0
    property SyslogFacility: TSyslogFacility read fSyslogFacility write fSyslogFacility;
    /// the optional remote UDP syslog server
    // - expecting https://tools.ietf.org/html/rfc5424 messages over UDP
    // - e.g. '1.2.3.4' to connect to UDP server 1.2.3.4 using default port 514 -
    // but you can specify an alternative port as '1.2.3.4:2514'
    // - works in conjunction with SyslogLevels/SyslogFacility properties
    // - default is '' to disable syslog remote logging
    property SyslogServer: RawUTF8 read fSyslogServer write fSyslogServer;
  end;

  TDDDAppSettingsAbstract = class;

  /// abstract parent class for storing application settings
  TDDDAppSettingsStorageAbstract = class(TSynAutoCreateFields)
  protected
    fInitialJsonContent: RawUTF8;
    fOwner: TDDDAppSettingsAbstract;
    /// called by TDDDAppSettingsAbstract.Create
    function SetOwner(aOwner: TDDDAppSettingsAbstract): boolean;
    /// inherited classes would override this to persist fInitialJsonContent
    procedure InternalStore; virtual; abstract;
  public
    /// initialize the storage instance
    constructor Create(const aInitialJSON: RawUTF8); reintroduce; virtual;
    /// TDDDAppSettingsAbstract would use this to actually persist the data
    procedure Store(const aJSON: RawUTF8); virtual;
    /// the JSON content, as specified when creating the instance
    // - will allow SettingsDidChange to check if has changed
    // - here the JSON content is stored with default ObjectToJSON() options,
    // so will be the normalized representation of the content, which may not
    // match the JSON supplied to SetInitialJsonContent() protected method
    property InitialJsonContent: RawUTF8 read fInitialJsonContent;
    /// the associated settings values
    property Owner: TDDDAppSettingsAbstract read fOwner;
  end;

  /// abstract class for storing application settings
  // - this class implements IAutoCreateFieldsResolve so is able to inject
  // its own values to any TInjectableAutoCreateFields instance
  // - you have to manage instance lifetime of these inherited classes with a
  // local IAutoCreateFieldsResolve variable, just like any TInterfaceObject
  TDDDAppSettingsAbstract = class(TInterfacedObjectAutoCreateFields,
    IAutoCreateFieldsResolve, IDDDSettingsStorable)
  protected
    fAllProps: PPropInfoDynArray;
    fDescription: string;
    fLog: TDDDLogSettings;
    fSyslog: TCrtSocket;
    fSyslogProcID: RawUTF8;
    fStorage: TDDDAppSettingsStorageAbstract;
    procedure SetProperties(Instance: TObject); virtual;
    function SyslogEvent(Sender: TTextWriter; Level: TSynLogInfo;
      const Text: RawUTF8): boolean;
  public
    /// initialize the settings, with a corresponding storage process
    constructor Create(aStorage: TDDDAppSettingsStorageAbstract); reintroduce;
    /// persist if needed, and finalize the settings
    destructor Destroy; override;
    /// to be called when the application starts, to initialize settings
    // - you can specify a default Description value
    // - it will set the global SQLite3Log.Family according to Log values
    procedure Initialize(const aDescription: string); virtual;
    /// persist the settings if needed
    // - just a wrapper around Storage.Store(AsJson)
    // - implements IDDDSettingsStorable for "#settings save" admin command
    procedure StoreIfUpdated; virtual;
    /// serialize the settings as JSON
    // - any enumerated or set published property will be commented with their
    // textual values, and 'stored false' properties would be included
    // - returns the new JSON content corresponding to the updated settings
    function AsJson: RawUTF8; virtual;
    /// low-level method returning all TSynPersistentPassword full paths
    // of all previously created TDDDAppSettingsStorageFile .settings
    // - as settingsfile=class1@full.path.to.pass1,class2@full.path.to.pass2,...
    // - you may use this method to create a 'passwords' resource for
    // /HardenPasswords command line switch as implemented in dddInfraSettings.pas:
    // ! passwords := SynLZCompress(TDDDAppSettingsAbstract.PasswordFields);
    // ! FileFromString(passwords, 'passwords.data');
    // then create e.g. a passwords.rc file as such:
    // $ passwords 10 "passwords.data"
    // compile this resource:
    // $ brcc32 passwords.rc
    // and link the resulting .res file to your daemon executable:
    // ! {$R passwords.res}
    // then /HardenPasswords and /PlainPasswords command line switchs will
    // cypher/uncypher all TSynPersistentPassword protected fields using safe
    // per-user CryptDataForCurrentUser() encryption
    class function PasswordFields: RawUTF8;
    /// access to the associated settings storage
    property Storage: TDDDAppSettingsStorageAbstract read fStorage;
    /// transmitted as PROCID as part of any Log.SyslogServer message
    property SyslogProcID: RawUTF8 read fSyslogProcID write fSyslogProcID;
  published
    /// some text which will be used to describe this application
    property Description: string read FDescription write FDescription;
    /// defines how logging will be done for this application
    property Log: TDDDLogSettings read fLog;
  end;

  /// class type used for storing application settings
  TDDDAppSettingsAbstractClass = class of TDDDAppSettingsAbstract;

  /// class used for storing application settings as a JSON file
  TDDDAppSettingsStorageFile = class(TDDDAppSettingsStorageAbstract)
  protected
    fSettingsJsonFileName: TFileName;
    procedure InternalStore; override;
  public
    /// initialize and read the settings from the supplied JSON file name
    // - if no file name is specified, will use the executable name with
    // '.settings' as extension
    constructor Create(const aSettingsJsonFileName: TFileName=''); reintroduce; virtual;
    /// compute a file name relative to the .settings file path
    function FileNameRelativeToSettingsFile(const aFileName: TFileName): TFileName;
    /// the .settings file name, including full path
    property SettingsJsonFileName: TFileName
      read fSettingsJsonFileName write fSettingsJsonFileName;
  end;

  /// some options to be used for TDDDRestSettings
  // - as part of the .settings, they may be tuned for specific installations,
  // whereas TDDDNewRestInstanceOptions are defined in code
  TDDDRestSettingsOption =
    (optEraseDBFileAtStartup,
     optSQlite3FileSafeSlowMode,
     optSQlite3FileSafeNonExclusive,
     optNoSystemUse,
     optSQlite3File4MBCacheSize,
     optForceAjaxJson,
     optSQLite3LogQueryPlan);

  /// define options to be used for TDDDRestSettings
  TDDDRestSettingsOptions = set of TDDDRestSettingsOption;

  /// how TDDDRestSettings.NewRestInstance would create its instances
  // - riOwnModel will set ModelInstance.Owner := RestInstance
  // - riHandleAuthentication will set the corresponding parameter to true
  // - riDefaultLocalSQlite3IfNone/riDefaultInMemorySQLite3IfNone will create
  // a SQLite3 engine with a local file/memory storage, if
  // TDDDRestSettings.ORM.Kind is not set
  // - riDefaultFullMemoryIfNone will create a TSQLRestServerFullMemory non
  // persistent storage, or riDefaultLocalBinaryFullMemoryIfNone with a
  // binary local file, if TDDDRestSettings.ORM.Kind is not set
  // - riCreateMissingTables will call RestInstance.CreateMissingTables
  // - riRaiseExceptionIfNoRest will raise an EDDDInfraException if
  // TDDDRestSettings.NewRestInstance would return nil
  // - riWithInternalState will enable 'Server-InternalState:' header transmission
  // i.e. disable rsoNoInternalState for TSQLRestServer.Options
  TDDDNewRestInstanceOptions = set of (
    riOwnModel, riCreateVoidModelIfNone,
    riHandleAuthentication,
    riDefaultLocalSQlite3IfNone, riDefaultInMemorySQLite3IfNone,
    riDefaultFullMemoryIfNone, riDefaultLocalBinaryFullMemoryIfNone,
    riCreateMissingTables,
    riRaiseExceptionIfNoRest,
    riWithInternalState);

  /// storage class for initializing an ORM REST class
  // - this class will contain some generic properties to initialize a TSQLRest
  // pointing to a local or remote SQL/NoSQL database, with optional wrappers
  TDDDRestSettings = class(TSynAutoCreateFields)
  protected
    fORM: TSynConnectionDefinition;
    fDefaultDataFolder: TFileName;
    fDefaultDataFileName: RawUTF8;
    fRoot: RawUTF8;
    fWrapperTemplateFolder: TFileName;
    fWrapperSourceFolders: TFileName;
    fOptions: TDDDRestSettingsOptions;
    fWrapperTemplateFolderFixed: TFileName;
    fWrapperSourceFolderFixed: TFileName;
  public
    /// is able to instantiate a REST instance according to the stored definition
    // - Definition.Kind will identify the TSQLRestServer or TSQLRestClient class
    // to be instantiated, or if equals 'MongoDB'/'MongoDBS' use a full MongoDB
    // engine, or an external SQL database if it matches a TSQLDBConnectionProperties
    // classname
    // - if aDefaultLocalSQlite3 is TRUE, then if Definition.Kind is '',
    // a local SQlite3 file database will be initiated
    // - if aMongoDBIdentifier is not 0, then it will be supplied to every
    // TSQLRestStorageMongoDB.SetEngineAddComputeIdentifier() created
    // - will return nil if the supplied Definition is not correct
    // - note that the supplied Model.Root is expected to be the default root
    // URI, which will be overriden with this TDDDRestSettings.Root property
    // - will also publish /wrapper HTML page if WrapperTemplateFolder is set
    function NewRestInstance(aRootSettings: TDDDAppSettingsAbstract;
      aModel: TSQLModel; aOptions: TDDDNewRestInstanceOptions
      {$ifndef DDDNOSYNDB}; aExternalDBOptions:
      TVirtualTableExternalRegisterOptions=[regDoNotRegisterUserGroupTables]{$endif}
      {$ifndef DDDNOMONGODB}; aMongoDBIdentifier: word=0; aMongoDBOptions:
      TStaticMongoDBRegisterOptions=[mrDoNotRegisterUserGroupTables]{$endif}): TSQLRest; overload; virtual;
    /// is able to instantiate a REST instance according to the stored definition
    // - just an overloaded version which will create an owned TSQLModel with
    // the supplied TSQLRecord classes
    function NewRestInstance(aRootSettings: TDDDAppSettingsAbstract;
      const aTables: array of TSQLRecordClass; aOptions: TDDDNewRestInstanceOptions
      {$ifndef DDDNOSYNDB}; aExternalDBOptions:
      TVirtualTableExternalRegisterOptions=[regDoNotRegisterUserGroupTables]{$endif}
      {$ifndef DDDNOMONGODB}; aMongoDBIdentifier: word=0; aMongoDBOptions:
      TStaticMongoDBRegisterOptions=[mrDoNotRegisterUserGroupTables]{$endif}): TSQLRest; overload; virtual;
    /// initialize a stand-alone TSQLRestServerDB instance
    // - with its own database file located in DefaultDataFileName + aDBFileName
    // - will own its own TSQLModel with aModelRoot/aModelTables
    // - you can tune aCacheSize if the default 40MB value is not right
    // - will eventually call CreateMissingTables
    // - define custom TDDDRestSettingsOptions if needed
    function NewRestServerDB(const aDBFileName: TFileName; const aModelRoot: RawUTF8;
      const aModelTables: array of TSQLRecordClass; aOptions: TDDDRestSettingsOptions=[];
      aCacheSize: cardinal=10000): TSQLRestServerDB;
    /// if DB is a TSQLRestServerDB, will define the expection options
    // - DB.FileName will be erased from disk if optEraseDBFileAtStartup is defined
    // - force LockingMode=exclusive and Synchrounous=off unless
    // optSQlite3FileSafeNonExclusive/optSQlite3FileSafeSlowMode options are set
    class procedure RestServerDBSetOptions(DB: TSQLRestServer; Options: TDDDRestSettingsOptions);
    /// returns the WrapperTemplateFolder property, all / chars replaced by \
    // - so that you would be able to store the paths with /, avoiding JSON escape
    function WrapperTemplateFolderFixed(ReturnLocalIfNoneSet: boolean=false): TFileName;
    /// returns the WrapperSourceFolder property, all / chars replaced by \
    // - so that you would be able to store the paths with /, avoiding JSON escape
    function WrapperSourceFolderFixed: TFileName;
    /// generate API documentation corresponding to REST SOA interfaces
    procedure WrapperGenerate(Rest: TSQLRestServer; Port: integer;
      const DestFile: TFileName; const Template: TFileName = 'API.adoc.mustache');
    /// the default folder where database files are to be stored
    // - will be used by NewRestInstance instead of the .exe folder, if set
    property DefaultDataFolder: TFileName read fDefaultDataFolder write fDefaultDataFolder;
    /// the default database file name 
    property DefaultDataFileName: RawUTF8 read fDefaultDataFileName write fDefaultDataFileName;
  published
    /// the URI Root to be used for the REST Model
    property Root: RawUTF8 read fRoot write fRoot;
    /// defines a mean of access to a TSQLRest instance
    // - using Kind/ServerName/DatabaseName/User properties: Kind would define
    // the TSQLRest class to be instantiated by function NewRestInstance()
    property ORM: TSynConnectionDefinition read fORM;
    /// if set to a valid folder, the generated TSQLRest will publish a
    // '/Root/wrapper' HTML page so that client code could be generated
    property WrapperTemplateFolder: TFileName
      read fWrapperTemplateFolder write fWrapperTemplateFolder;
    /// where the source code may be searched, for comment extraction of types
    // - several folders may be defined, separated by ; (just like in Delphi IDE)
    // - only used if WrapperTemplateFolder is defined
    property WrapperSourceFolders: TFileName
      read fWrapperSourceFolders write fWrapperSourceFolders;
    /// how the REST instance is to be initialized
    property Options: TDDDRestSettingsOptions read fOptions write fOptions;
  end;

  /// parent class for storing REST-based application settings 
  // - this class could be used for an application with a single REST server
  // running on a given HTTP port
  TDDDAppSettingsRest = class(TDDDAppSettingsAbstract)
  protected
    fRest: TDDDRestSettings;
    fServerPort: RawUTF8;
  public
    /// to be called when the application starts, to initialize settings
    // - will call inherited TDDDAppSettingsFile.Initialize, and
    // set ServerPort to a default 888/8888 value under Windows/Linux
    procedure Initialize(const aDescription: string); override;
  published
    /// allow to instantiate a REST instance from its JSON definition
    property Rest: TDDDRestSettings read fRest;
    /// the IP port to be used for the HTTP server associated with the application
    property ServerPort: RawUTF8 read fServerPort write fServerPort;
  end;

  /// define how an administrated service/daemon is remotely accessed via REST
  // - the IAdministratedDaemon service will be published to administrate
  // this service/daemon instance
  // - those values should match the ones used on administrative tool side
  TDDDAdministratedDaemonRemoteAdminSettings = class(TSynAutoCreateFields)
  protected
    FAuthRootURI: RawUTF8;
    FAuthHashedPassword: RawUTF8;
    FAuthUserName: RawUTF8;
    FAuthNamedPipeName: TFileName;
    FAuthHttp: TSQLHttpServerDefinition;
  public
  published
    /// the root URI used for the REST data model
    // - default URI is 'admin'
    property AuthRootURI: RawUTF8 read FAuthRootURI write FAuthRootURI;
    /// if set, expect authentication with this single user name
    // - that is, the TSQLRestServer will register a single TSQLAuthUser
    // instance with the supplied AuthUserName/AuthHashedPassword credentials
    property AuthUserName: RawUTF8 read FAuthUserName write FAuthUserName;
    /// the SHA-256 hashed password to authenticate AuthUserName
    // - follows the TSQLAuthUser.ComputeHashedPassword() encryption
    // - marked as 'stored false' so that it won't appear e.g. in the logs
    property AuthHashedPassword: RawUTF8 read FAuthHashedPassword write FAuthHashedPassword
      stored false;
    /// if defined, the following pipe name would be used for REST publishing
    // - by definition, will work only on Windows
    property AuthNamedPipeName: TFileName read FAuthNamedPipeName write FAuthNamedPipeName;
    /// if defined, these parameters would be used for REST publishing over HTTP
    property AuthHttp: TSQLHttpServerDefinition read FAuthHttp;
  end;

  /// parent class for storing a service/daemon settings
  // - under Windows, some Service* properties will handle installation as a
  // regular Windows Service, thanks to TDDDDaemon
  TDDDAdministratedDaemonSettings = class(TDDDAppSettingsAbstract)
  protected
    FRemoteAdmin: TDDDAdministratedDaemonRemoteAdminSettings;
    FServiceDisplayName: string;
    FServiceName: string;
    FServiceDependencies: TStringDynArray;
    FServiceAutoStart: boolean;
    FAppUserModelID: string;
  public
    /// to be called when the application starts, to initialize settings
    // - you can specify default Description and Service identifiers
    // - the service-related parameters are Windows specific, and will be
    // ignored on other platforms
    procedure Initialize(const aDescription,
        aServiceName,aServiceDisplayName,aAppUserModelID: string;
        const aServiceDependencies: TStringDynArray = nil); reintroduce; virtual;
    /// returns the folder containing .settings files - .exe folder by default
    function SettingsFolder: TFileName; virtual;
    /// under Windows, will define optional Service internal Dependencies
    // - not published by default: could be defined if needed, or e.g. set in
    // overriden constructor
    property ServiceDependencies: TStringDynArray read FServiceDependencies write FServiceDependencies;
  published
    /// define how this administrated service/daemon is accessed via REST
    property RemoteAdmin: TDDDAdministratedDaemonRemoteAdminSettings read FRemoteAdmin;
    /// under Windows, will define the Service internal name
    property ServiceName: string read FServiceName write FServiceName;
    /// under Windows, will define the Service displayed name
    property ServiceDisplayName: string read FServiceDisplayName write FServiceDisplayName;
    /// under Windows, will define if the Service should auto-start at boot
    // - FALSE means that it should be started on demand
    property ServiceAutoStart: boolean read FServiceAutoStart write FServiceAutoStart;
    /// under Windows 7 and later, will set an unique application-defined
    // Application User Model ID (AppUserModelID) that identifies the current
    // process to the taskbar
    // - this identifier allows an application to group its associated processes
    // and windows under a single taskbar button
    // - should follow SetAppUserModelID() expectations, i.e. 'Company.Product'
    property AppUserModelID: string read FAppUserModelID write FAppUserModelID;
  end;

  /// a Factory event allowing to customize/mock a socket connection
  // - the supplied aOwner should be a TDDDSocketThread instance
  // - returns a IDDDSocket interface instance (e.g. a TDDDSynCrtSocket)
  TOnIDDDSocketThreadCreate = procedure(aOwner: TObject; out Obj) of object;

  /// the settings of a TDDDThreadSocketProcess thread
  // - defines how to connect (and reconnect) to the associated TCP server
  TDDDSocketThreadSettings = class(TSynAutoCreateFields)
  protected
    fHost: RawUTF8;
    fPort: integer;
    fSocketLoopPeriod: integer;
    fSocketTimeout: integer;
    fSocketBufferBytes: integer;
    fSocketMaxBufferBytes: integer;
    fConnectionAttemptsInterval: Integer;
    fAutoReconnectAfterSocketError: boolean;
    fMonitoringInterval: integer;
    fOnIDDDSocketThreadCreate: TOnIDDDSocketThreadCreate;
  public
    /// used to set the default values
    constructor Create; override;
    /// set Host and Port values from a 'ip:port' or 'ip' text
    function SetHostPort(const IpPort: RawByteString; defaultPort: integer): boolean;
    /// retrieve Host and Port values as a single 'ip:port' text
    function GetHostPort: RawUTF8;
    /// you could set here a factory method to mock the socket connection
    // - this property is public, but not published, since it should not be
    // serialized on the settings file, but overloaded at runtime
    property OnIDDDSocketThreadCreate: TOnIDDDSocketThreadCreate
      read fOnIDDDSocketThreadCreate write fOnIDDDSocketThreadCreate;
  published
    /// the associated TCP server host
    property Host: RawUTF8 read FHost write FHost;
    /// the associated TCP server port
    property Port: integer read FPort write FPort;
    /// how many millisecond the main socket reading loop should wait
    // for pending data, before calling TDDDSocketThread.InternalExecuteIdle 
    // - default is 100 ms
    property SocketLoopPeriod: integer read fSocketLoopPeriod write fSocketLoopPeriod;
    /// the time out period, in milliseconds, for socket access
    // - default is 2000 ms, i.e. 2 seconds
    property SocketTimeout: integer read FSocketTimeout write FSocketTimeout;
    /// the internal size of the input socket buffer
    // - default is 32768, i.e. 32 KB
    property SocketBufferBytes: integer read FSocketBufferBytes write FSocketBufferBytes;
    /// the maximum size of the thread input buffer
    // - i.e. how many bytes are stored in fSocketInputBuffer memory, before
    // nothing is retrieved from the socket buffer
    // - set to avoid any "out of memory" of the currrent process, if the
    // incoming data is not processed fast enough
    // - default is 16777216, i.e. 16 MB
    property SocketMaxBufferBytes: integer read FSocketMaxBufferBytes write FSocketMaxBufferBytes;
    /// the time, in seconds, between any reconnection attempt
    // - default value is 5 - i.e. five seconds
    // - if you set -1 as value, thread would end without any retrial
    property ConnectionAttemptsInterval: Integer
      read fConnectionAttemptsInterval write fConnectionAttemptsInterval;
    /// if TRUE, any communication error would try to reconnect the socket
    property AutoReconnectAfterSocketError: boolean
      read FAutoReconnectAfterSocketError write FAutoReconnectAfterSocketError;
    /// the period, in milliseconds, on which Monitoring information is logged
    // - default value is 120000, i.e. 2 minutes
    property MonitoringLogInterval: integer read FMonitoringInterval write FMonitoringInterval;
  end;

  /// storage class for a ServicesLog settings
  TDDDServicesLogRestSettings = class(TDDDRestSettings)
  protected
    fShardDBCount: Integer;
  public
    /// compute a stand-alone REST instance for interface-based services logging
    // - all services of aMainRestWithServices would log their calling information
    // into a dedicated table, but the methods defined in aExcludedMethodNamesCSV
    // (which should be specified, even as '', to avoid FPC compilation error)
    // - by default, will create a local SQLite3 file for storage, optionally
    // via TSQLRestStorageShardDB if ShardDBCount is set
    // - the first supplied item of aLogClass array would be used for the
    // service logging; any additional item would be part of the model of the
    // returned REST instance, but may be used later on (e.g. to handle
    // DB-based asynchronous remote notifications as processed by
    // TServiceFactoryClient.SendNotificationsVia method)
    // - if aLogClass=[], plain TSQLRecordServiceLog would be used as default
    // - aShardRange is used for TSQLRestStorageShardDB if ShardDBCount>0
    function NewRestInstance(aRootSettings: TDDDAppSettingsAbstract;
      aMainRestWithServices: TSQLRestServer; const aLogClass: array of TSQLRecordServiceLogClass;
      const aExcludedMethodNamesCSV: RawUTF8; aShardRange: TID=50000): TSQLRest; reintroduce;
  published
    /// if set, will define MaxShardCount for TSQLRestStorageShardDB persistence
    property ShardDBCount: Integer read fShardDBCount write fShardDBCount;
  end;

  /// parent class for storing a HTTP published service/daemon settings
  TDDDAdministratedDaemonHttpSettings = class(TDDDAdministratedDaemonSettings)
  protected
    fRest: TDDDRestSettings;
    fHttp: TSQLHttpServerDefinition;
    fServicesLog: TDDDServicesLogRestSettings;
  published
    /// how the main REST server is implemented
    // - most probably using a TSQLRestServerDB, i.e. local SQLite3 storage
    property Rest: TDDDRestSettings read fRest;
    /// how the HTTP server should be defined
    property Http: TSQLHttpServerDefinition read fHttp;
    /// how the SOA calls would be logged into their own SQlite3 database
    property ServicesLog: TDDDServicesLogRestSettings read fServicesLog;
  end;

  /// stand-alone property to publish a secondary TSQLRestServer over HTTP
  TDDDRestHttpSettings = class(TSynAutoCreateFields)
  protected
    fRest: TDDDRestSettings;
    fHttp: TSQLHttpServerDefinition;
  published
    /// how the REST server is implemented
    // - most probably using a TSQLRestServerDB, i.e. local SQLite3 storage
    property Rest: TDDDRestSettings read fRest;
    /// how the HTTP server should be defined
    property Http: TSQLHttpServerDefinition read fHttp;
  end;

  /// stand-alone property to publish a secondary logged service over HTTP
  TDDDRestHttpLogSettings = class(TDDDRestHttpSettings)
  protected
    fServicesLog: TDDDServicesLogRestSettings;
  published
    /// how the SOA calls would be logged into their own SQlite3 database
    property ServicesLog: TDDDServicesLogRestSettings read fServicesLog;
  end;
  
  /// storage class for a remote MongoDB server direct access settings
  TDDDMongoDBRestSettings = class(TDDDRestSettings)
  public
    /// set the default values for direct MongoDB server connection
    // - if MongoServerAddress is e.g. '?:27017', entry with default value would
    // be saved in the settings, but NewRestInstance() would ignore it: once the
    // remote MongoDB server IP is known, you may just replace '?' to use it
    // - if MongoUser and MongoPassword are set, would call TMongoClient.OpenAuth()
    procedure SetDefaults(const Root, MongoServerAddress, MongoDatabase,
      MongoUser, MongoPassword: RawUTF8; TLS: boolean=false);
  end;

  TDDDEmailerSettings = class(TSynPersistent)
  protected
    fSMTP: RawUTF8;
    fRecipients: RawUTF8;
  public
    constructor Create; override;
  published
    property SMTP: RawUTF8 read fSMTP write fSMTP;
    property Recipients: RawUTF8 read fRecipients write fRecipients;
  end;


implementation

{ TDDDAppSettingsAbstract }

procedure TDDDAppSettingsAbstract.Initialize(const aDescription: string);
var
  uri: TURI;
begin
  {$ifdef WITHLOG}
  with SQLite3Log.Family do begin
    Level := Log.Levels-[sllNone]; // '*' would include sllNone
    if Log.ConsoleLevels<>[] then
      EchoToConsole := Log.ConsoleLevels-[sllNone];
    PerThreadLog := ptIdentifiedInOnFile;
    if Log.DestinationPath<>'' then
     DestinationPath := Log.DestinationPath;
    if Log.CustomFileName<>'' then
      CustomFileName := Log.CustomFileName;
    RotateFileCount := Log.RotateFileCount;
    RotateFileSizeKB := Log.RotateFileSizeKB;
    RotateFileDailyAtHour := Log.RotateFileDailyAtHour;
    if Log.RotateFileCount<=0 then
      HighResolutionTimestamp := true;
    FileExistsAction := acAppend; // default rotation mode
    if Log.StackTraceViaAPI then
      StackTraceUse := stOnlyAPI;
    // AutoFlushTimeOut not set now, since won't work with /form
    if (Log.SyslogServer<>'') and (Log.SyslogServer[1]<>'?') and
       not Assigned(EchoCustom) and (fSyslog=nil) and (Log.SyslogLevels<>[]) and
       uri.From(Log.SyslogServer,'514') then
      try
        fSyslog := TCrtSocket.Open(uri.Server,uri.Port,cslUDP,2000);
        EchoCustom := SyslogEvent;
      except
        fSyslog := nil;
      end;
  end;
  {$endif}
  if fDescription='' then
    fDescription := aDescription;
end;

function TDDDAppSettingsAbstract.SyslogEvent(Sender: TTextWriter; Level: TSynLogInfo;
  const Text: RawUTF8): boolean;
var
  buf: array[0..511] of AnsiChar; // 512 bytes for fast unfragmented UDP packet
  len: PtrInt;
begin
  result := false;
  if (fSyslog=nil) or not (Level in Log.SyslogLevels) then
    exit;
  len := SyslogMessage(Log.SyslogFacility,LOG_TO_SYSLOG[Level],Text,
    fSyslogProcID,ToText(Level),@buf,sizeof(buf),true);
  if len<>0 then
    if fSyslog.TrySndLow(@buf,len) then // works even if no server is available
      result := true else
      raise ESynException.CreateUTF8('%.SyslogEvent failed for %:% as error %',
        [self,fSyslog.Server,fSyslog.Port,fSyslog.LastLowSocketError]);
end;

procedure TDDDAppSettingsAbstract.SetProperties(Instance: TObject);
begin
  CopyObject(self,Instance);
end;

destructor TDDDAppSettingsAbstract.Destroy;
begin
  StoreIfUpdated;
  inherited Destroy;
  fStorage.Free;
  if fSyslog<>nil then begin
    {$ifdef WITHLOG}
    SQLite3Log.Family.EchoCustom := nil;
    {$endif}
    FreeAndNil(fSyslog);
  end;
end;

function TDDDAppSettingsAbstract.AsJson: RawUTF8;
begin
  result := ObjectToJSON(Self,[woHumanReadable,woStoreStoredFalse,
    woHumanReadableFullSetsAsStar,woHumanReadableEnumSetAsComment]);
end;

procedure TDDDAppSettingsAbstract.StoreIfUpdated;
begin
  if fStorage<>nil then
    fStorage.Store(AsJson);
end;

var
  TDDDAppSettingsAbstractFiles: array of record
    FileName: RawUTF8;
    SettingClass: TDDDAppSettingsAbstractClass;
  end;

constructor TDDDAppSettingsAbstract.Create(aStorage: TDDDAppSettingsStorageAbstract);
begin
  inherited Create;
  if aStorage=nil then
    aStorage := TDDDAppSettingsStorageFile.Create;
  fStorage := aStorage;
  fStorage.SetOwner(self);
  if aStorage.InheritsFrom(TDDDAppSettingsStorageFile) then begin
    SetLength(TDDDAppSettingsAbstractFiles,length(TDDDAppSettingsAbstractFiles)+1);
    with TDDDAppSettingsAbstractFiles[high(TDDDAppSettingsAbstractFiles)] do begin
      FileName := Split(StringToUTF8(ExtractFileName(TDDDAppSettingsStorageFile(aStorage).
        fSettingsJsonFileName)),'.');
      SettingClass := pointer(ClassType);
    end;
  end;
end;

class function TDDDAppSettingsAbstract.PasswordFields: RawUTF8;
  procedure InternalAdd(const path: RawUTF8; C: TClass; var res: TRawUTF8DynArray);
  var PI,PP: PPropInfo;
      CT: TClass;
      p: RawUTF8;
      i: integer;
      offset: pointer;
  begin
    offset := TSynPersistentWithPassword(nil).GetPasswordFieldAddress;
    while C<>nil do begin
      for i := 1 to InternalClassPropInfo(C,PI) do begin
        if PI^.PropType^.Kind=tkClass then begin
          FormatUTF8('%%.',[path,PI^.Name],p);
          CT := PI^.PropType^.ClassType^.ClassType;
          if CT.InheritsFrom(TSynPersistentWithPassword) then begin
            PP := ClassFieldPropWithParentsFromClassOffset(CT,offset);
            if PP<>nil then
              AddRawUTF8(res,FormatUTF8('%@%%', [ClassNameShort(CT)^, p, PP^.Name]));
          end;
          InternalAdd(p,CT,res); // recursive search of all password fields
        end;
        PI := PI^.Next;
      end;
      C := GetClassParent(C);
    end;
  end;
var i: integer;
    res: TRawUTF8DynArray;
begin
  result := '';
  for i := 0 to high(TDDDAppSettingsAbstractFiles) do
    with TDDDAppSettingsAbstractFiles[i] do begin
      res := nil;
      InternalAdd('',SettingClass,res);
      if res<>nil then
        result := FormatUTF8('%%=%'#13#10,[result,FileName,RawUTF8ArrayToCSV(res)]);
    end;
end;


{ TDDDLogSettings }

constructor TDDDLogSettings.Create;
begin
  inherited Create;
  fLevels := [low(TSynLogInfo)..high(TSynLogInfo)]; // "Levels":"*" by default
  fRotateFileAtHour := -1;
  fRotateFileCount := 20;
  fRotateFileSize := 128*1024; // 128 MB per rotation log by default
  fAutoFlush := 5;
  fSyslogLevels := [sllWarning,sllLastError,sllError,
    sllException,sllExceptionOS,sllNewRun,sllDDDError];
  fSyslogFacility := sfLocal0;
end;


{ TDDDRestSettings }

function TDDDRestSettings.NewRestInstance(aRootSettings: TDDDAppSettingsAbstract;
  const aTables: array of TSQLRecordClass; aOptions: TDDDNewRestInstanceOptions
  {$ifndef DDDNOSYNDB}; aExternalDBOptions: TVirtualTableExternalRegisterOptions {$endif}
  {$ifndef DDDNOMONGODB}; aMongoDBIdentifier: word;
  aMongoDBOptions: TStaticMongoDBRegisterOptions{$endif}): TSQLRest;
begin
  include(aOptions,riOwnModel);
  result := NewRestInstance(aRootSettings,TSQLModel.Create(aTables,fRoot),aOptions
    {$ifndef DDDNOSYNDB},aExternalDBOptions{$endif}
    {$ifndef DDDNOMONGODB},aMongoDBIdentifier,aMongoDBOptions{$endif});
end;

function TDDDRestSettings.NewRestInstance(aRootSettings: TDDDAppSettingsAbstract;
  aModel: TSQLModel; aOptions: TDDDNewRestInstanceOptions
  {$ifndef DDDNOSYNDB}; aExternalDBOptions: TVirtualTableExternalRegisterOptions{$endif}
  {$ifndef DDDNOMONGODB}; aMongoDBIdentifier: word;
  aMongoDBOptions: TStaticMongoDBRegisterOptions{$endif}): TSQLRest;

  procedure ComputeDefaultORMServerName(const Ext: RawUTF8);
  var FN: RawUTF8;
  begin
    if fORM.ServerName='' then begin
      if fDefaultDataFolder='' then
        fDefaultDataFolder := ExeVersion.ProgramFilePath;
      if fDefaultDataFileName='' then
        FN := ExeVersion.ProgramName else
        FN := fDefaultDataFileName;
      fORM.ServerName := StringToUTF8(IncludeTrailingPathDelimiter(
        fDefaultDataFolder))+FN+Ext;
    end;
  end;

begin
  if aModel=nil then
    if riCreateVoidModelIfNone in aOptions then begin
      aModel := TSQLModel.Create([],fRoot);
      include(aOptions,riOwnModel);
    end else
       raise EDDDInfraException.CreateUTF8('%.NewRestInstance(aModel=nil)',[self]);
  if fRoot='' then // supplied TSQLModel.Root is the default root URI
    fRoot := aModel.Root else
    aModel.Root := fRoot;
  if fORM.Kind='' then
    if riDefaultLocalSQlite3IfNone in aOptions then begin
      fORM.Kind := 'TSQLRestServerDB'; // SQlite3 engine by default
      ComputeDefaultORMServerName('.db');
    end else
    if riDefaultInMemorySQLite3IfNone in aOptions then begin
      fORM.Kind := 'TSQLRestServerDB';
      fORM.ServerName := SQLITE_MEMORY_DATABASE_NAME;
    end else
    if riDefaultFullMemoryIfNone in aOptions then
      fORM.Kind := 'TSQLRestServerFullMemory' else
    if riDefaultLocalBinaryFullMemoryIfNone in aOptions then begin
      fORM.Kind := 'TSQLRestServerFullMemory';
      fORM.DatabaseName := 'binary'; // as TSQLRestServerFullMemory.DefinitionTo
      ComputeDefaultORMServerName('.data');
    end;
  result := nil;
  try
    if fORM.Kind='' then
      exit;
    if (optEraseDBFileAtStartup in Options) and (fORM.ServerName<>'') then
      if (fORM.Kind='TSQLRestServerDB') or
         (fORM.Kind='TSQLRestServerFullMemory') then
        DeleteFile(UTF8ToString(fORM.ServerName));
    {$ifndef DDDNOMONGODB}
    result := TSQLRestMongoDBCreate(aModel,ORM,
      riHandleAuthentication in aOptions,aMongoDBOptions,aMongoDBIdentifier);
    {$endif}
    {$ifdef DDDNOSYNDB}
    result := TSQLRest.CreateTryFrom(aModel,ORM,riHandleAuthentication in aOptions);
    {$else}
    if result=nil then // failed to use MongoDB -> try external or internal DB
      result := TSQLRestExternalDBCreate(aModel,ORM,
        riHandleAuthentication in aOptions,aExternalDBOptions);
    {$endif}
    if result=nil then
      exit; // no match or wrong parameters
    if result.InheritsFrom(TSQLRestServer) then
    try // initialize server features
      if (WrapperTemplateFolder<>'') and DirectoryExists(WrapperTemplateFolderFixed) then
        AddToServerWrapperMethod(TSQLRestServer(result),[WrapperTemplateFolderFixed],
          WrapperSourceFolderFixed);
      RestServerDBSetOptions(TSQLRestServer(result), Options);
      if not (riWithInternalState in aOptions) then
        TSQLRestServer(result).Options := TSQLRestServer(result).Options+[rsoNoInternalState];
      if riCreateMissingTables in aOptions then
        TSQLRestServer(result).CreateMissingTables;
    except
      FreeAndNil(result);
    end; // note: TSQLRestClient.SetUser() has been called in TSQLRest*DBCreate()
    if not(optNoSystemUse in Options) then
      // if not already set, update cpu/ram info every 10 sec + 10 min history
      result.SystemUseTrack(10);
  finally
    if riOwnModel in aOptions then
      if result=nil then // avoid memory leak
        aModel.Free else
        aModel.Owner := result;
    if (result=nil) and (riRaiseExceptionIfNoRest in aOptions) then
      raise EDDDInfraException.CreateUTF8('Impossible to initialize % on %/%',
        [fORM.Kind,fORM.ServerName,fRoot]);
  end;
end;

class procedure TDDDRestSettings.RestServerDBSetOptions(DB: TSQLRestServer;
  Options: TDDDRestSettingsOptions);
begin
  if DB = nil then
    exit;
  if DB.InheritsFrom(TSQLRestServerDB) then
    with TSQLRestServerDB(DB).DB do begin // tune internal SQlite3 engine
      if optEraseDBFileAtStartup in Options then
        DeleteFile(FileName);
      if optSQlite3FileSafeNonExclusive in Options then
        LockingMode := lmNormal else
        LockingMode := lmExclusive;
      if optSQlite3FileSafeSlowMode in Options then
        Synchronous := smNormal else
        Synchronous := smOff;
      if optSQlite3File4MBCacheSize in Options then
        CacheSize := (4 shl 20) div PageSize;
      if optSQLite3LogQueryPlan in Options then
        TSQLRestServerDB(DB).StatementPreparedSelectQueryPlan := true;
    end;
  DB.NoAJAXJSON := not (optForceAjaxJson in Options);
end;

function TDDDRestSettings.NewRestServerDB(const aDBFileName: TFileName;
  const aModelRoot: RawUTF8; const aModelTables: array of TSQLRecordClass;
  aOptions: TDDDRestSettingsOptions; aCacheSize: cardinal): TSQLRestServerDB;
begin
  result := TSQLRestServerDB.CreateWithOwnModel(aModelTables, DefaultDataFolder +
    UTF8ToString(DefaultDataFileName) + aDBFileName, false, aModelRoot, '', aCacheSize);
  RestServerDBSetOptions(result, Options); // tune internal SQlite3 engine
  result.Options := result.Options+[rsoNoInternalState];
  result.CreateMissingTables;
end;

procedure TDDDRestSettings.WrapperGenerate(Rest: TSQLRestServer; Port: integer;
  const DestFile, Template: TFileName);
var dest: TFileName;
    mus: RawUTF8;
begin
  if (self = nil) or (Rest = nil) then
    exit;
  if DestFile = '' then
    dest := ExeVersion.ProgramFilePath+'mORMotClient.asc' else
    dest := DestFile;
  mus := StringFromFile(WrapperTemplateFolderFixed(true)+Template);
  FileFromString(WrapperFromModel(Rest,mus,'',Port),dest);
end;

function TDDDRestSettings.WrapperSourceFolderFixed: TFileName;
begin
  if fWrapperSourceFolders='' then
    result := '' else begin
    if fWrapperSourceFolderFixed='' then
      fWrapperSourceFolderFixed := IncludeTrailingPathDelimiter(StringReplace(
        fWrapperSourceFolders,'/',PathDelim,[rfReplaceAll]));
    result := fWrapperSourceFolders;
  end;
end;

function TDDDRestSettings.WrapperTemplateFolderFixed(ReturnLocalIfNoneSet: boolean): TFileName;
begin
  if fWrapperTemplateFolder='' then
    if ReturnLocalIfNoneSet then
      result := ExeVersion.ProgramFilePath
    else
      result := '' else begin
      if fWrapperTemplateFolderFixed='' then
        fWrapperTemplateFolderFixed := StringReplace(
          fWrapperTemplateFolder,'/',PathDelim,[rfReplaceAll]);
      result := fWrapperTemplateFolder;
    end;
end;


{ TDDDAppSettingsRest }

procedure TDDDAppSettingsRest.Initialize(const aDescription: string);
begin
  inherited Initialize(aDescription);
  if ServerPort='' then
    ServerPort := {$ifdef LINUX}'8888'{$else}'888'{$endif};
end;



{ TDDDAdministratedDaemonSettings }

procedure TDDDAdministratedDaemonSettings.Initialize(
  const aDescription, aServiceName, aServiceDisplayName, aAppUserModelID: string;
  const aServiceDependencies: TStringDynArray);
begin
  inherited Initialize(aDescription);
  if FServiceName='' then
    FServiceName := aServiceName;
  if FServiceDisplayName='' then
    FServiceDisplayName := aServiceDisplayName;
  FServiceDependencies := aServiceDependencies;
  if FAppUserModelID='' then
    FAppUserModelID := aAppUserModelID;
end;

function TDDDAdministratedDaemonSettings.SettingsFolder: TFileName;
begin
  if fStorage.InheritsFrom(TDDDAppSettingsStorageFile) then
    result := ExtractFilePath(TDDDAppSettingsStorageFile(fStorage).fSettingsJsonFileName)
  else
    result := ExeVersion.ProgramFilePath;
end;


{ TDDDSocketThreadSettings }

constructor TDDDSocketThreadSettings.Create;
begin
  inherited Create;
  fSocketLoopPeriod := 100;
  fConnectionAttemptsInterval := 5;
  fMonitoringInterval := 120*1000; // log monitoring information every 2 minutes
  fSocketBufferBytes := 32768; // 32KB
  fSocketMaxBufferBytes := 16777216; // 16MB
end;

function TDDDSocketThreadSettings.GetHostPort: RawUTF8;
begin
  FormatUTF8('%:%', [fHost, fPort], result);
end;

function TDDDSocketThreadSettings.SetHostPort(
  const IpPort: RawByteString; defaultPort: integer): boolean;
var p: RawUTF8;
begin
  Split(IpPort,':',fHost,p);
  result := false;
  if trim(fHost) = '' then
    exit;
  fPort := GetIntegerDef(pointer(p), defaultPort);
  result := fPort > 0;
end;


{ TDDDServicesLogRestSettings }

function TDDDServicesLogRestSettings.NewRestInstance(
  aRootSettings: TDDDAppSettingsAbstract; aMainRestWithServices: TSQLRestServer;
  const aLogClass: array of TSQLRecordServiceLogClass;
  const aExcludedMethodNamesCSV: RawUTF8; aShardRange: TID): TSQLRest;
var classes: TSQLRecordClassDynArray;
    server: TSQLRestServer;
    fn: TFileName;
    i: integer;
begin
  if length(aLogClass)=0 then begin
    SetLength(classes,1);
    classes[0] := TSQLRecordServiceLog;
  end else begin
    SetLength(classes,length(aLogClass));
    for i := 0 to high(aLogClass) do
      classes[i] := aLogClass[i];
  end;
  if (fShardDBCount > 0) and (aShardRange > 100) and (length(classes)=1) then
    {$WARNINGS OFF} // methods are pure abstract, but fine with a single class
    result := TSQLRestServer.CreateWithOwnModel(classes,false,fRoot) else
    {$WARNINGS ON}
    result := inherited NewRestInstance(aRootSettings,TSQLModel.Create(classes),
      [riOwnModel,riDefaultLocalSQlite3IfNone,riCreateMissingTables]);
  if result=nil then
    exit;
  if (fShardDBCount > 0) and (aShardRange > 100) then begin
    server := result as TSQLRestServer;
    fn := IncludeTrailingPathDelimiter(fDefaultDataFolder)+TFileName(fDefaultDataFileName);
    if not server.StaticDataAdd(TSQLRestStorageShardDB.Create(
       classes[0], server, aShardRange, [], fn, fShardDBCount)) then
      raise EDDDInfraException.CreateUTF8('%.NewRestInstance(%) StaticDataAdd(%)=false',
        [self,fRoot,classes[0]]);
  end else
    if result.InheritsFrom(TSQLRestServerDB) then
      TSQLRestServerDB(result).DB.UseCache := false;
  // set the first supplied class type to log services
  if (aMainRestWithServices <> nil) and classes[0].InheritsFrom(TSQLRecordServiceLog) then
    (aMainRestWithServices.ServiceContainer as TServiceContainerServer).
      SetServiceLog(result,TSQLRecordServiceLogClass(classes[0]),aExcludedMethodNamesCSV);
end;


{ TDDDAppSettingsStorageAbstract }

constructor TDDDAppSettingsStorageAbstract.Create(
  const aInitialJSON: RawUTF8);
begin
  inherited Create;
  if aInitialJSON='' then
    exit;
  fInitialJsonContent := aInitialJSON;
end;

function TDDDAppSettingsStorageAbstract.SetOwner(
  aOwner: TDDDAppSettingsAbstract): boolean;
begin
  if self=nil then
    result := false
  else begin
    fOwner := aOwner;
    result := JSONSettingsToObject(fInitialJsonContent, fOwner);
  end;
end;

procedure TDDDAppSettingsStorageAbstract.Store(const aJSON: RawUTF8);
begin
  if aJSON=fInitialJsonContent then
    exit;
  fInitialJsonContent := aJSON;
  InternalStore;
end;


{ TDDDAppSettingsStorageFile }

constructor TDDDAppSettingsStorageFile.Create(const aSettingsJsonFileName: TFileName);
var content: RawUTF8;
begin
  if aSettingsJsonFileName<>'' then
    fSettingsJsonFileName := aSettingsJsonFileName else
    fSettingsJsonFileName := ChangeFileExt(ExeVersion.ProgramFileName,'.settings');
  fSettingsJsonFileName := ExpandFileName(fSettingsJsonFileName);
  content := AnyTextFileToRawUTF8(fSettingsJsonFileName,true);
  inherited Create(content);
end;

function TDDDAppSettingsStorageFile.FileNameRelativeToSettingsFile(
  const aFileName: TFileName): TFileName;
var path,settings: TFileName;
begin
  path := ExtractFilePath(ExpandFileName(aFileName));
  settings := ExtractFilePath(ExpandFileName(SettingsJsonFileName));
  result := ExtractRelativePath(settings,path)+ExtractFileName(aFileName);
end;

procedure TDDDAppSettingsStorageFile.InternalStore;
begin
  FileFromString(fInitialJsonContent,fSettingsJsonFileName);
end;


{ TDDDMongoDBRestSettings }

procedure TDDDMongoDBRestSettings.SetDefaults(const Root, MongoServerAddress,
  MongoDatabase, MongoUser, MongoPassword: RawUTF8; TLS: boolean);
begin
  if fORM.Kind<>'' then
    exit;
  fRoot := Root;
  if TLS then
    fORM.Kind := 'MongoDBS' else
    fORM.Kind := 'MongoDB';
  fORM.ServerName := MongoServerAddress;
  fORM.DatabaseName := MongoDatabase;
  fORM.User := MongoUser;
  fORM.PasswordPlain := MongoPassword;
end;


{ TDDDEmailerSettings }

constructor TDDDEmailerSettings.Create;
begin
  inherited Create;
  fSMTP := SMTP_DEFAULT;
end;

initialization
  TSynPersistentWithPasswordUserCrypt := CryptDataForCurrentUser;

end.
