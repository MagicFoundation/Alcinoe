/// minimum stand-alone cross-platform REST process for mORMot client
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynCrossPlatformREST;

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
  - hanspi
  - warleyalex

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


  Should compile with Delphi for any platform (including NextGen for mobiles),
    with FPC 2.7 or Kylix, and with SmartMobileStudio 2.1.1

}

{$ifdef DWSCRIPT} // always defined since SMS 1.1.2
  {$define ISDWS}           // e.g. for SmartMobileStudio or Delphi Web Script
  {$define ISSMS}           // for SmartMobileStudio
{$else}
  {$i SynCrossPlatform.inc} // define e.g. HASINLINE
{$endif}

interface

{$ifdef ISDWS}
uses
  SmartCL.System,
  System.Types,
  ECMA.Json,
{$else}
uses
  SysUtils,
  Classes,
  TypInfo,
{$ifdef ISDELPHI2010}
  System.Generics.Collections,
{$endif}
{$ifndef NEXTGEN}
  Contnrs,
{$endif}
  Variants,
  SynCrossPlatformJSON,
{$endif ISDWS}
  SynCrossPlatformSpecific,
  SynCrossPlatformCrypto;


const
  /// maximum number of fields in a database Table
  MAX_SQLFIELDS = 256;

  /// used as "stored AS_UNIQUE" published property definition in TSQLRecord
  AS_UNIQUE = false;

type
  /// alias to share the same string type between client and server
  RawUTF8 = string;

  TSQLRest = class;
  TSQLRecord = class;
  TSQLModel = class;

  TSQLRecordClass = class of TSQLRecord;
  TSQLRecordClassDynArray = array of TSQLRecordClass;

  {$ifdef ISDWS}

  // UTILS functions 
  function window: variant; external 'window' property;
  function document: variant; external 'document' property;

  // URI functions
  function EncodeURIComponent(str: String): String; external 'encodeURIComponent';
  function DecodeURIComponent(str: String): String; external 'decodeURIComponent';
  function EncodeURI(str: String): String; external 'encodeURI';
  function DecodeURI(str: String): String; external 'decodeURI';

  // Variant management 
  function VarIsValidRef(const aRef: Variant): Boolean;

type					   
  // circumvent limited DWS / SMS syntax
  TPersistent = TObject;
  TObjectList = array of TObject;
  // stored as binary, transmitted as Base64 (VariantToBlob=atob and BlobToVariant=btoa)
  TSQLRawBlob = variant;
  // TTimeLogBits.Value has a 38-bit precision, so features exact representation
  // as JavaScript numbers (stored in a 52-bit mantissa)
  TTimeLog = Int53;
  TModTime = TTimeLog;
  TCreateTime = TTimeLog;
  TGUID = string;
  TSQLFieldBit = enum (Low = 0, High = MAX_SQLFIELDS-1);

  ERestException = class(EW3Exception);

  /// handle a JSON result table, as returned by mORMot's REST server ORM
  // - we define a dedicated class to by-pass SynCrossPlatformJSON unit
  TSQLTableJSON = class
  protected
    fInternalState: cardinal;
    fFieldCount, fRowCount, fCurrentRow: integer;
    fFieldNames: TStrArray;
    fValues: TVariantDynArray;
  public
    /// parse the supplied JSON content
    constructor Create(const aJSON: string);
    /// to be called in a loop to iterate through all data rows
    // - if returned true, Object published properties will contain this row
    function FillOne(Value: TSQLRecord; SeekFirst: boolean=false): boolean;
  end;
 
  {$else}

  /// Exception type raised when working with REST access
  ERestException = class(Exception);

  /// alias to share the same blob type between client and server
  TSQLRawBlob = TByteDynArray;

  /// fast bit-encoded date and time value
  TTimeLog = type Int64;

  /// used to define a field which shall be set at each modification
  TModTime = type TTimeLog;

  /// used to define a field which shall be set at record creation
  TCreateTime = type TTimeLog;

  /// used to identify the a field in a Table as in TSQLFieldBits
  TSQLFieldBit = 0..MAX_SQLFIELDS-1;

  /// handle a JSON result table, as returned by mORMot's REST server ORM
  // - this class is expected to work with TSQLRecord instances only
  // - it will let any "RowID" JSON key match TSQLRecord.ID property
  TSQLTableJSON = class(TJSONTableObject)
  protected
    fInternalState: cardinal;
    /// allow to let "RowID" JSON key match TSQLRecord.ID
    function GetPropInfo(aTypeInfo: TRTTITypeInfo; const PropName: string): TRTTIPropInfo; override;
  public
    /// to be called in a loop to iterate through all data rows
    // - if returned true, Object published properties will contain this row
    function FillOne(aValue: TSQLRecord; aSeekFirst: boolean=false): boolean;
  end;

  {$endif ISDWS}

  /// Exception type raised when working with interface-based service process
  EServiceException = class(ERestException);

  /// the available logging events, as handled by our Cross-Platform units
  // - defined with the same values in SynCommons.pas
  // - sllInfo will log general information events
  // - sllDebug will log detailed debugging information
  // - sllTrace will log low-level step by step debugging information
  // - sllWarning will log unexpected values (not an error)
  // - sllError will log errors
  // - sllEnter will log every method start
  // - sllLeave will log every method exit
  // - sllLastError will log the GetLastError OS message
  // - sllException will log all exception raised - available since Windows XP
  // - sllExceptionOS will log all OS low-level exceptions (EDivByZero,
  // ERangeError, EAccessViolation...)
  // - sllMemory will log memory statistics
  // - sllStackTrace will log caller's stack trace (it's by default part of
  // TSynLogFamily.LevelStackTrace like sllError, sllException, sllExceptionOS,
  // sllLastError and sllFail)
  // - sllFail was defined for TSynTestsLogged.Failed method, and can be used
  // to log some customer-side assertions (may be notifications, not errors)
  // - sllSQL is dedicated to trace the SQL statements
  // - sllCache should be used to trace the internal caching mechanism
  // - sllResult could trace the SQL results, JSON encoded
  // - sllDB is dedicated to trace low-level database engine features
  // - sllHTTP could be used to trace HTTP process
  // - sllClient/sllServer could be used to trace some Client or Server process
  // - sllServiceCall/sllServiceReturn to trace some remote service or library
  // - sllUserAuth to trace user authentication (e.g. for individual requests)
  // - sllCustom* items can be used for any purpose
  // - sllNewRun will be written when a process opens a rotated log
  // - sllDDDError will log any DDD-related low-level error information
  // - sllDDDInfo will log any DDD-related low-level debugging information
  TSynLogInfo = (
    sllNone, sllInfo, sllDebug, sllTrace, sllWarning, sllError,
    sllEnter, sllLeave,
    sllLastError, sllException, sllExceptionOS, sllMemory, sllStackTrace,
    sllFail, sllSQL, sllCache, sllResult, sllDB, sllHTTP, sllClient, sllServer,
    sllServiceCall, sllServiceReturn, sllUserAuth,
    sllCustom1, sllCustom2, sllCustom3, sllCustom4,
    sllNewRun, sllDDDError, sllDDDInfo);

  /// used to define a set of logging level abilities
  // - i.e. a combination of none or several logging event
  // - e.g. use LOG_VERBOSE constant to log all events, or LOG_STACKTRACE
  // to log all errors and exceptions
  TSynLogInfos = set of TSynLogInfo;

  /// callback event used to write some text to a logging system
  // - could be a local file (not for SMS apps), or a remote log server
  // - the Text is already in the same format than the one generated by TSynLog  
  TOnSQLRestLog = procedure(const Text: string) of object;

  /// used to store bit set for all available fields in a Table
  // - in this unit, field at index [0] indicates TSQLRecord.ID
  TSQLFieldBits = set of TSQLFieldBit;

  /// a published property kind
  // - does not match mORMot.pas TSQLFieldType: here we recognize only types
  // which may expect a special behavior in this unit
  TSQLFieldKind = (
    sftUnspecified, sftDateTime, sftTimeLog, sftBlob, sftModTime, sftCreateTime,
    sftRecord, sftVariant);

  /// a set of published property Kind
  TSQLFieldKinds = set of TSQLFieldKind;

  { TODO: TID should be a string since number is limited to 53-bit in JavaScript
    -> define and use an explicit Int52 type for SMS }
  /// the TSQLRecord primary key is a 64 bit integer
  TID = {$ifndef ISDWS}type{$endif} Int64;

  /// a dynamic array of TSQLRecord primary keys
  // - used e.g. for BATCH process
  TIDDynArray = array of TID;

  /// store information of one TSQLRecord published property
  TSQLModelInfoPropInfo = class
  public
    /// the name of the published property
    // - e.g. 'FirstName'
    Name: string;
    /// the property field type
    Kind: TSQLFieldKind;
    {$ifdef ISDWS}
    /// index of the published property in the associated Prop[]
    FieldIndex: TSQLFieldBit;
    {$else}
    /// the property type name, as retrieved from RTTI
    TypeName: string;
    /// RTTI information about the published property
    RTTI: TRTTIPropInfo;
    /// initialize the instance
    constructor CreateFrom(aRTTI: TRTTIPropInfo);
    {$endif}
  end;

  /// store information of all TSQLRecord published properties
  TSQLModelInfoPropInfoDynArray = array of TSQLModelInfoPropInfo;
  
  /// store information of one TSQLRecord class
  TSQLModelInfo = class
  public
    /// the TSQLRecord class type itself
    Table: TSQLRecordClass;
    /// the short name of the class
    // - i.e. 'People' for TSQLRecordPeople
    Name: string;
    /// information about every published property
    // - first is always the ID field
    Prop: TSQLModelInfoPropInfoDynArray;
    /// specifies the "simple" fields, i.e. all non BLOB fields
    SimpleFields: TSQLFieldBits;
    /// specifies the BLOB fields
    BlobFields: TSQLFieldBits;
    /// specifies all fields, including simple and BLOB fields
    AllFields: TSQLFieldBits;
    /// specifies the TModTime fields
    ModTimeFields: TSQLFieldBits;
    /// specifies the TCreateTime fields
    CreateTimeFields: TSQLFieldBits;
    /// specifies the TModTime and TCreateTime fields
    ModAndCreateTimeFields: TSQLFieldBits;
    /// specifies the Record fields
    RecordFields: TSQLFieldBits;
    /// specifies the Variant fields
    VariantFields: TSQLFieldBits;
    /// contains all published properties kind
    HasKind: TSQLFieldKinds;
    /// TRUE if has TModTime or TCreateTime fields
    HasTimeFields: boolean;
    {$ifdef ISSMS}
    /// allow fast by-name access to Prop[]
    PropCache: variant;
    {$else}
    /// finalize the memory used
    destructor Destroy; override;
    {$endif}
    /// initialize the class member for the supplied TSQLRecord
    constructor CreateFromRTTI(aTable: TSQLRecordClass);
    /// FieldNames='' to retrieve simple fields, '*' all fields, or as specified
    function FieldNamesToFieldBits(const FieldNames: string;
      IncludeModTimeFields: boolean): TSQLFieldBits;
    /// return the corresponding field names
    function FieldBitsToFieldNames(const FieldBits: TSQLFieldBits): string;
    /// set TModTime and TCreateFields
    procedure ComputeFieldsBeforeWrite(aClient: TSQLRest;
      Value: TSQLRecord; AndCreate: Boolean);
    /// compute the 'SELECT ... FROM ...' corresponding to the supplied fields
    function SQLSelect(const FieldNames: string): string;
    /// save the specified record as JSON for record adding
    function ToJSONAdd(Client: TSQLRest; Value: TSQLRecord; ForceID: boolean;
      const FieldNames: string): string;
    /// save the specified record as JSON for record update
    function ToJSONUpdate(Client: TSQLRest; Value: TSQLRecord;
      const FieldNames: string; ForceID: boolean): string;
    /// save the specified record as JSON
    function ToJSON(Value: TSQLRecord; const Fields: TSQLFieldBits): string; overload;
  end;

  /// store information of several TSQLRecord class
  TSQLModelInfoDynArray = array of TSQLModelInfo;

  /// store the database model
  TSQLModel = class
  protected
    fRoot: string;
    fInfo: TSQLModelInfoDynArray;
  public
    /// initialize the Database Model
    // - set the Tables to be associated with this Model, as TSQLRecord classes
    // - set the optional Root URI path of this Model - default is 'root'
    constructor Create(const Tables: array of TSQLRecordClass;
      const aRoot: string {$ifndef ISDWS}='root'{$endif});
    /// register a new Table class to this Model
    procedure Add(Table: TSQLRecordClass);
    {$ifndef ISSMS}
    /// finalize the memory used
    destructor Destroy; override;
    {$endif}
    /// get index of aTable in Tables[], returns -1 if not found
    function GetTableIndex(aTable: TSQLRecordClass): integer; overload;
    /// get index of aTable in Tables[], returns -1 if not found
    function GetTableIndex(const aTableName: string): integer; overload;
    /// get index of aTable in Tables[], raise an ERestException if not found
    function GetTableIndexExisting(aTable: TSQLRecordClass): integer;
    /// get the RTTI information for the specified class or raise an ERestException
    function InfoExisting(aTable: TSQLRecordClass): TSQLModelInfo;
    /// the RTTI information for each class
    property Info: TSQLModelInfoDynArray read fInfo;
    /// the Root URI path of this Database Model
    property Root: string read fRoot;
  end;

  {$ifdef ISSMS}
  /// low-level structure used for server-side generated pseudo RTTI
  TRTTIPropInfos = class
  public
    Props: TSQLModelInfoPropInfoDynArray;
    PropCache: variant;
    /// define the published properties
    // - optional PropKinds[] can override default sftUnspecified type
    constructor Create(const PropNames: array of string;
      const PropKinds: array of TSQLFieldKind);
  end;
  {$endif}

  /// abstract ORM class to access remote tables
  // - in comparison to mORMot.pas TSQLRecord published fields, dynamic arrays
  // shall be defined as variant (since SynCrossPlatformJSON do not serialize)
  // - inherit from TPersistent to have RTTI for its published properties
  // (SmartMobileStudio does not allow {$M+} in the source)
  TSQLRecord = class(TPersistent)
  protected
    fID: TID;
    fInternalState: cardinal;
    fFill: TSQLTableJSON;
    {$ifdef ISSMS}
    class function GetRTTI: TRTTIPropInfos;
    /// you should override these methods
    class function ComputeRTTI: TRTTIPropInfos; virtual;
    procedure SetProperty(FieldIndex: integer; const Value: variant); virtual;
    function GetProperty(FieldIndex: integer): variant; virtual;
    {$endif}
  public
    /// this constructor initializes the record
    constructor Create; overload; virtual;
    /// this constructor loads a record from a REST instance from its ID
    constructor Create(aClient: TSQLRest; aID: TID;
      ForUpdate: boolean=false); overload;
    /// this constructor loads a record from a REST instance
    // - you can bind parameters by using ? in the SQLWhere clause
    // - use DateTimeToSQL() for date/time database fields
    // - FieldNames='' retrieve simple fields, '*' all fields, or as specified
    constructor Create(aClient: TSQLRest; const FieldNames, SQLWhere: string;
      const BoundsSQLWhere: array of const); overload;
    /// this constructor ask the server for a list of matching records
    // - you can bind parameters by using ? in the SQLWhere clause
    // - use DateTimeToSQL() for date/time database fields
    // - FieldNames='' retrieve simple fields, '*' all fields, or as specified
    // - then you can also loop through all rows with
    // ! while Rec.FillOne do
    // !   dosomethingwith(Rec);
    constructor CreateAndFillPrepare(aClient: TSQLRest; const FieldNames,
      SQLWhere: string; const BoundsSQLWhere: array of const);
    /// this constructor will loads a record from its variant representation
    // - will call internaly the FromJSON() method
    constructor CreateFromVariant(const aValue: variant);
    /// finalize the record memory
    destructor Destroy; override;
    /// fill the specified record from the supplied JSON
    function FromJSON(const aJSON: string): boolean;
    /// fill the specified record from its variant representation
    function FromVariant(const aValue: variant): boolean;
    {$ifdef ISSMS}
    /// fill the specified record from Names/Values pairs
    function FromNamesValues(const Names: TStrArray; const Values: TVariantDynArray;
      ValuesStartIndex: integer): boolean;
    {$endif}
    /// fill all published properties of this object with the next available
    // row of data, as returned by CreateAndFillPrepare() constructor
    function FillOne: boolean;
    /// go to the first data row, as returned by CreateAndFillPrepare(),
    // then fill all published properties of this object
    // - you can use it e.g. as:
    // ! while Rec.FillOne do
    // !   dosomethingwith(Rec);
    // ! if Rec.FillRewind then
    // ! repeat
    // !   dosomeotherthingwith(Rec);
    // ! until not Rec.FillOne;
    function FillRewind: boolean;
    /// get the object properties as JSON
    // - FieldNames='' to retrieve simple fields, '*' all fields, or as specified
    function ToJSON(aModel: TSQLModel; aFieldNames: string=''): string;
    /// get the object properties as a TJSONVariant document
    function ToVariant: variant;
    /// return the class type of this TSQLRecord
    function RecordClass: TSQLRecordClass;
      {$ifdef HASINLINE}inline;{$endif}
    /// contains the TSQLTableJSON instance after CreateAndFillPrepare()
    property FillTable: TSQLTableJSON read fFill;
    /// internal state counter of the mORMot server at last access time
    // - can be used to check if retrieved data may be out of date
    property InternalState: cardinal read fInternalState;
  published
    /// stores the record's primary key
    property ID: TID read fID write fID;
  end;

  /// table containing the available user access rights for authentication
  // - is added here since should be part of the model
  // - no wrapper is available to handle AccessRights, since for security
  // reasons it is not available remotely from client side
  TSQLAuthGroup = class(TSQLRecord)
  protected
    fIdent: string;
    fAccessRights: string;
    fSessionTimeOut: integer;
    {$ifdef ISSMS}
    class function ComputeRTTI: TRTTIPropInfos; override;
    procedure SetProperty(FieldIndex: integer; const Value: variant); override;
    function GetProperty(FieldIndex: integer): variant; override;
    {$endif}
  published
    /// the access right identifier, ready to be displayed
    // - the same identifier can be used only once (this column is marked as
    // unique via a "stored AS_UNIQUE" (i.e. "stored false") attribute)
    property Ident: string read fIdent write fIdent
      {$ifndef ISDWS}stored AS_UNIQUE{$endif};
    /// the number of minutes a session is kept alive
    property SessionTimeout: integer read fSessionTimeOut write fSessionTimeOut;
    /// a textual representation of a TSQLAccessRights buffer
    property AccessRights: string read fAccessRights write fAccessRights;
  end;

  /// class of the table containing the available user access rights for authentication
  TSQLAuthGroupClass = class of TSQLAuthGroup;

  /// table containing the Users registered for authentication
  TSQLAuthUser = class(TSQLRecord)
  protected
    fLogonName: string;
    fPasswordHashHexa: string;
    fDisplayName: string;
    fData: TSQLRawBlob;
    fGroup: TID;
    {$ifdef ISSMS}
    class function ComputeRTTI: TRTTIPropInfos; override;
    procedure SetProperty(FieldIndex: integer; const Value: variant); override;
    function GetProperty(FieldIndex: integer): variant; override;
    {$endif}
    procedure SetPasswordPlain(const Value: string);
  public
    /// able to set the PasswordHashHexa field from a plain password content
    // - in fact, PasswordHashHexa := SHA256('salt'+PasswordPlain) in UTF-8
    property PasswordPlain: string write SetPasswordPlain;
  published
    /// the User identification Name, as entered at log-in
    // - the same identifier can be used only once (this column is marked as
    // unique via a "stored AS_UNIQUE" - i.e. "stored false" - attribute), and
    // therefore indexed in the database (e.g. hashed in TSQLRestStorageInMemory)
    property LogonName: string read fLogonName write fLogonName
      {$ifndef ISDWS}stored AS_UNIQUE{$endif};
   /// the User Name, as may be displayed or printed
    property DisplayName: string read fDisplayName write fDisplayName;
    /// the hexa encoded associated SHA-256 hash of the password
    property PasswordHashHexa: string read fPasswordHashHexa write fPasswordHashHexa;
    /// the associated access rights of this user in TSQLAuthGroup
    // - access rights are managed by group
    // - note that 'Group' field name is not allowed by SQLite
    property GroupRights: TID read fGroup write fGroup;
    /// some custom data, associated to the User
    // - Server application may store here custom data
    // - its content is not used by the framework but 'may' be used by your
    // application
    property Data: TSQLRawBlob read fData write fData;
  end;

  TSQLRestServerAuthentication = class;

  /// class used for client authentication
  TSQLRestServerAuthenticationClass = class of TSQLRestServerAuthentication;

  /// the possible Server-side instance implementation patterns for
  // interface-based services 
  // - each interface-based service will be implemented by a corresponding
  // class instance on the server: this parameter is used to define how
  // class instances are created and managed
  // - on the Client-side, each instance will be handled depending on the
  // server side implementation (i.e. with sicClientDriven behavior if necessary)
  TServiceInstanceImplementation = (
    sicSingle, sicShared, sicClientDriven, sicPerSession, sicPerUser, sicPerGroup,
    sicPerThread);

  TSQLRestClientURI = class;

  /// abstract ancestor to all client-side interface-based services
  // - any overriden class will in fact call the server to execute its methods
  // - inherited classes are in fact the main entry point for all interface-based
  // services, without any interface use:
  // ! aCalculator := TServiceCalculator.Create(aClient);
  // ! try
  // !   aIntegerResult := aCalculator.Add(10,20);
  // ! finally
  // !   aCalculator.Free;
  // ! end;
  // - under SmartMobileStudio, calling Free is mandatory only for
  // sicClientDriven mode (to release the server-side associated session),
  // so e.g. for a sicShared instance, you can safely write:
  // ! aIntegerResult := TServiceCalculator.Create(aClient).Add(10,20);
  // - as you already noted, server-side interface-based services are in fact
  // consummed without any interface in this cross-platform unit!
  TServiceClientAbstract = class{$ifndef ISDWS}(TInterfacedObject){$endif}
  protected
    fClient: TSQLRestClientURI;
    fServiceName: string;
    fServiceURI: string;
    fInstanceImplementation: TServiceInstanceImplementation;
    fContractExpected: string;
    function GetClient: TSQLRestClientURI;
    function GetContractExpected: string;
    function GetInstanceImplementation: TServiceInstanceImplementation;
    function GetRunningInstance: TServiceClientAbstract;
    function GetServiceName: string;
    function GetServiceURI: string;
  public
    /// initialize the fake instance
    // - this method will synchronously (i.e. blocking) check the server
    // contract according to the one expected by the client
    // - overriden constructors will set the parameters expected by the server
    constructor Create(aClient: TSQLRestClientURI); virtual;
    /// the associated TSQLRestClientURI instance
    property Client: TSQLRestClientURI read GetClient;
    /// the unmangdled remote service name
    property ServiceName: string read GetServiceName;
    /// the URI to access to the remote service
    property ServiceURI: string read GetServiceURI;
    /// how this instance lifetime is expected to be handled
    property InstanceImplementation: TServiceInstanceImplementation read GetInstanceImplementation;
    /// the published service contract, as expected by both client and server
    property ContractExpected: string read GetContractExpected;
  end;

  {$ifndef ISDWS}
  /// all generated client interfaces will inherit from this abstract parent
  IServiceAbstract = interface
    ['{06F02DCC-0DD1-4961-A5F4-C11AE375F03B}']
    function GetClient: TSQLRestClientURI;
    function GetContractExpected: string;
    function GetInstanceImplementation: TServiceInstanceImplementation;
    function GetRunningInstance: TServiceClientAbstract;
    function GetServiceName: string;
    function GetServiceURI: string;
    /// the associated TSQLRestClientURI instance
    property Client: TSQLRestClientURI read GetClient;
    /// the unmangdled remote service name
    property ServiceName: string read GetServiceName;
    /// the URI to access to the remote service
    property ServiceURI: string read GetServiceURI;
    /// how this instance lifetime is expected to be handled
    property InstanceImplementation: TServiceInstanceImplementation read GetInstanceImplementation;
    /// the published service contract, as expected by both client and server
    property ContractExpected: string read GetContractExpected;
    /// the client class instance currently implementing this interface
    property RunningInstance: TServiceClientAbstract read GetRunningInstance;
  end;
  {$endif}

  /// abstract ancestor to all sicClientDriven interface-based services
  // - since server-side life-time is driven by the client, this kind of class
  // expects an explicit call to aService.Free (even on SmartMobileStudio)
  TServiceClientAbstractClientDriven = class(TServiceClientAbstract)
  protected
    fClientID: string;
  public
    /// initialize the fake instance and create the remote per-client session
    // - raise an EServiceException if a per-client session was already started
    // for the specified TSQLRestClientURI
    // - overriden constructors will set the parameters expected by the server
    constructor Create(aClient: TSQLRestClientURI); override;
    /// this overriden method (called at aService.Free) will notify the server
    destructor Destroy; override;
    /// the currently running instance ID on the server side
    // - only one instance is allowed per TSQLRestClientURI process
    property ClientID: string read fClientID;
  end;

  /// class type used to identify an interface-based service
  // - we do not rely on interfaces here, but simply on abstract classes
  TServiceClientAbstractClass = class of TServiceClientAbstract;

  /// class used to determine the protocol of interface-based services
  // - see TSQLRestRoutingREST and TSQLRestRoutingJSON_RPC
  // for overridden methods - NEVER set this abstract TSQLRestRoutingAbstract
  // class on TSQLRest.ServicesRouting property !
  TSQLRestRoutingAbstract = class
  public
    /// at Client Side, compute URI and BODY according to the routing scheme
    // - abstract implementation which is to be overridden
    // - as input, "method" should be the method name to be executed for "uri",
    // "params" should contain the incoming parameters as JSON array (with []),
    // and "clientDriven" ID should contain the optional Client ID value
    // - at output, should update the HTTP "uri" corresponding to the proper
    // routing, and should return the corresponding HTTP body within "sent"
    class procedure ClientSideInvoke(var uri: string;
      const method, params, clientDrivenID: string; var sent: string); virtual; abstract;
  end;

  /// used to define the protocol of interface-based services
  TSQLRestRoutingAbstractClass = class of TSQLRestRoutingAbstract;

  /// default simple REST protocol for interface-based services
  // - this is the default protocol used by TSQLRest
  TSQLRestRoutingREST = class(TSQLRestRoutingAbstract)
  public
    /// at Client Side, compute URI and BODY according to RESTful routing scheme
    // - e.g. on input uri='root/Calculator', method='Add', params='[1,2]' and
    // clientDrivenID='1234' -> on output uri='root/Calculator.Add/1234' and
    // sent='[1,2]'
    class procedure ClientSideInvoke(var uri: string;
      const method, params, clientDrivenID: string; var sent: string); override;
  end;

  /// JSON/RPC protocol for interface-based services
  // - alternative to the TSQLRestRoutingREST default protocol set by TSQLRest
  TSQLRestRoutingJSON_RPC = class(TSQLRestRoutingAbstract)
  public
    /// at Client Side, compute URI and BODY according to JSON/RPC routing scheme
    // - e.g. on input uri='root/Calculator', method='Add', params='[1,2]' and
    // clientDrivenID='1234' -> on output uri='root/Calculator' and
    // sent={"method":"Add","params":[1,2],"id":1234}
    class procedure ClientSideInvoke(var uri: string;
      const method, params, clientDrivenID: string; var sent: string); override;
  end;

  /// the available options for TSQLRest.BatchStart() process
  // - boInsertOrIgnore will create 'INSERT OR IGNORE' statements instead of
  // plain 'INSERT' - by now, only direct SQLite3 engine supports it on server
  TSQLRestBatchOption = (
    boInsertOrIgnore);

  /// a set of options for TSQLRest.BatchStart() process
  TSQLRestBatchOptions = set of TSQLRestBatchOption;

  {$ifdef ISSMS}
  /// callback used e.g. by TSQLRestClientURI.Connect() overloaded method
  TSQLRestEvent = procedure(Client: TSQLRestClientURI);

  /// callback which should return TRUE on process success, or FALSE on error
  TSQLRestEventProcess = function: boolean;

  {$else}
  TSQLRestLogClientThread = class;
  {$endif ISSMS}


  /// abstract REST access class
  TSQLRest = class
  protected
    fModel: TSQLModel;
    fServerTimeStampOffset: TDateTime;
    fBatch: string;
    fBatchTable: TSQLRecordClass;
    fBatchCount: integer;
    fServicesRouting: TSQLRestRoutingAbstractClass;
    fInternalState: cardinal;
    fOwnModel: boolean;
    fLogLevel: TSynLogInfos;
    fOnLog: TOnSQLRestLog;
    {$ifdef ISSMS}
    fLogClient: TSQLRestClientURI;
    procedure LogToRemoteServerText(const Text: string);
    {$else}
    fLogClient: TSQLRestLogClientThread; 
    fLogFileBuffer: array of byte;
    fLogFile: system.text;
    procedure LogToFileText(const Text: string);
    {$endif}
    procedure LogClose;
    function GetServerTimeStamp: TTimeLog;
    function SetServerTimeStamp(const ServerResponse: string): boolean;
    function InternalBatch(Table: TSQLRecordClass; const CMD: string; var Info: TSQLModelInfo): Integer;
    function ExecuteAdd(tableIndex: integer; const json: string): TID; virtual; abstract;
    function ExecuteUpdate(tableIndex: integer; ID: TID; const json: string): boolean; virtual; abstract;
    function ExecuteBatchSend(Table: TSQLRecordClass; const Data: string;
      var Results: TIDDynArray): integer; virtual; abstract;
  public
    /// initialize the class, and associate it to a specified database Model
    // - if aOwnModel is TRUE, this class destructor will free aModel instance
    constructor Create(aModel: TSQLModel; aOwnModel: boolean=false); virtual;
    /// will release the associated Model, if aOwnModel was TRUE at Create()
    destructor Destroy; override;

    /// get a member from its ID
    // - return true on success, and fill all simple fields
    function Retrieve(aID: TID; Value: TSQLRecord;
      ForUpdate: boolean=false): boolean; overload; virtual; abstract;
    /// get a member from a where clause
    // - you can bind parameters by using ? in the SQLWhere clause
    // - use DateTimeToSQL() for date/time database fields
    // - FieldNames='' retrieve simple fields, '*' all fields, or as specified
    function Retrieve(const FieldNames, SQLWhere: string;
     const BoundsSQLWhere: array of const; Value: TSQLRecord): boolean; overload;
    {$ifndef ISSMS}
    /// get a blob field content from its record ID and supplied blob field name
    // - returns true on success, and the blob binary data
    function RetrieveBlob(Table: TSQLRecordClass; aID: TID;
      const BlobFieldName: string; out BlobData: TSQLRawBlob): boolean; virtual; abstract;
    {$endif}
    /// execute directly a SQL statement, expecting a list of results
    // - return a result table on success, nil on failure
    // - you can bind parameters by using ? in the SQLWhere clause
    // - use DateTimeToSQL() for date/time database fields
    // - FieldNames='' retrieve simple fields, '*' all fields, or as specified
    function MultiFieldValues(Table: TSQLRecordClass; const FieldNames,
      SQLWhere: string; const BoundsSQLWhere: array of const;
      LimitFirstRow: Boolean=false): TSQLTableJSON; overload;
    /// execute directly a SQL statement, expecting a list of results
    // - return a result table on success, nil on failure
    // - FieldNames='' retrieve simple fields, '*' all fields, or as specified
    function MultiFieldValues(Table: TSQLRecordClass; const FieldNames,
      SQLWhere: string): TSQLTableJSON; overload;
    /// execute directly a SQL statement, returning a list of TSQLRecord
    // - you can bind parameters by using ? in the SQLWhere clause
    // - use DateTimeToSQL() for date/time database fields
    // - FieldNames='' retrieve simple fields, '*' all fields, or as specified
    function RetrieveList(Table: TSQLRecordClass; const FieldNames,
      SQLWhere: string; const BoundsSQLWhere: array of const): TObjectList; overload;
    /// execute directly a SQL statement, returning a list of data rows or nil
    function ExecuteList(const SQL: string): TSQLTableJSON; virtual; abstract;
    {$ifdef ISDELPHI2010} // Delphi 2009 generics support is buggy :(
    /// execute directly a SQL statement, returning a generic list of TSQLRecord
    // - you can bind parameters by using ? in the SQLWhere clause
    // - use DateTimeToSQL() for date/time database fields
    // - FieldNames='' retrieve simple fields, '*' all fields, or as specified
    function RetrieveList<T: TSQLRecord>(const FieldNames, SQLWhere: string; const BoundsSQLWhere: array of const): TObjectList<T>; overload;
    {$endif}

    /// create a new member, returning the newly created ID, or 0 on error
    // - if SendData is true, content of Value is sent to the server as JSON
    // - if ForceID is true, client sends the Value.ID field to use this ID for
    // adding the record (instead of a database-generated ID)
    // - by default, only simple fields are pushed to the server, but you may
    // specify a CSV list of field values to be transmitted - including blobs,
    // which will be sent as base-64 encoded JSON
    function Add(Value: TSQLRecord; SendData: boolean; ForceID: boolean=false;
      FieldNames: string=''): TID; virtual;
    /// delete a member
    function Delete(Table: TSQLRecordClass; ID: TID): boolean; virtual; abstract;
    /// update a member
    // - you can let default FieldNames='' to update simple fields, '*' to
    // update all fields (including BLOBs), or specify a CSV list of updated fields
    function Update(Value: TSQLRecord; FieldNames: string=''): boolean; virtual;

    /// begin a BATCH sequence to speed up huge database change
    // - then call BatchAdd(), BatchUpdate()  or BatchDelete() methods with the
    // proper class or instance of the
    // - at BatchSend call, all the sequence transactions will be sent at once
    // - at BatchAbort call, all operations will be aborted
    // - expect one TSQLRecordClass as parameter, which will be used for the whole
    //   sequence (in this case, you can't mix classes in the same BATCH sequence)
    // - if no TSQLRecordClass is supplied, the BATCH sequence will allow any
    //   kind of individual record in BatchAdd/BatchUpdate/BatchDelete
    // - return TRUE on success, FALSE if aTable is incorrect or a previous BATCH
    //   sequence was already initiated
    // - this method includes a AutomaticTransactionPerRow parameter, which will
    // let all BATCH process be executed on the server side within an unique
    // transaction grouped by the given number of rows
    function BatchStart(aTable: TSQLRecordClass;
      AutomaticTransactionPerRow: cardinal=10000;
      BatchOptions: TSQLRestBatchOptions=[]): boolean; virtual;
    /// create a new member in current BATCH sequence
    // - similar to Add(), but in BATCH mode: nothing is sent until BatchSend()
    // - returns the corresponding index in the current BATCH sequence, -1 on error
    // - you can set FieldNames='' to sent simple fields, '*' to add all fields
    // (including BLOBs), or specify a CSV list of added fields
    // - this method will always compute and send TCreateTime/TModTime fields
    function BatchAdd(Value: TSQLRecord; SendData: boolean; ForceID: boolean=false;
      FieldNames: string=''): integer;
    /// update a member in current BATCH sequence
    // - similar to Update(), but in BATCH mode: nothing is sent until BatchSend()
    // - returns the corresponding index in the current BATCH sequence, -1 on error
    // - you can set FieldNames='' to sent simple fields, '*' to add all fields
    // (including BLOBs), or specify a CSV list of added fields
    // - this method will always compute and send any TModTime fields
    function BatchUpdate(Value: TSQLRecord; FieldNames: string=''): integer;
    /// delete a member in current BATCH sequence
    // - similar to Delete(), but in BATCH mode: nothing is sent until BatchSend()
    // - returns the corresponding index in the current BATCH sequence, -1 on error
    // - deleted record class is the TSQLRecordClass used at BatchStart()
    // call: it will fail if no class was specified for this BATCH sequence
    function BatchDelete(ID: TID): integer; overload;
    /// delete a member in current BATCH sequence
    // - similar to Delete(), but in BATCH mode: nothing is sent until BatchSend()
    // - returns the corresponding index in the current BATCH sequence, -1 on error
    // - with this overloaded method, the deleted record class is specified:
    // no class shall have been set at BatchStart() call, or should be the same
    function BatchDelete(Table: TSQLRecordClass; ID: TID): integer; overload;
    /// delete a member in current BATCH sequence
    // - similar to Delete(), but in BATCH mode: nothing is sent until BatchSend()
    // - returns the corresponding index in the current BATCH sequence, -1 on error
    function BatchDelete(Value: TSQLRecord): integer; overload;
    /// retrieve the current number of pending transactions in the BATCH sequence
    // - every call to BatchAdd/Update/Delete methods increases this count
    function BatchCount: integer;
    /// execute a BATCH sequence started by BatchStart() method
    // - send all pending BatchAdd/Update/Delete statements to the remote server
    // - will return the URI Status value, i.e. 200/HTTP_SUCCESS OK on success
    // - a dynamic array of 64 bit integers will be created in Results,
    // containing all ROWDID created for each BatchAdd call, or 200
    // (=HTTP_SUCCESS) for all successfull BatchUpdate/BatchDelete, or 0 on error
    // - any error during server-side process MUST be checked against Results[]
    // (the main URI Status is 200 if about communication success, and won't
    // imply that all statements in the BATCH sequence were successfull
    function BatchSend(var Results: TIDDynArray): integer;
    /// abort a BATCH sequence started by BatchStart() method
    // - in short, nothing is sent to the remote server, and sequence is voided
    procedure BatchAbort;

    /// call this method to add some information to the log at a specified level
    // - the supplied log level will be checked against TSQLRest.LogLevel 
    // - if Instance is set, it will log the corresponding class name and address
    // - will compute the text line in the very same format as TSynLog class
    // - use LogToFile() or LogToRemoteServer() to set the OnLog callback
    procedure Log(Level: TSynLogInfo; const Text: string; Instance: TObject=nil); overload; 
    /// call this method to add some information to the log at a specified level
    // - overloaded method which will call Format() to render the text
    // - here the Fmt layout is e.g. '%s %d %g', as standard Format(), and not
    // the same as with SynCommons' FormatUTF8()
    // - the supplied log level will be checked against TSQLRest.LogLevel 
    // - if Instance is set, it will log the corresponding class name and address
    // - use LogToFile() or LogToRemoteServer() to set the OnLog callback
    procedure Log(Level: TSynLogInfo; const Fmt: string; const Args: array of const;
      Instance: TObject=nil); overload;
    /// call this method to add some information to the log at a specified level
    // - overloaded method which will log the corresponding class name and address
    // - the supplied log level will be checked against TSQLRest.LogLevel
    // - use LogToFile() or LogToRemoteServer() to set the OnLog callback
    procedure Log(Level: TSynLogInfo; Instance: TObject); overload;
    /// call this method to add some information to the log for an Exception
    // - will log the Exception class name and message, if sllExecption is set
    procedure Log(E: Exception); overload;
    {$ifdef ISSMS}
    /// start the logging process into a remote log server
    // - the server could be for instance a LogView tool running in server mode
    procedure LogToRemoteServer(LogLevel: TSynLogInfos;
      const aServer: string; aPort: integer=8091; aRoot: string='LogService');
    {$else}
    /// start the logging process into a file
    // - if no directory is specified, will use the current one
    // - if no file name is supplied, will compute a new one with the current
    // time stamp, in the specified directory
    procedure LogToFile(LogLevel: TSynLogInfos;
      const aFolderName: TFileName=''; const aFileName: TFileName='');
    /// start the logging process into a remote log server
    // - the server could be for instance a LogView tool running in server mode
    procedure LogToRemoteServer(LogLevel: TSynLogInfos;
      const aServer: string; aPort: integer=8091; const aRoot: string='LogService');
    {$endif}

    /// the associated data model
    property Model: TSQLModel read fModel;
    /// the set of log events which will be logged by Log() overloaded methods
    // - set to [] by default, meaning that log is disabled
    property LogLevel: TSynLogInfos read fLogLevel write fLogLevel;
    /// the callback to be executed by Log() overloaded methods
    // - if none is set, the instance won't log anything
    property OnLog: TOnSQLRestLog read fOnLog write fOnLog;
    /// the current Date and Time, as retrieved from the server at connection
    property ServerTimeStamp: TTimeLog read GetServerTimeStamp;
    /// internal state counter of the mORMot server at last access time
    // - can be used to check if retrieved data may be out of date
    property InternalState: cardinal read fInternalState;
    /// the access protocol to be used for interface-based services
    // - is set to TSQLRestRoutingREST by default
    // - you can set TSQLRestRoutingJSON_RPC if the server expects this protocol
    property ServicesRouting: TSQLRestRoutingAbstractClass read fServicesRouting;
  end;

  /// REST client access class
  TSQLRestClientURI = class(TSQLRest)
  protected
    fAuthentication: TSQLRestServerAuthentication;
    fOnlyJSONRequests: boolean;
    fRunningClientDriven: TStringList;
    {$ifdef ISSMS}
    fAsynchCount: integer;
    fAsynchPendingText: array of string;
    procedure SetAsynchText(const Text: string);
    procedure CallAsynchText;
    /// connect to the REST server, and retrieve its time stamp offset
    // - under SMS, you SHOULD use this asynchronous method, which won't block
    // the browser, e.g. if the network is offline
    procedure SetAsynch(var Call: TSQLRestURIParams; onSuccess, onError: TSQLRestEvent;
      onBeforeSuccess: TSQLRestEventProcess);
    {$endif}
    function getURI(aTable: TSQLRecordClass): string;
    function getURIID(aTableExistingIndex: integer; aID: TID): string;
    function getURICallBack(const aMethodName: string; aTable: TSQLRecordClass; aID: TID): string;
    function ExecuteAdd(tableIndex: integer; const json: string): TID; override;
    function ExecuteUpdate(tableIndex: integer; ID: TID; const json: string): boolean; override;
    function ExecuteBatchSend(Table: TSQLRecordClass; const Data: string;
      var Results: TIDDynArray): integer; override;
    procedure InternalURI(var Call: TSQLRestURIParams); virtual; abstract;
    procedure InternalStateUpdate(const Call: TSQLRestURIParams);
    procedure CallRemoteServiceInternal(var Call: TSQLRestURIParams;
      aCaller: TServiceClientAbstract; const aMethod, aParams: string);
    procedure InternalServiceCheck(const aMethodName: string;
      const Call: TSQLRestURIParams);
  public
    {$ifndef ISSMS}
    /// initialize the class, and associate it to a specified database Model
    // - if aOwnModel is TRUE, this class destructor will free aModel instance
    constructor Create(aModel: TSQLModel; aOwnModel: boolean=false); override;
    {$endif}
    /// will call SessionClose
    destructor Destroy; override;

    {$ifdef ISSMS}
    /// connect to the REST server, and retrieve its time stamp offset
    // - under SMS, only this asynchronous method is available, which won't
    // block the browser, e.g. if the network is offline
    // - code sample using two lambda functions may be:
    // !  client := TSQLRestClientHTTP.Create(ServerAddress.Text,888,model,false);
    // !  client.Connect(
    // !  lambda
    // !    if client.ServerTimeStamp=0 then
    // !      ShowMessage('Impossible to retrieve server time stamp') else
    // !      writeln('ServerTimeStamp='+IntToStr(client.ServerTimeStamp));
    // !    if not client.SetUser(TSQLRestServerAuthenticationDefault,LogonName.Text,LogonPassWord.Text) then
    // !      ShowMessage('Authentication Error');
    // !    writeln('Safely connected with SessionID='+IntToStr(client.Authentication.SessionID));
    // !    people := TSQLRecordPeople.Create(client,1); // blocking request
    // !    assert(people.ID=1);
    // !    writeln('Disconnect from server');
    // !    client.Free;
    // !  end,
    // !  lambda
    // !    ShowMessage('Impossible to connect to the server');
    // !  end);
    procedure Connect(onSuccess, onError: TSQLRestEvent);
    {$else}
    /// connect to the REST server, and retrieve its time stamp offset
    // - under SMS, you should not use this blocking version, but
    // the overloaded asynchronous method
    function Connect: boolean;
    {$endif ISSMS}
    /// method calling the remote Server via a RESTful command
    // - calls the InternalURI abstract method
    // - this method will sign the url, if authentication is enabled
    procedure URI(var Call: TSQLRestURIParams); virtual;
    /// get a member from its ID using URI()
    function Retrieve(aID: TID; Value: TSQLRecord;
      ForUpdate: boolean=false): boolean; overload; override;
    {$ifndef ISSMS}
    /// get a blob field content from its record ID and supplied blob field name
    // - returns true on success, and the blob binary data, as direclty
    // retrieved from the server via a dedicated HTTP GET request
    function RetrieveBlob(Table: TSQLRecordClass; aID: TID;
      const BlobFieldName: string; out BlobData: TSQLRawBlob): boolean; override;
    {$endif}
    /// execute directly a SQL statement, returning a list of rows or nil
    // - we expect reUrlEncodedSQL to be defined in AllowRemoteExecute on
    // server side, since we will encode the SQL at URL level, so that all
    // HTTP client libraires will accept this layout (e.g. Indy or AJAX)
    function ExecuteList(const SQL: string): TSQLTableJSON; override;
    /// delete a member
    function Delete(Table: TSQLRecordClass; ID: TID): boolean; override;

    /// wrapper to the protected URI method to call a method on the server
    // - perform a ModelRoot/[TableName/[ID/]]MethodName RESTful GET request
    // - if no Table is expected, set aTable=nil (we do not define nil as
    // default parameter, since the SMS compiler is sometimes confused) 
    procedure CallBackGet(const aMethodName: string;
      const aNameValueParameters: array of const; var Call: TSQLRestURIParams;
      aTable: TSQLRecordClass; aID: TID=0);
    /// decode "result":... content as returned by CallBackGet()
    // - if no Table is expected, set aTable=nil (we do not define nil as
    // default parameter, since the SMS compiler is sometimes confused) 
    function CallBackGetResult(const aMethodName: string;
      const aNameValueParameters: array of const;
      aTable: TSQLRecordClass; aID: TID=0): string;
    /// authenticate an User to the current connected Server
    // - using TSQLRestServerAuthenticationDefault or TSQLRestServerAuthenticationNone
    // - will set Authentication property on success
    function SetUser(aAuthenticationClass: TSQLRestServerAuthenticationClass;
      const aUserName, aPassword: string; aHashedPassword: Boolean=False): boolean;
    /// close the session initiated with SetUser()
    // - will reset Authentication property to nil
    procedure SessionClose;

    {$ifdef ISSMS}
    /// asynchronous execution a specified interface-based service method on the server
    // - under SMS, this asynchronous method won't block the browser, e.g. if
    // the network is offline
    // - you should not call it, but directly TServiceClient* methods
    procedure CallRemoteServiceAsynch(aCaller: TServiceClientAbstract;
      const aMethodName: string; aExpectedOutputParamsCount: integer;
      const aInputParams: array of variant; 
      onSuccess: procedure(res: array of Variant); onError: TSQLRestEvent;
      aReturnsCustomAnswer: boolean=false);
    /// synchronous execution a specified interface-based service method on the server
    // - under SMS, this synchronous method would block the browser, e.g. if
    // the network is offline, or the server is late to answer
    // - but synchronous code is somewhat easier to follow than asynchronous
    // - you should not call it, but directly TServiceClient* methods
    function CallRemoteServiceSynch(aCaller: TServiceClientAbstract;
      const aMethodName: string; aExpectedOutputParamsCount: integer;
      const aInputParams: array of variant; aReturnsCustomAnswer: boolean=false): TVariantDynArray;
    {$else}
    /// execute a specified interface-based service method on the server
    // - this blocking method would raise an EServiceException on error
    // - you should not call it, but directly TServiceClient* methods
    procedure CallRemoteService(aCaller: TServiceClientAbstract;
      const aMethodName: string; aExpectedOutputParamsCount: integer;
      const aInputParams: array of variant; out res: TVariantDynArray;
      aReturnsCustomAnswer: boolean=false);
    {$endif ISSMS}
    /// set this property to TRUE if the server expects only APPLICATION/JSON
    // - applies only for AJAX clients (i.e. SmartMobileStudio platform)
    // - true will let any remote call be identified as "preflighted requests",
    // so will send an OPTIONS method prior to any request: may be twice slower
    // - the default is false, as in TSQLHttpServer.OnlyJSONRequests
    property OnlyJSONRequests: boolean read fOnlyJSONRequests write fOnlyJSONRequests;
    /// if not nil, point to the current authentication session running
    property Authentication: TSQLRestServerAuthentication read fAuthentication;
  end;

  {$ifndef ISSMS}
  TSQLRestClientHTTP = class;
  
  /// thread used to asynchronously log to a remote client
  TSQLRestLogClientThread = class(TThread)
  protected
    fOwner: TSQLRest;
    fOnLog: TOnSQLRestLog;
    fClient: TSQLRestClientHTTP;
    fLock: TMutex;
    fPending: string;
    procedure Execute; override;
  public
    /// initialize the thread
    constructor Create(Owner: TSQLRest;
      const aServer: string; aPort: integer; const aRoot: string);
    /// log one line of text
    procedure LogToRemoteServerText(const Text: string);
    /// finalize the thread
    destructor Destroy; override;
  end;
  {$endif ISSMS}

  /// abstract class used for client authentication
  TSQLRestServerAuthentication = class
  protected
    fUser: TSQLAuthUser;
    fSessionID: cardinal;
    fSessionIDHexa8: string;
    procedure SetSessionID(Value: Cardinal);
    // override this method to return the session key
    function ClientComputeSessionKey(Sender: TSQLRestClientURI): string;
      virtual; abstract;
    function ClientSessionComputeSignature(Sender: TSQLRestClientURI;
      const url: string): string; virtual; abstract;
  public
    /// initialize client authentication instance, i.e. the User associated instance
    constructor Create(const aUserName, aPassword: string;
      aHashedPassword: Boolean=false);
    /// finalize the instance
    destructor Destroy; override;
    /// read-only access to the logged user information
    // - only LogonName and PasswordHashHexa are set here
    property User: TSQLAuthUser read fUser;
    /// contains the session ID used for the authentication
    property SessionID: cardinal read fSessionID;
  end;

  /// mORMot secure RESTful authentication scheme
  TSQLRestServerAuthenticationDefault = class(TSQLRestServerAuthentication)
  protected
    fSessionPrivateKey: hash32;
    function ClientComputeSessionKey(Sender: TSQLRestClientURI): string; override;
    function ClientSessionComputeSignature(Sender: TSQLRestClientURI;
      const url: string): string; override;
  end;

  /// mORMot weak RESTful authentication scheme
  TSQLRestServerAuthenticationNone = class(TSQLRestServerAuthentication)
  protected
    function ClientComputeSessionKey(Sender: TSQLRestClientURI): string; override;
    function ClientSessionComputeSignature(Sender: TSQLRestClientURI;
      const url: string): string; override;
  end;

  /// REST client via HTTP
  // - note that this implementation is not thread-safe yet
  TSQLRestClientHTTP = class(TSQLRestClientURI)
  protected
    fConnection: TAbstractHttpConnection;
    fParameters: TSQLRestConnectionParams;
    fKeepAlive: Integer;
    fCustomHttpHeader: RawUTF8; // e.g. for SetHttpBasicAuthHeaders()
    fForceTerminate: Boolean;
    procedure InternalURI(var Call: TSQLRestURIParams); override;
  public
    /// access to a mORMot server via HTTP
    constructor Create(const aServer: string; aPort: integer; aModel: TSQLModel;
      aOwnModel: boolean=false; aHttps: boolean=false
    {$ifndef ISSMS}; const aProxyName: string='';
      const aProxyByPass: string=''; aSendTimeout: Cardinal=30000;
      aReceiveTimeout: Cardinal=30000; aConnectionTimeOut: cardinal=30000{$endif});
      reintroduce; virtual;
    /// finalize the connection
    destructor Destroy; override;
    /// force the HTTP headers of any request to contain some HTTP BASIC
    // authentication, without creating any remote session
    // - here the password should be given as clear content
    // - potential use case is to use a mORMot client through a HTTPS proxy
    // - then you can use SetUser(TSQLRestServerAuthenticationDefault,...) to
    // define any another "mORMot only" authentication
    procedure SetHttpBasicAuthHeaders(const aUserName, aPasswordClear: RawUTF8);

    /// the associated connection, if active
    property Connection: TAbstractHttpConnection read fConnection;
    /// the connection parameters
    property Parameters: TSQLRestConnectionParams read fParameters;
    {$ifndef ISSMS}
    /// the keep-alive timeout, in ms (20000 by default)
    property KeepAlive: Integer read fKeepAlive write fKeepAlive;
    {$endif ISSMS}
  end;

const
  /// \uFFF1 special code to mark ISO-8601 SQLDATE in JSON
  // - e.g. '"\uFFF12012-05-04"' pattern
  // - Unicode special char U+FFF1 is UTF-8 encoded as EF BF B1 bytes
  // - as generated by DateTimeToSQL/TimeLogToSQL functions, and expected by
  // our mORMot server
  // - should be used with BoundsSQLWhere parameters, e.g. with FormatBind()
  {$ifdef UNICODE}
  JSON_SQLDATE_MAGIC = #$fff1;
  {$else}
  {$ifdef ISSMS}
  JSON_SQLDATE_MAGIC = #$fff1;
  {$else}
  JSON_SQLDATE_MAGIC = #$ef#$bf#$b1;
  {$endif}
  {$endif}

 var
  /// can be set to TSQLRest.LogLevel in order to log all available events
  LOG_VERBOSE: TSynLogInfos;

  /// contains the logging levels for which stack trace should be dumped
  // - which are mainly exceptions or application errors
  LOG_STACKTRACE: TSynLogInfos;

  /// the text equivalency of each logging level, as written in the log content
  // - and expected by TSynLog and our LogView tool
  LOG_LEVEL_TEXT: array[TSynLogInfo] of string = (
    '       ', ' info  ', ' debug ', ' trace ', ' warn  ', ' ERROR ',
    '  +    ', '  -    ',
    ' OSERR ', ' EXC   ', ' EXCOS ', ' mem   ', ' stack ', ' fail  ',
    ' SQL   ', ' cache ', ' res   ', ' DB    ', ' http  ', ' clnt  ', ' srvr  ',
    ' call  ', ' ret   ', ' auth  ',
    ' cust1 ', ' cust2 ', ' cust3 ', ' cust4 ', ' rotat ', ' dddER ', ' dddIN ');


/// true if PropName is either 'ID' or 'RowID'
function IsRowID(const PropName: string): boolean;
  {$ifndef FPC}{$ifdef HASINLINE}inline;{$endif}{$endif}

/// can be used to create a statement with inlined parameters 
// - use DateTimeToSQL() for date/time database fields
function FormatBind(const SQLWhere: string;
  const BoundsSQLWhere: array of const): string;

/// compute a TTimeLog value from Delphi date/time type
function DateTimeToTTimeLog(Value: TDateTime): TTimeLog;

/// convert a TTimeLog value into the Delphi date/time type
function TTimeLogToDateTime(Value: TTimeLog): TDateTime;

/// convert a TTimeLog value into an ISO-8601 encoded date/time text
function TTimeLogToIso8601(Value: TTimeLog): string;

/// returns a string with 2 digits
// - the supplied value should be in 0..99 range
function ToDigit2(value: integer): string;

/// returns a string with 4 digits
// - the supplied value should be in 0..9999 range
function ToDigit4(value: integer): string;

/// convert a date/time to a ISO-8601 string format for SQL '?' inlined parameters
// - if DT=0, returns ''
// - if DT contains only a date, returns the date encoded as '\uFFF1YYYY-MM-DD'
// - if DT contains only a time, returns the time encoded as '\uFFF1Thh:mm:ss'
// - otherwise, returns the ISO-8601 date and time encoded as '\uFFF1YYYY-MM-DDThh:mm:ss'
// - to be used e.g. as in:
// ! aRec.CreateAndFillPrepare(Client,'Datum<=?',[DateTimeToSQL(Now)]);
// - see TimeLogToSQL() if you are using TTimeLog/TModTime/TCreateTime values
function DateTimeToSQL(DT: TDateTime): string;

/// convert a TTimeLog value into a ISO-8601 string format for SQL '?' inlined
// parameters
// - follows the same pattern as DateToSQL or DateTimeToSQL functions, i.e.
// will return the date or time encoded as '\uFFF1YYYY-MM-DDThh:mm:ss'
function TimeLogToSQL(const TimeStamp: TTimeLog): string;

/// convert a base-64 encoded blob into its binary representation
function VariantToBlob(const Value: variant): TSQLRawBlob;

/// convert a binary blob into its base-64 representation
function BlobToVariant(const Blob: TSQLRawBlob): variant;

/// convert a string value into a TGUID instance
function VariantToGUID(const value: variant): TGUID;

/// convert a TGUID instance into a string value
function GUIDToVariant(const GUID: TGUID): variant;

/// convert a variant value into a THttpBody binary
// - will use a variant of type string as mean of proprietary raw binary storage:
// format is limited to HttpBodyToVariant() conversion
function VariantToHttpBody(const value: variant): THttpBody;

/// convert a THttpBody binary content into a variant value
// - will use a variant of type string as mean of proprietary raw binary storage:
// you need to use VariantToHttpBody() to get the value back from the variant
function HttpBodyToVariant(const HttpBody: THttpBody): variant;

/// convert a text or integer enumeration representation into its ordinal value
function VariantToEnum(const Value: variant; const TextValues: array of string): integer;

/// convert any TSQLRecord class instance into a TJSONVariant type
function ObjectToVariant(value: TSQLRecord): variant;

/// hash the supplied text values after UTF-8 encoding
// - as expected by the framework server
function SHA256Compute(const Values: array of string): string;

/// encode a text as defined by RFC 3986
function UrlEncode(const aValue: string): string; overload;

/// encode name=value pairs as defined by RFC 3986
function UrlEncode(const aNameValueParameters: array of const): string; overload;

/// decode a text as defined by RFC 3986
function UrlDecode(const aValue: string): string;

/// retrieve one header from a low-level HTTP response
// - use e.g. location := GetOutHeader(Call,'location');
function GetOutHeader(const Call: TSQLRestURIParams; const Name: string): string;

const
  /// the first field in TSQLFieldBits is always ID/RowID
  ID_SQLFIELD: TSQLFieldBit = TSQLFieldBit(0);
  
var
  /// contains no field bit set
  NO_SQLFIELDBITS: TSQLFieldBits;


implementation

{$ifdef ISDWS}
function VarIsValidRef(const aRef: Variant): Boolean;
begin
  asm
    @Result = !((@aRef == null) || (@aRef == undefined));
  end;
end;
{$endif}
function IsRowID(const PropName: string): boolean;
begin
  result := IdemPropName(PropName,'ID') or
            IdemPropName(PropName,'RowID');
end;

function FormatBind(const SQLWhere: string;
  const BoundsSQLWhere: array of const): string;
var tmpIsString: Boolean;
    tmp: string;
    i,deb,arg,maxArgs,SQLWhereLen: integer;
{$ifdef ISSMS}
    args: variant; // open parameters are not a true array in JavaScript
begin
  asm
    @args=@BoundsSQLWhere;
  end;
  maxArgs := args.length-1;
{$else}
begin
  maxArgs := high(BoundsSQLWhere);
{$endif}
  result := '';
  arg := 0;
  deb := 1;
  i := 1; // we need i after then main loop -> do not use for i := 1 to ...
  SQLWhereLen := length(SQLWhere);
  while i<=SQLWhereLen do
    if SQLWhere[i]='?' then begin
      result := result+copy(SQLWhere,deb,i-deb)+':(';
      if arg>maxArgs then
        tmp := 'null' else begin
        tmp := VarRecToValue(
          {$ifdef ISSMS}args{$else}BoundsSQLWhere{$endif}[arg],tmpIsString);
        if tmpIsString then
          DoubleQuoteStr(tmp);
        inc(arg);
      end;
      result := result+tmp+'):';
      inc(i);
      deb := i;
    end else
      inc(i);
  result := result+copy(SQLWhere,deb,i-deb);
end;

function DateTimeToTTimeLog(Value: TDateTime): TTimeLog;
var HH,MM,SS,MS,Y,M,D: word;
    {$ifndef ISSMS}
    V: Int64;
    {$endif}
begin
  DecodeTime(Value,HH,MM,SS,MS);
  DecodeDate(Value,Y,M,D);
  {$ifdef ISSMS} // JavaScript truncates to 32 bit binary
  result := SS+MM*$40+(HH+D*$20+M*$400+Y*$4000-$420)*$1000;
  {$else}
  V := HH+D shl 5+M shl 10+Y shl 14-(1 shl 5+1 shl 10);
  result := SS+MM shl 6+V shl 12;
  {$endif}
end;

function TTimeLogToDateTime(Value: TTimeLog): TDateTime;
var Y: cardinal;
    Time: TDateTime;
begin
  {$ifdef ISSMS} // JavaScript truncates to 32 bit binary
  Y := (Value div $4000000) and 4095;
  {$else}
  Y := (Value shr (6+6+5+5+4)) and 4095;
  {$endif}
  if (Y=0) or not TryEncodeDate(Y,1+(Value shr (6+6+5+5)) and 15,
       1+(Value shr (6+6+5)) and 31{$ifdef ISSMS},DateTimeZone.UTC{$endif},result) then
    result := 0;
  if (Value and (1 shl (6+6+5)-1)<>0) and
     TryEncodeTime((Value shr (6+6)) and 31,
       (Value shr 6) and 63,Value and 63, 0, Time) then
    result := result+Time;
end;

function TTimeLogToIso8601(Value: TTimeLog): string;
begin
  result := DateTimeToIso8601(TTimeLogToDateTime(Value));
end;

function DateTimeToSQL(DT: TDateTime): string;
begin
  result := JSON_SQLDATE_MAGIC+DateTimeToIso8601(DT);
end;

function TimeLogToSQL(const TimeStamp: TTimeLog): string;
begin
  result := JSON_SQLDATE_MAGIC+TTimeLogToIso8601(TimeStamp);
end;

function ToDigit2(value: integer): string;
begin
  if value<=0 then
    result := '00' else
  if value>99 then
    result := '99' else
    result := chr(48+value div 10)+chr(48+value mod 10);
end;

function ToDigit4(value: integer): string;
begin
  if value<=0 then
    result := '0000' else
  if value>9999 then
    result := '9999' else
    result := ToDigit2(value div 100)+ToDigit2(value mod 100);
end;

function UrlEncode(const aValue: string): string; overload;
{$ifdef ISSMS} inline;
begin // see http://www.w3schools.com/jsref/jsref_encodeuricomponent.asp
  result := encodeURIComponent(aValue);
end;
{$else}
const
  HexChars: array[0..15] of string = (
    '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
var i,c: integer;
    utf8: TUTF8Buffer;
begin
  result := '';
  {$ifdef NEXTGEN}
  utf8 := TEncoding.UTF8.GetBytes(aValue);
  for i := 0 to high(utf8) do begin
  {$else}
  utf8 := UTF8Encode(aValue);
  for i := 1 to length(utf8) do begin
  {$endif}
    c := ord(utf8[i]);
    case c of
    ord('0')..ord('9'),ord('a')..ord('z'),ord('A')..ord('Z'),
    ord('_'),ord('-'),ord('.'),ord('~'):
              AppendChar(result,char(c));
    ord(' '): AppendChar(result,'+');
    else result := result+'%'+HexChars[c shr 4]+HexChars[c and $F];
    end; // see rfc3986 2.3. Unreserved Characters
  end;
end;
{$endif}

function UrlEncode(const aNameValueParameters: array of const): string; overload;
var n,a: integer;
    name,value: string;
    {$ifdef ISSMS}
    temp: variant;
    {$else}
    wasString: Boolean;
    i: integer;
    {$endif}
begin
  result := '';
{$ifdef ISSMS} // open parameters are not a true array in JavaScript
  asm
    @temp=@aNameValueParameters;
  end;
  n := temp.length;
  if n>1 then begin
    for a := 0 to (n-1)shr 1 do begin
      name := temp[a*2];
      value := temp[a*2+1];
{$else}
  n := high(aNameValueParameters);
  if n>0 then begin
    for a := 0 to n div 2 do begin
      name := VarRecToValue(aNameValueParameters[a*2],wasString);
      for i := 1 to length(name) do
        if not (ord(name[i]) in [ord('a')..ord('z'),ord('A')..ord('Z')]) then
          raise ERestException.CreateFmt(
            'UrlEncode() expect alphabetic names, not "%s"',[name]);
      value := VarRecToValue(aNameValueParameters[a*2+1],wasString);
{$endif}
      result := result+'&'+name+'='+UrlEncode(value);
    end;
  end;
  if result<>'' then
    result[1] := '?';
end;

function HexDecode(a,b: char): integer;
var ai,bi: integer;
begin
  ai := Pos(upcase(a),'0123456789ABCDEF')-1;
  bi := Pos(upcase(b),'0123456789ABCDEF')-1;
  if (ai<0) or (bi<0) then
    result := ord('?') else
    result := ai shl 4+bi;
end;

function UrlDecode(const aValue: string): string;
{$ifdef ISSMS}
begin
  result := decodeURIComponent(aValue);
end;
{$else}
var i,c,n,len: integer;
    utf8: TUTF8Buffer;
begin
  i := 1;
  len := length(aValue);
  n := 0;
  SetLength(utf8,len);
  while i<=length(aValue) do begin
    {$ifndef NEXTGEN} // TUTF8Buffer = UTF8String is [1-based]
    inc(n);
    {$endif}
    c := ord(aValue[i]);
    case c of
    ord('+'):
      utf8[n] := AnsiChar(' ');
    ord('%'): begin
      if i+2<=len then
        utf8[n] := AnsiChar(HexDecode(aValue[i+1],aValue[i+2])) else
        utf8[n] := AnsiChar('?');
      inc(i,2);
    end;
    else if c>127 then
      utf8[n] := AnsiChar('?') else
      utf8[n] := AnsiChar(c);
    end;
    inc(i);
    {$ifdef NEXTGEN} // TUTF8Buffer = TBytes is [0-based]
    inc(n);
    {$endif}
  end;
  SetLength(utf8,n);
  {$ifdef NEXTGEN}
  result := TEncoding.UTF8.GetString(utf8);
  {$else}
  {$ifdef UNICODE}
  result := UTF8ToString(utf8);
  {$else}
  result := Utf8Decode(utf8);
  {$endif}
  {$endif}
end;
{$endif ISSMS}


{ TSQLRecord }

{$ifdef ISSMS}

constructor TRTTIPropInfos.Create(const PropNames: array of string;
  const PropKinds: array of TSQLFieldKind);
var name: string;
    p: integer;
    prop: TSQLModelInfoPropInfo;
begin
  prop := new TSQLModelInfoPropInfo;
  prop.Name := 'RowID'; // first Field is RowID
  Props.Add(prop);
  for name in PropNames do begin
    prop := new TSQLModelInfoPropInfo;
    prop.Name := name;
    Props.Add(prop);
  end;
  PropCache := new JObject;
  for p := 0 to high(Props) do begin
    prop := Props[p];
    prop.FieldIndex := TSQLFieldBit(p);
    if (p>0) and (p<=length(PropKinds)) then
      prop.Kind := PropKinds[p-1] else
      prop.Kind := sftUnspecified;
    PropCache[uppercase(prop.Name)] := prop;
  end;
end;

function Find(PropCache: variant; Name: string; var Info: TSQLModelInfoPropInfo): boolean; inline;
begin
  Name := UpperCase(Name);
  if Name='ID' then
    Name := 'ROWID';
  var nfo: TSQLModelInfoPropInfo;
  asm
    @nfo=@PropCache[@Name];
  end;
  result := VarIsValidRef(nfo);
  Info := nfo;
end;

var
  RTTI_Cache: variant = new JObject;

{$HINTS OFF}
class function TSQLRecord.GetRTTI: TRTTIPropInfos;
begin // use RTTI_Cache as global dictionary of all TSQLRecord's RTTI
  var res = RTTI_Cache[ClassName];
  if VarIsValidRef(res) then asm
    @result=@res;
  end else begin
    result := ComputeRTTI;
    RTTI_Cache[ClassName] := result;
  end;
end;
{$HINTS ON}

class function TSQLRecord.ComputeRTTI: TRTTIPropInfos;
begin
  result := TRTTIPropInfos.Create([],[]);
end;

procedure TSQLRecord.SetProperty(FieldIndex: integer; const Value: variant);
begin
  case FieldIndex of
  0: fID := Value;
  end;
end;

function TSQLRecord.GetProperty(FieldIndex: integer): variant;
begin
  case FieldIndex of
  0: result := fID;
  end;
end;

{$endif ISSMS}

constructor TSQLRecord.Create;
begin
  // do nothing by now: inherited classes may set some properties
end;

constructor TSQLRecord.Create(aClient: TSQLRest; aID: TID;
  ForUpdate: boolean=false);
begin
  Create;
  if aClient<>nil then
    aClient.Retrieve(aID,self,ForUpdate);
end;

constructor TSQLRecord.Create(aClient: TSQLRest;
  const FieldNames, SQLWhere: string; const BoundsSQLWhere: array of const);
begin
  Create;
  if aClient<>nil then
    aClient.Retrieve(FieldNames,SQLWhere,BoundsSQLWhere,self);
end;

constructor TSQLRecord.CreateAndFillPrepare(aClient: TSQLRest;
  const FieldNames, SQLWhere: string;
  const BoundsSQLWhere: array of const);
begin
  Create;
  fFill := aClient.MultiFieldValues(RecordClass,FieldNames,SQLWhere,BoundsSQLWhere);
end;

destructor TSQLRecord.Destroy;
begin
  fFill.Free; // may help even with SMS (marking objects as  Garbage Collect)
  inherited;
end;

function TSQLRecord.RecordClass: TSQLRecordClass;
begin
  if self=nil then
    result := nil else
    result := TSQLRecordClass(ClassType);
end;

function TSQLRecord.FillOne: boolean;
begin
  if (self=nil) or (fFill=nil) then
    result := false else
    result := fFill.FillOne(self);
end;

function TSQLRecord.FillRewind: boolean;
begin
  if (self=nil) or (fFill=nil) then
    result := false else
    result := fFill.FillOne(self,true);
end;

{$ifdef ISSMS}

function TSQLRecord.FromNamesValues(const Names: TStrArray;
  const Values: TVariantDynArray; ValuesStartIndex: integer): boolean;
var i: integer;
    info: TSQLModelInfoPropInfo;
    rtti: TRTTIPropInfos;
begin
  result := false;
  if ValuesStartIndex+length(Names)>length(Values) then
    exit;
  rtti := GetRTTI;
  for i := 0 to high(Names) do
    if Find(rtti.PropCache,Names[i],info) then
      SetProperty(info.FieldIndex,Values[i+ValuesStartIndex]) else
      exit;
  result := true;
end;

{$endif}

function TSQLRecord.FromJSON(const aJSON: string): boolean;
var doc: TJSONVariantData;
    table: TSQLTableJSON;
    {$ifndef ISSMS}
    i: Integer;
    {$endif}
begin
  if (self=nil) or (aJSON='') then
    result := false else
  if StartWithPropName(aJSON,'{"fieldCount":') then begin
    table := TSQLTableJSON.Create(aJSON); // non expanded format
    try
      result := table.FillOne(self);
    finally
      table.Free;
    end;
  end else begin // expanded format
    {$ifdef ISSMS}
    doc := TJSONVariantData.Create(aJSON);
    result := FromNamesValues(doc.Names,doc.Values,0);
    {$else}
    doc.Init(aJSON);
    for i := 0 to doc.Count-1 do
      if IsRowID(doc.Names[i]) then
        doc.Names[i] := 'ID';
    result := doc.ToObject(self);
    {$endif}
  end;
end;

constructor TSQLRecord.CreateFromVariant(const aValue: variant);
begin
  Create;
  FromVariant(aValue);
end;

function TSQLRecord.FromVariant(const aValue: variant): boolean;
begin
  result := FromJSON(ValueToJSON(aValue));
end;

function TSQLRecord.ToJSON(aModel: TSQLModel; aFieldNames: string=''): String;
var nfo: TSQLModelInfo;
begin
  if self=nil then
    result := 'null' else begin
    nfo := aModel.InfoExisting(RecordClass);
    result := nfo.ToJSON(self,nfo.FieldNamesToFieldBits(aFieldNames,false));
  end;
end;

function TSQLRecord.ToVariant: variant;
begin
  if self=nil then
    result := null else begin
    {$ifdef ISSMS}
    result := new JObject;
    var rtti := GetRTTI;
    for var f := 0 to high(rtti.Props) do
      result[rtti.Props[f].Name] := GetProperty(f);
    {$else}
    result := JSONVariant(ObjectToJSON(self));
    {$endif}
  end;
end;


{ TSQLTableJSON }

{$ifdef ISDWS} // circumvent weird DWS / SMS syntax

constructor TSQLTableJSON.Create(const aJSON: string);
begin
  var dat = JSON.Parse(aJSON);
  case VariantType(dat) of
  jvObject: begin
    // non expanded format: {"fieldCount":2,"values":["ID","Int",1,0,2,0,3,...]
    fFieldCount := dat.fieldCount;
    var values := dat.values;
    if VariantType(values)<>jvArray then
      exit;
    asm
      @fValues=@values;
    end;
    var n = fValues.Count;
    if (n<fFieldCount) or (n mod fFieldCount<>0) then
      exit;
    for var i := 0 to fFieldCount-1 do
      fFieldNames.Add(string(fValues[i]));
    fRowCount := (n div fFieldCount)-1;
  end;
  jvArray: begin
    // expanded format: [{"ID":1,"Int":0},{"ID":2,"Int":0},{"ID":3,...]
    asm
      @fValues=@dat;
    end;
    fRowCount := fValues.Count;
  end;
  end;
  if fRowCount>0 then
    fCurrentRow := 1;
end;

function TSQLTableJSON.FillOne(Value: TSQLRecord; SeekFirst: boolean=false): boolean;
begin
  result := false;
  if (Value=nil) or (fRowCount=0) then
    exit;
  if SeekFirst then
    fCurrentRow := 1 else
    if fCurrentRow>fRowCount then
      exit;
  if fFieldNames.Count>0 then begin
    // non expanded format
    result := Value.FromNamesValues(fFieldNames,fValues,fCurrentRow*fFieldCount);
  end else begin
    // expanded format
    var doc := TJSONVariantData.CreateFrom(fValues[fCurrentRow-1]);
    result := Value.FromNamesValues(doc.Names,doc.Values,0);
  end;
  inc(fCurrentRow);
  if result then
    Value.fInternalState := fInternalState;
end;

{$else}

function TSQLTableJSON.FillOne(aValue: TSQLRecord; aSeekFirst: boolean): boolean;
begin
  result := StepObject(aValue,aSeekFirst);
  if result then
    aValue.fInternalState := fInternalState;
end;

function TSQLTableJSON.GetPropInfo(aTypeInfo: TRTTITypeInfo;
  const PropName: string): TRTTIPropInfo;
begin
  result := inherited GetPropInfo(aTypeInfo,PropName);
  if (result=nil) and IdemPropName(PropName,'RowID') then
    result := inherited GetPropInfo(aTypeInfo,'ID');
end;


{ TSQLModelInfoPropInfo }

constructor TSQLModelInfoPropInfo.CreateFrom(aRTTI: TRTTIPropInfo);
begin
  RTTI := aRTTI;
  TypeName := RTTIPropInfoTypeName(RTTI);
  case RTTI^.PropType^.Kind of
    tkRecord:  Kind := sftRecord;
    tkVariant: Kind := sftVariant;
  else
    if TypeName='TByteDynArray' then
      Kind := sftBlob else
    if TypeName='TDateTime' then
      Kind := sftDateTime else
    if TypeName='TCreateTime' then
      Kind := sftCreateTime else
    if TypeName='TModTime' then
      Kind := sftModTime;
  end;
end;

{$endif ISDWS}


{ TSQLModelInfo }

procedure TSQLModelInfo.ComputeFieldsBeforeWrite(aClient: TSQLRest;
  Value: TSQLRecord; AndCreate: Boolean);
var f: TSQLFieldBit;
    fields: TSQLFieldBits;
    TimeStamp: Int64;
begin
  if (Value=nil) or not HasTimeFields then
    exit;
  if AndCreate then
    fields := ModAndCreateTimeFields else
    fields := ModTimeFields;
  TimeStamp := aClient.ServerTimeStamp;
  for f := 0 to length(Prop)-1 do
    if f in fields then
      {$ifdef ISSMS}
      Value.SetProperty(ord(f),TimeStamp);
      {$else}
      SetInstanceProp(Value,Prop[f].RTTI,TimeStamp);
      {$endif}
end;

function GetDisplayNameFromClass(C: TClass): string;
begin
  if C=nil then
    result := '' else begin
    result := C.ClassName;
    if IdemPropName(copy(result,1,4),'TSQL') then
      if IdemPropName(copy(result,5,6),'Record') then
        delete(result,1,10) else
        delete(result,1,4) else
      if result[1]='T' then
        delete(result,1,1);
  end;
end;

constructor TSQLModelInfo.CreateFromRTTI(aTable: TSQLRecordClass);
var f: TSQLFieldBit;
    Kind: TSQLFieldKind;
{$ifdef ISDWS}
    rtti: TRTTIPropInfos;
{$else}
    List: TRTTIPropInfoDynArray;
    Names: TStringDynArray;
{$endif}
begin
  Table := aTable;
  Name := GetDisplayNameFromClass(Table);
  {$ifdef ISDWS}
  rtti := aTable.GetRTTI;
  Prop := rtti.Props;
  PropCache := rtti.PropCache;
  {$else}
  GetPropsInfo(Table.ClassInfo,Names,List);
  SetLength(Prop,length(List));
  for f := 0 to high(List) do begin
    Prop[f] := TSQLModelInfoPropInfo.CreateFrom(List[f]);
    if f=0 then
      Prop[f].Name := 'RowID' else
      Prop[f].Name := Names[f];
  end;
  {$endif}
  for f := 0 to TSQLFieldBit(high(Prop)) do begin
    include(AllFields,f);
    Kind := Prop[ord(f)].Kind;
    include(HasKind,Kind);
    if Kind=sftBlob then
      Include(BlobFields,f) else
      Include(SimpleFields,f);
    case Kind of
    sftModTime: begin
      include(ModTimeFields,f);
      include(ModAndCreateTimeFields,f);
      HasTimeFields := true;
    end;
    sftCreateTime: begin
      include(CreateTimeFields,f);
      include(ModAndCreateTimeFields,f);
      HasTimeFields := true;
    end;
    sftRecord:
      include(RecordFields,f);
    sftVariant:
      include(VariantFields,f);
    end;
  end;
end;

{$ifndef ISSMS}

destructor TSQLModelInfo.Destroy;
var i: integer;
begin
  inherited;
  for i := 0 to Length(Prop)-1 do
    Prop[i].Free;
end;

{$endif}

function TSQLModelInfo.FieldBitsToFieldNames(
  const FieldBits: TSQLFieldBits): string;
var f: TSQLFieldBit;
begin
  result := '';
  for f := 0 to length(Prop)-1 do
  if f in FieldBits then
    result := result+Prop[ord(f)].Name+',';
  if result<>'' then
    SetLength(result,length(result)-1);
end;

function TSQLModelInfo.FieldNamesToFieldBits(const FieldNames: string;
  IncludeModTimeFields: boolean): TSQLFieldBits;
var i: integer;
    f: TSQLFieldBit;
    field: string;
begin
  if FieldNames='' then
    result := SimpleFields else
  if FieldNames='*' then
    result := AllFields else begin
    result := NO_SQLFIELDBITS;
    i := 1;
    while GetNextCSV(FieldNames,i,field,',',true) do begin
      {$ifdef ISSMS}
      var Info: TSQLModelInfoPropInfo;
      if Find(PropCache,field,info) then
        include(result,info.FieldIndex);
      {$else}
      if IsRowID(field) then
        Include(result,ID_SQLFIELD) else
        for f := 1 to length(Prop)-1 do
          if IdemPropName(field,Prop[ord(f)].Name) then begin
            include(result,f);
            break;
          end;
      {$endif}
    end;
    {$ifdef ISSMS}
    if IncludeModTimeFields and (sftModTime in HasKind) then
      for f := 1 to length(Prop)-1 do
        if f in ModTimeFields then
          include(result,f);
    {$else}
    if IncludeModTimeFields then
      result := result+ModTimeFields;
    {$endif}
  end;
end;

function TSQLModelInfo.SQLSelect(const FieldNames: string): string;
begin
  result := 'select '+FieldBitsToFieldNames(FieldNamesToFieldBits(
    FieldNames,false))+' from '+Name;
end;

function TSQLModelInfo.ToJSON(Value: TSQLRecord; const Fields: TSQLFieldBits): string;
var f: TSQLFieldBit;
begin
{$ifdef ISSMS}
  if Value=nil then
    exit('null');
  var doc: variant := new JObject;
  for f := 0 to length(Prop)-1 do
    if f in Fields then
      doc[Prop[ord(f)].Name] := Value.GetProperty(f);
  result := JSON.Stringify(doc); // rely on JavaScript serialization
{$else}
  result := '{';
  for f := 0 to length(Prop)-1 do
    if f in Fields then
      result := result+'"'+Prop[ord(f)].Name+'":'+
        ValueToJSON(GetInstanceProp(Value,Prop[f].RTTI))+',';
  if result='{' then
    result := 'null' else
    result[Length(Result)] := '}';
{$endif}
end;

function TSQLModelInfo.ToJSONAdd(Client: TSQLRest;
  Value: TSQLRecord; ForceID: boolean; const FieldNames: string): string;
var Fields: TSQLFieldBits;
begin
  ComputeFieldsBeforeWrite(Client,Value,true);
  fields := FieldNamesToFieldBits(FieldNames,true);
  if not ForceID then
    exclude(fields,ID_SQLFIELD);
  result := ToJSON(Value,fields);
end;

function TSQLModelInfo.ToJSONUpdate(Client: TSQLRest; Value: TSQLRecord;
  const FieldNames: string; ForceID: boolean): string;
var Fields: TSQLFieldBits;
begin
  fields := FieldNamesToFieldBits(FieldNames,true);
  if ForceID then
    include(fields,ID_SQLFIELD) else
    exclude(fields,ID_SQLFIELD);
  ComputeFieldsBeforeWrite(Client,Value,false);
  result := ToJSON(Value,fields);
end;


{ TSQLModel }

procedure TSQLModel.Add(Table: TSQLRecordClass);
var n,i: integer;
    nfo: TSQLModelInfo;
begin
  n := length(fInfo);
  for i := 0 to n-1 do
    if fInfo[i].Table=Table then
      raise ERESTException.CreateFmt('%s registered twice',[Table.ClassName]);
  nfo := TSQLModelInfo.CreateFromRTTI(Table);
  {$ifdef ISSMS}
  fInfo.Add(nfo);
  {$else}
  SetLength(fInfo,n+1);
  fInfo[n] := nfo;
  {$endif}
end;

constructor TSQLModel.Create(const Tables: array of TSQLRecordClass;
  const aRoot: string);
var t: integer;
begin
  {$ifdef ISSMS}
  for t := 0 to high(Tables) do
    fInfo.Add(TSQLModelInfo.CreateFromRTTI(Tables[t]));
  {$else}
  SetLength(fInfo,length(Tables));
  for t := 0 to high(fInfo) do
    fInfo[t] := TSQLModelInfo.CreateFromRTTI(Tables[t]);
  {$endif}
  if aRoot<>'' then
    if aRoot[length(aRoot)]='/' then
      fRoot := copy(aRoot,1,Length(aRoot)-1) else
      fRoot := aRoot;
end;

function TSQLModel.GetTableIndex(aTable: TSQLRecordClass): integer;
begin
  if self<>nil then
    for result := 0 to High(fInfo) do
      if fInfo[result].Table=aTable then
        exit;
  result := -1;
end;

{$ifndef ISSMS}

destructor TSQLModel.Destroy;
var i: integer;
begin
  inherited;
  for i := 0 to high(fInfo) do
    fInfo[i].Free;
end;

{$endif}

function TSQLModel.InfoExisting(aTable: TSQLRecordClass): TSQLModelInfo;
begin
  result := Info[GetTableIndexExisting(aTable)];
end;

function TSQLModel.GetTableIndex(const aTableName: string): integer;
begin
  if self<>nil then                                
    for result := 0 to High(fInfo) do
      if IdemPropName(fInfo[result].Name,aTableName) then
        exit;
  result := -1;
end;

function TSQLModel.GetTableIndexExisting(aTable: TSQLRecordClass): integer;
begin
  if self=nil then
    result := -1 else
    result := GetTableIndex(aTable);
  if result<0 then
    raise ERestException.CreateFmt('%s should be part of the Model',
      [aTable.ClassName]);
end;

{ TSQLRest }

constructor TSQLRest.Create(aModel: TSQLModel; aOwnModel: boolean);
begin
  inherited Create;
  fModel := aModel;
  fOwnModel := aOwnModel;
  fServicesRouting := TSQLRestRoutingREST;
end;

destructor TSQLRest.Destroy;
begin
  Log(sllInfo,'Destroy',self);
  inherited;
  if fOwnModel then
    fModel.Free;
  LogClose;
end;

function TSQLRest.MultiFieldValues(Table: TSQLRecordClass;
  const FieldNames, SQLWhere: string; const BoundsSQLWhere: array of const;
  LimitFirstRow: Boolean): TSQLTableJSON;
var where: string;
begin
  where := FormatBind(SQLWhere,BoundsSQLWhere);
  if LimitFirstRow then
    where := where+' limit 1';
  result := MultiFieldValues(Table,FieldNames,where);
end;

function TSQLRest.GetServerTimeStamp: TTimeLog;
begin
  if fServerTimeStampOffset=0 then
    result := 0 else
    result := DateTimeToTTimeLog(Now+fServerTimeStampOffset);
end;

function TSQLRest.SetServerTimeStamp(const ServerResponse: string): boolean;
var TimeStamp: Int64;
begin
  if not TryStrToInt64(ServerResponse,TimeStamp) then
    result := false else begin
    fServerTimeStampOffset := TTimeLogToDateTime(TimeStamp)-Now;
    if fServerTimeStampOffset=0 then
      fServerTimeStampOffset := 0.000001; // ensure <> 0 (indicates error)
    result := true;
  end;
end;

function TSQLRest.MultiFieldValues(Table: TSQLRecordClass;
  const FieldNames, SQLWhere: string): TSQLTableJSON;
var sql: string;
begin
  sql := Model.InfoExisting(Table).SQLSelect(FieldNames);
  if SQLWhere<>'' then
    sql := sql+' where '+SQLWhere;
  result := ExecuteList(sql);
end;

function TSQLRest.Retrieve(const FieldNames,SQLWhere: string;
  const BoundsSQLWhere: array of const; Value: TSQLRecord): boolean;
var table: TSQLTableJSON;
begin
  table := MultiFieldValues(Value.RecordClass,FieldNames,
    SQLWhere,BoundsSQLWhere,true);
  if table=nil then
    result := false else
    try
      result := table.FillOne(Value);
    finally
      table.Free;
    end;
end;

function TSQLRest.RetrieveList(Table: TSQLRecordClass; const FieldNames,
  SQLWhere: string; const BoundsSQLWhere: array of const): TObjectList;
var rows: TSQLTableJSON;
    rec: TSQLRecord;
begin
  {$ifndef ISSMS} // result is already created as "array of TObject"
  result := TObjectList.Create;
  {$endif}
  rows := MultiFieldValues(Table,FieldNames,SQLWhere,BoundsSQLWhere);
  if rows<>nil then
    try
      repeat
        rec := Table.Create;
        if not rows.FillOne(rec) then begin
          rec.Free;
          break;
        end;
        result.Add(rec);
      until false;
    finally
      rows.Free;
    end;
end;

{$ifdef ISDELPHI2010}
function TSQLRest.RetrieveList<T>(const FieldNames, SQLWhere: string;
  const BoundsSQLWhere: array of const): TObjectList<T>;
var rows: TSQLTableJSON;
    rec: TSQLRecord;
begin
  result := TObjectList<T>.Create; // TObjectList<T> will free each T instance
  rows := MultiFieldValues(TSQLRecordClass(T),FieldNames,SQLWhere,BoundsSQLWhere);
  if rows<>nil then
    try
      repeat
        rec := TSQLRecordClass(T).Create;
        if not rows.FillOne(rec) then begin
          rec.Free;
          break;
        end;
        result.Add(rec);
      until false;
    finally
      rows.Free;
    end;
end;
{$endif}

function TSQLRest.Add(Value: TSQLRecord; SendData, ForceID: boolean;
  FieldNames: string): TID;
var tableIndex: Integer;
    json: string;
begin
  tableIndex := Model.GetTableIndexExisting(Value.RecordClass);
  if SendData then
    json := Model.Info[tableIndex].ToJSONAdd(self,Value,ForceID,FieldNames);
  result := ExecuteAdd(tableIndex,json);
  if result>0 then
    Value.fInternalState := InternalState;
end;

function TSQLRest.Update(Value: TSQLRecord; FieldNames: string): boolean;
var tableIndex: Integer;
    json: string;
begin
  if (Value=nil) or (Value.ID<=0) then begin
    result := false;
    exit;
  end;
  tableIndex := Model.GetTableIndexExisting(Value.RecordClass);
  json := Model.Info[tableIndex].ToJSONUpdate(self,Value,FieldNames,false);
  result := ExecuteUpdate(tableIndex,Value.ID,json);
  if result then
    Value.fInternalState := InternalState;
end;

function TSQLRest.BatchStart(aTable: TSQLRecordClass;
  AutomaticTransactionPerRow: cardinal; BatchOptions: TSQLRestBatchOptions): boolean;
begin
  if (fBatchCount<>0) or (fBatch<>'') or (AutomaticTransactionPerRow<=0) then begin
    result := false; // already opened BATCH sequence
    exit;
  end;
  if aTable<>nil then // sent as '{"Table":["cmd",values,...]}'
    fBatch := '{"'+Model.InfoExisting(aTable).Name+'":';
  fBatch := Format('%s["automaticTransactionPerRow",%d,"options",%d,',
    [fBatch,AutomaticTransactionPerRow,byte(BatchOptions)]);
  fBatchTable := aTable;
  result := true;
end;

function TSQLRest.InternalBatch(Table: TSQLRecordClass; const CMD: string;
  var Info: TSQLModelInfo): Integer;
begin
  result := -1;
  if (self=nil) or (Table=nil) or (fBatch='') then
    exit; // invalid parameters, or not opened BATCH sequence
  Info := Model.InfoExisting(Table);
  if fBatchTable<>nil then
    if fBatchTable<>Table then
      exit else 
      fBatch := fBatch+CMD+'",' else
      fBatch := fBatch+CMD+'@'+Info.Name+'",';
  result := fBatchCount;
  inc(fBatchCount);
end;

function TSQLRest.BatchAdd(Value: TSQLRecord; SendData: boolean; ForceID: boolean;
  FieldNames: string): integer;
var info: TSQLModelInfo;
begin
  result := InternalBatch(Value.RecordClass,'"POST',info);
  if result>=0 then
    if not SendData then
      fBatch := fBatch+'{},' else
      fBatch := fBatch+info.ToJSONAdd(self,Value,ForceID,FieldNames)+',';
end;

function TSQLRest.BatchUpdate(Value: TSQLRecord; FieldNames: string): integer;
var info: TSQLModelInfo;
begin
  if (Value=nil) or (Value.ID<=0) then
    result := -1 else begin
    result := InternalBatch(Value.RecordClass,'"PUT',info);
    if result>=0 then
      fBatch := fBatch+info.ToJSONUpdate(self,Value,FieldNames,true)+',';
  end;
end;

function TSQLRest.BatchDelete(Table: TSQLRecordClass; ID: TID): integer;
var info: TSQLModelInfo;
begin
  if ID<=0 then
    result := -1 else begin
    result := InternalBatch(Table,'"DELETE',info);
    if result>=0 then
      fBatch := fBatch+IntToStr(ID)+',';
  end;
end;

function TSQLRest.BatchDelete(ID: TID): integer;
begin
  result := BatchDelete(fBatchTable,ID);
end;

function TSQLRest.BatchDelete(Value: TSQLRecord): integer;
begin
  result := BatchDelete(Value.RecordClass,Value.ID);
end;

function TSQLRest.BatchCount: integer;
begin
  if self=nil then
    result := 0 else
    result := fBatchCount;
end;

function TSQLRest.BatchSend(var Results: TIDDynArray): integer;
begin
  if (self=nil) or (fBatch='') then
    result := HTTP_BADREQUEST else
  try
    if BatchCount>0 then begin
      fBatch[length(fBatch)] := ']';
      if fBatchTable<>nil then
        fBatch := fBatch+'}';
      result := ExecuteBatchSend(fBatchTable,fBatch,Results);
    end else
      result := HTTP_SUCCESS; // nothing to send
  finally
    BatchAbort;
  end;
end;

procedure TSQLRest.BatchAbort;
begin
  if self=nil then
    exit;
  fBatchCount := 0;
  fBatchTable := nil;
  fBatch := '';
end;

procedure TSQLRest.Log(Level: TSynLogInfo; const Text: string; Instance: TObject);
procedure DoLog;
var line: string;
    Value: TDateTime;
    HH,MM,SS,MS,Y,M,D: word;
    {$ifndef ISSMS}
    i: integer;
    {$endif}
begin
  // compute the line as expected by TSynLog / LogView
  Value := Now;
  DecodeTime(Value,HH,MM,SS,MS);
  DecodeDate(Value,Y,M,D);
  line := ToDigit4(Y)+ToDigit2(M)+ToDigit2(D)+' '+ToDigit2(HH)+ToDigit2(MM)+
    ToDigit2(SS)+ToDigit2(MS shr 4)+LOG_LEVEL_TEXT[Level];
  if Assigned(Instance) then
    line := line+Instance.ClassName+
    {$ifdef ISSMS}' ';{$else}'('+IntToHex(
    {$ifdef CPU64}Int64(Instance),16{$else}cardinal(Instance),8{$endif})+') ';
    {$endif}
  line := line+Text;
  // ensure no CR/LF in the output row
  {$ifdef ISSMS}
  line := line.Replace(#10,' ').Replace(#13,' ');
  {$else}
  for i := 1 to length(line) do
    if ord(line[i])<32 then
      line[i] := ' ';
  {$endif}
  // line output
  fOnLog(line);
end;
begin
  if Assigned(self) and Assigned(fOnLog) and (Level in fLogLevel) then
    DoLog;
end;

procedure TSQLRest.Log(Level: TSynLogInfo; const Fmt: string; const Args: array of const;
  Instance: TObject);
begin
  if Assigned(self) and Assigned(fOnLog) and (Level in fLogLevel) then
    Log(Level,Format(Fmt,Args),Instance);
end;

procedure TSQLRest.Log(Level: TSynLogInfo; Instance: TObject);
begin
  if Assigned(self) and Assigned(fOnLog) and (Level in fLogLevel) then
    Log(Level,'',Instance);
end;

procedure TSQLRest.Log(E: Exception);
begin
  if Assigned(self) and Assigned(fOnLog) and (sllException in fLogLevel) then begin
   {$ifdef ISSMS}
   var msg: string;
   asm @msg = new Error().stack; end;
   Log(sllException,'%s raised with message "%s" %s',[E.ClassName,E.Message,msg]);
   {$else}
   Log(sllException,'%s raised with message "%s"',[E.ClassName,E.Message]);
   {$endif}
  end;
end;

{$ifdef ISSMS}

procedure TSQLRest.LogToRemoteServer(LogLevel: TSynLogInfos;
  const aServer: string; aPort: integer; aRoot: string);
var Call: TSQLRestURIParams;
    userAgent: string;
begin
  LogClose;
  fLogClient := TSQLRestClientHTTP.Create(aServer,aPort,TSQLModel.Create([],aRoot),true);
  fLogClient.CallBackGet('TimeStamp',[],Call,nil); // synchronous connection
  if Call.OutStatus=HTTP_SUCCESS then begin
    fLogLevel := LogLevel;
    OnLog := LogToRemoteServerText;
    asm @userAgent = navigator.userAgent; end;
    Log(sllClient,'Remote Cross-Platform Client Connected from AJAX app '+userAgent);
  end else
    LogClose;
end;

procedure TSQLRest.LogToRemoteServerText(const Text: string);
begin
  if fLogClient<>nil then
    fLogClient.SetAsynchText(Text);
end;

{$else}

constructor TSQLRestLogClientThread.Create(Owner: TSQLRest; 
  const aServer: string; aPort: integer; const aRoot: string);
begin
  fLock := TMutex.Create;
  fOwner := Owner;
  fClient := TSQLRestClientHTTP.Create(aServer,aPort,
    TSQLModel.Create([],aRoot),true,false,'','',10000,10000,500);
  fOwner.OnLog := {$ifdef FPC}@{$endif}LogToRemoteServerText;
  inherited Create(false);
end;

destructor TSQLRestLogClientThread.Destroy;
begin
  if fOwner.fLogClient=Self then begin
    fOwner.fLogClient := nil;
    fOwner.fOnlog := nil;
  end;
  fClient.fForceTerminate := true;
  inherited Destroy;
  fClient.Free;
  fLock.Free;
end;

procedure TSQLRestLogClientThread.LogToRemoteServerText(const Text: string);
begin
  if self=nil then
    exit; // avoid GPF
  fLock.Enter;
  if fPending='' then
    fPending := Text else
    fPending := fPending+#13#10+Text;
  fLock.Leave;
end;

procedure TSQLRestLogClientThread.Execute;
var exeName, data: string;
    Call: TSQLRestURIParams;
begin
  if not fClient.Connect then
    exit;
  fOwner.OnLog := {$ifdef FPC}@{$endif}LogToRemoteServerText;
  exeName := paramstr(0);
  if exeName='' then
    exeName := 'non Windows platform';
  fOwner.Log(sllClient,'Remote Cross-Platform Client %s Connected from %s',
    [ClassName,exeName]);
  while not Terminated do begin
    sleep(10);
    if Terminated then
      break;
    fLock.Enter;
    data := fPending;
    fPending := '';
    fLock.Leave;
    if data='' then
      continue;
    Call.Init(fClient.getURICallBack('RemoteLog',nil,0),'PUT',data);
    fClient.URI(Call);
  end;
end;

procedure TSQLRest.LogToRemoteServer(LogLevel: TSynLogInfos;
  const aServer: string; aPort: integer; const aRoot: string);
begin
  LogClose;
  fLogLevel := LogLevel;
  fLogClient := TSQLRestLogClientThread.Create(self,aServer,aPort,aRoot);
end;

procedure TSQLRest.LogToFile(LogLevel: TSynLogInfos; const aFolderName,aFileName: TFileName);
var FN: TFileName;
begin
  LogClose;
  if aFolderName<>'' then
    FN := IncludeTrailingPathDelimiter(aFolderName);
  if aFileName<>'' then
    if ExtractFileExt(aFileName)='' then
      FN := FN+aFileName+'.log' else
      FN := FN+aFileName else
    FN := FN+FormatDateTime('yyyymmddhhnnss',Now)+'.log';
  try
    AssignFile(fLogFile,FN);
    SetLength(fLogFileBuffer,4096);
    system.SetTextBuf(fLogFile,fLogFileBuffer[0],4096);
    Rewrite(fLogFile);
    Writeln(fLogFile,paramstr(0),' 0.0.0.0 (',DateTimeToIso8601(Date),')');
    Writeln(fLogFile,'Host=Unknown User=Unknown CPU=Unknown OS=0.0=0.0.0 Wow64=0 Freq=1');
    Writeln(fLogFile,'TSQLRest 1.18 CrossPlatform ',NowToIso8601,#13#10);
    fLogLevel := LogLevel;
    OnLog := {$ifdef FPC}@{$endif}LogToFileText;
  except
    on E: Exception do
      Finalize(fLogFileBuffer);
  end;
end;

procedure TSQLRest.LogToFileText(const Text: string);
begin
  if fLogFileBuffer<>nil then
    writeln(fLogFile,Text);
end;

{$endif ISSMS}

procedure TSQLRest.LogClose;
begin
  fLogLevel := [];
  fOnLog := nil;
  {$ifdef ISSMS}
  if fLogClient<>nil then begin
    fLogClient.CallAsynchText; // send NOW any pending log
    fLogClient.Free;
    fLogClient := nil;
  end;
  {$else}
  if fLogFileBuffer<>nil then
    try
      system.Close(fLogFile);
    finally
      Finalize(fLogFileBuffer);
    end;
  FreeAndNil(fLogClient);
  {$endif}
end;


{ TSQLRestClientURI }

const
  LOGLEVELDB: array[boolean] of TSynLogInfo = (sllError,sllDB);
  
function TSQLRestClientURI.getURI(aTable: TSQLRecordClass): string;
begin
  result := Model.Root;
  if (aTable<>nil) and (aTable<>TSQLRecord) then // SMS converts nil->TSQLRecord
    result := result+'/'+Model.InfoExisting(aTable).Name;
end;

function TSQLRestClientURI.getURICallBack(const aMethodName: string;
  aTable: TSQLRecordClass; aID: TID): string;
begin
  result := getURI(aTable);
  if aID>0 then
    result := result+'/'+IntToStr(aID);
  result := result+'/'+aMethodName;
end;

function TSQLRestClientURI.getURIID(aTableExistingIndex: integer; aID: TID): string;
begin
  result := Model.Root+'/'+Model.Info[aTableExistingIndex].Name;
  if aID>0 then
    result := result+'/'+IntToStr(aID);
end;

function TSQLRestClientURI.ExecuteList(const SQL: string): TSQLTableJSON;
var Call: TSQLRestURIParams;
    json: string;
begin
  result := nil;
  if self=nil then
    exit;
  Log(sllSQL,SQL);
  // strict HTTP does not allow any body content -> encode SQL at URL
  // so we expect reUrlEncodedSQL to be defined in AllowRemoteExecute
  Call.Init(Model.Root+UrlEncode(['sql',sql]),'GET','');
  URI(Call);
  if Call.OutStatus=HTTP_SUCCESS then begin
    json := Call.OutBodyUtf8;
    result := TSQLTableJSON.Create(json);
    result.fInternalState := fInternalState;
  end else
    Log(sllError,'ExecuteList failed');
end;

function TSQLRestClientURI.Retrieve(aID: TID; Value: TSQLRecord;
  ForUpdate: boolean): boolean;
var tableIndex: Integer;
    Call: TSQLRestURIParams;
    json: string;
begin
  tableIndex := Model.GetTableIndexExisting(Value.RecordClass);
  Call.Url := getURIID(tableIndex,aID);
  if ForUpdate then
     Call.Verb := 'LOCK' else
     Call.Verb := 'GET';
  URI(Call);
  result := Call.OutStatus=HTTP_SUCCESS;
  if result then begin
    json := Call.OutBodyUtf8;
    Value.FromJSON(json);
    Value.fInternalState := fInternalState;
  end;
  Log(LOGLEVELDB[result],'%s.Retrieve(ID=%d) %s',[Model.Info[tableIndex].Name,aID,json]);
end;

{$ifndef ISSMS}
function TSQLRestClientURI.RetrieveBlob(Table: TSQLRecordClass; aID: TID;
  const BlobFieldName: string; out BlobData: TSQLRawBlob): boolean;
var tableIndex: Integer;
    Call: TSQLRestURIParams;
begin
  tableIndex := Model.GetTableIndexExisting(Table);
  Call.Init(getURIID(tableIndex,aID)+'/'+BlobFieldName,'GET','');
  URI(Call);
  result := Call.OutStatus=HTTP_SUCCESS;
  if result then
    BlobData := TSQLRawBlob(Call.OutBody);
  Log(LOGLEVELDB[result],'%s.RetrieveBlob(ID=%d,"%s") len=%d',
    [Model.Info[tableIndex].Name,aID,BlobFieldName,length(BlobData)]);
end;
{$endif}

function FindHeader(const Headers, Name: string): string;
{$ifdef ISSMS} // dedicated function using faster JavaScript library
var search,nameValue: string;
    searchLen: integer;
begin
  if Headers='' then
    exit '';
  search := UpperCase(Name);
  searchLen := Length(search);
  for nameValue in Headers.Split(#13#10) do
    if uppercase(copy(nameValue,1,searchLen))=search then
      exit copy(nameValue,searchLen+1,length(nameValue));
end;
{$else}
var i: integer;
    line: string;
begin
  result := '';
  i := 1;
  while GetNextCSV(Headers,i,line,#10) do
    if StartWithPropName(line,Name) then begin
      delete(line,1,length(Name));
      result := trim(line); // will work if EOL is CRLF or LF only
      exit;
    end;
end;                    
{$endif}

function GetOutHeader(const Call: TSQLRestURIParams; const Name: string): string;
begin
{$ifdef ISSMS_XHRISBUGGY} // retrieval from Call.XHR is buggy on some browers :(
  // see https://synopse.info/forum/viewtopic.php?pid=11730#p11730
  if VarIsValidRef(Call.XHR) then
    result := Call.XHR.getResponseHeader(Name);
{$else}
  result := FindHeader(Call.OutHead,Name+': ');
{$endif}
end;

procedure TSQLRestClientURI.InternalStateUpdate(const Call: TSQLRestURIParams);
var receivedState: cardinal;
begin
  if Call.OutHead='' then
    exit; // nothing to update from (e.g. asynchronous call)
  receivedState := StrToIntDef(GetOutHeader(Call,'Server-InternalState'),0);
  if receivedState>fInternalState then
    fInternalState := receivedState;
  if sllTrace in fLogLevel then
    Log(sllTrace,'%s %s status=%d state=%d in=%d out=%d',
      [Call.Verb,Call.UrlWithoutSignature,Call.OutStatus,fInternalState,
       length(Call.InBody),length(Call.OutBody)]);
end;

procedure TSQLRestClientURI.URI(var Call: TSQLRestURIParams);
var sign: string;
begin
  Call.OutStatus := HTTP_UNAVAILABLE;
  if self=nil then
    exit;
  Call.UrlWithoutSignature := Call.Url;
  if (fAuthentication<>nil) and (fAuthentication.SessionID<>0) then begin
    if Pos('?',Call.Url)=0 then
      sign := '?session_signature=' else
      sign := '&session_signature=';
    Call.Url := Call.Url+sign+
      fAuthentication.ClientSessionComputeSignature(self,Call.Url);
  end;
  InternalURI(Call);
  InternalStateUpdate(Call);
end;

procedure TSQLRestClientURI.InternalServiceCheck(const aMethodName: string;
  const Call: TSQLRestURIParams);
begin
  {$ifdef ISSMS}
  if Assigned(Call.OnSuccess) then
    exit; // asynchronous call do not have a result yet
  {$endif}
  if Call.OutStatus<>HTTP_SUCCESS then
    Log(sllError,'Service %s returned %s',[aMethodName,Call.OutBodyUtf8]) else
    Log(sllServiceReturn,'%s success',[aMethodName]);
end;

procedure TSQLRestClientURI.CallBackGet(const aMethodName: string;
  const aNameValueParameters: array of const; var Call: TSQLRestURIParams;
  aTable: TSQLRecordClass; aID: TID);
begin
  Log(sllServiceCall,'Method-based service %s',[aMethodName]);
  Call.Url := getURICallBack(aMethodName,aTable,aID)+UrlEncode(aNameValueParameters);
  Call.Verb := 'GET';
  URI(Call);
  InternalServiceCheck(aMethodName,Call);
end;

function TSQLRestClientURI.ExecuteBatchSend(Table: TSQLRecordClass; const Data: string;
  var Results: TIDDynArray): integer;
var {$ifdef ISSMS}
    doc: variant;
    {$else}
    doc: TJSONVariantData;
    jsonres: string;
    {$endif}
    Call: TSQLRestURIParams;
    start: TDateTime;
    i: integer;
begin
  start := Now;
  Log(sllServiceCall,'BATCH with %d rows',[fBatchCount]);
  Call.Init(getURICallBack('Batch',Table,0),'POST',Data);
  URI(Call);
  result := Call.OutStatus;
  if result<>HTTP_SUCCESS then begin
    Log(sllError,'BATCH error');
    exit; // transmission or internal server error
  end;
  Log(sllServiceReturn,'BATCH success in %s',[FormatDateTime('nn:ss:zzz',Now-start)]);
  {$ifdef ISSMS}
  Results.Clear;
  if Call.OutBody='["OK"]' then begin
    for i := 0 to fBatchCount-1 do
      Results.Add(HTTP_SUCCESS);
  end else begin
    doc := JSON.Parse(Call.OutBody);
    if (VariantType(doc)=jvArray) and (doc.length=fBatchCount) then
      for i := 0 to fBatchCount-1 do
        Results.Add(integer(doc[i]));
  end;
  {$else}
  SetLength(Results,fBatchCount);
  HttpBodyToText(Call.OutBody,jsonres);
  if jsonres='["OK"]' then begin
    for i := 0 to fBatchCount-1 do
      Results[i] := HTTP_SUCCESS;
  end else begin
    doc.Init(jsonres);
    if (doc.Kind=jvArray) and (doc.Count=fBatchCount) then
      for i := 0 to fBatchCount-1 do
        Results[i] := {$ifdef FPC}Int64{$endif}(doc.Values[i]);
  end;
  {$endif}
end;

/// marshall {result:...,id:...} and {result:...} body answers
function CallGetResult(const aCall: TSQLRestURIParams; var outID: integer): variant;
{$ifndef ISSMS}
var doc: TJSONVariantData;
    jsonres: string;
{$endif}
begin
  VarClear(result);
  outID := 0;
  if aCall.OutStatus<>HTTP_SUCCESS then
    exit;
  {$ifdef ISSMS}
  var doc := JSON.Parse(aCall.OutBody);
  if VarIsValidRef(doc.result) then
    result := doc.result;
  if VarIsValidRef(doc.id) then
    outID := doc.id;
  {$else}
  HttpBodyToText(aCall.OutBody,jsonres);
  doc.Init(jsonres);
  result := doc.ValueCopy['result']; // Value[] -> varByRef
  outID := doc.Value['id'];
  {$endif}
end;

function TSQLRestClientURI.CallBackGetResult(const aMethodName: string;
  const aNameValueParameters: array of const; aTable: TSQLRecordClass; aID: TID): string;
var Call: TSQLRestURIParams;
    dummyID: integer;
begin
  CallBackGet(aMethodName,aNameValueParameters,Call,aTable,aID);
  result := CallGetResult(Call,dummyID);
end;

procedure TSQLRestClientURI.CallRemoteServiceInternal(var Call: TSQLRestURIParams;
  aCaller: TServiceClientAbstract; const aMethod, aParams: string);
var url, clientDrivenID, sent, methName: string;
begin
  methName:= aCaller.fServiceURI+'.'+aMethod;
  Log(sllServiceCall,'Interface-based service '+methName);
  url := Model.Root+'/'+aCaller.fServiceURI;
  if aCaller.fInstanceImplementation=sicClientDriven then
    clientDrivenID := (aCaller as TServiceClientAbstractClientDriven).ClientID;
  ServicesRouting.ClientSideInvoke(url,aMethod,aParams,clientDrivenID,sent);
  Call.Init(url,'POST',sent);
  URI(Call); // asynchronous or synchronous call
  InternalServiceCheck(methName,Call); // will log only for synchronous call
end;

{ Some definitions copied from mORMot.pas unit }

type
  TServiceInternalMethod = (imFree, imContract, imSignature);

const
  SERVICE_PSEUDO_METHOD: array[TServiceInternalMethod] of string = (
    '_free_','_contract_','_signature_');

{$ifdef ISSMS}

procedure TSQLRestClientURI.SetAsynch(var Call: TSQLRestURIParams;
  onSuccess, onError: TSQLRestEvent; onBeforeSuccess: TSQLRestEventProcess);
begin
  if not Assigned(onSuccess) then
    raise ERestException.Create('SetAsynch expects onSuccess');
  inc(fAsynchCount);
  Call.OnSuccess :=
  lambda
    if Call.XHR.readyState=rrsDone then begin
      InternalStateUpdate(Call);
      if not assigned(onBeforeSuccess) then
        onSuccess(self) else
        if onBeforeSuccess then
          onSuccess(self) else
          if assigned(onError) then
            onError(self);
      if fAsynchCount>0 then
        dec(fAsynchCount);
      if fAsynchCount=0 then
        CallAsynchText; // send any pending asynchronous task
    end;
  end;
  Call.OnError :=
  lambda
    if Assigned(onError) then
      onError(Self);
    if fAsynchCount>0 then
      dec(fAsynchCount);
    if fAsynchCount=0 then
      CallAsynchText; // send any pending asynchronous task, even on error
  end;
end;

procedure TSQLRestClientURI.SetAsynchText(const Text: string);
begin
  fAsynchPendingText.Add(Text);
  if fAsynchCount=0 then
    CallAsynchText; // send it now if no pending asynchronous task
end;

procedure TSQLRestClientURI.CallAsynchText;
var Call: TSQLRestURIParams;
begin
  if length(fAsynchPendingText)=0 then
    exit; // nothing to send
  Call.Init(getURICallBack('RemoteLog',nil,0),'PUT',
    fAsynchPendingText.Join(#13#10)); // all rows sent at once
  fAsynchPendingText.Clear;
  SetAsynch(Call,lambda end,nil,nil); // asynchronous call without error check
  URI(Call);
end;

procedure TSQLRestClientURI.Connect(onSuccess, onError: TSQLRestEvent);
var Call: TSQLRestURIParams;
begin
  SetAsynch(Call,onSuccess,onError,
  lambda
    result := (Call.OutStatus=HTTP_SUCCESS) and SetServerTimeStamp(Call.OutBody);
  end);
  CallBackGet('TimeStamp',[],Call,nil); // asynchronous call
end;

procedure TSQLRestClientURI.CallRemoteServiceASynch(aCaller: TServiceClientAbstract;
  const aMethodName: string; aExpectedOutputParamsCount: integer;
  const aInputParams: array of variant;
  onSuccess: procedure(res: array of Variant); onError: TSQLRestEvent;
  aReturnsCustomAnswer: boolean);
var Call: TSQLRestURIParams;
begin
  // ForceServiceResultAsJSONObject not implemented yet
  SetAsynch(Call,
    lambda
      if not assigned(onSuccess) then
        exit; // no result to handle
      if aReturnsCustomAnswer then begin
        if Call.OutStatus=HTTP_SUCCESS then begin
          var result: TVariantDynArray;
          result.Add(Call.OutBody);
          onSuccess(result);
        end else
          if Assigned(onError) then
            onError(self);
        exit;
      end;
      var outID: integer;
      var result := CallGetResult(Call,outID); // from {result:...,id:...}
      if VarIsValidRef(result) then begin
         if (aCaller.fInstanceImplementation=sicClientDriven) and (outID<>0) then
           (aCaller as TServiceClientAbstractClientDriven).fClientID := IntToStr(outID);
        if aExpectedOutputParamsCount=0 then
          onSuccess([]) else begin
          var res := TJSONVariantData.CreateFrom(result);
          if (res.Kind=jvArray) and (res.Count=aExpectedOutputParamsCount) then
            onSuccess(res.Values) else
            if Assigned(onError) then
              onError(self);
        end;
      end else
        if Assigned(onError) then
          onError(self);
    end,
    onError,
    lambda
      result := (Call.OutStatus=HTTP_SUCCESS) and (Call.OutBody<>'');
    end);
  CallRemoteServiceInternal(Call,aCaller,aMethodName,JSON.Stringify(variant(aInputParams)));
end;

function TSQLRestClientURI.CallRemoteServiceSynch(aCaller: TServiceClientAbstract;
  const aMethodName: string; aExpectedOutputParamsCount: integer;
  const aInputParams: array of variant; aReturnsCustomAnswer: boolean): TVariantDynArray;
var Call: TSQLRestURIParams;
    outResult: variant;
    outID: integer;
procedure RaiseError;
begin
  raise EServiceException.CreateFmt('Error calling %s.%s - returned status %d',
    [aCaller.fServiceName,aMethodName,Call.OutStatus]);
end;
begin
  // ForceServiceResultAsJSONObject not implemented yet
  CallRemoteServiceInternal(Call,aCaller,aMethodName,JSON.Stringify(variant(aInputParams)));
  if aReturnsCustomAnswer then begin
    if Call.OutStatus<>HTTP_SUCCESS then
      RaiseError;
    result.Add(Call.OutBody);
    exit;
  end;
  outResult := CallGetResult(Call,outID); // from {result:...,id:...}
  if not VarIsValidRef(outResult) then
    RaiseError;
  if (aCaller.fInstanceImplementation=sicClientDriven) and (outID<>0) then
     (aCaller as TServiceClientAbstractClientDriven).fClientID := IntToStr(outID);
  if aExpectedOutputParamsCount=0 then
    exit; // returns default []
  var res := TJSONVariantData.CreateFrom(outResult);
  if (res.Kind=jvArray) and (res.Count=aExpectedOutputParamsCount) then
    result := res.Values else
    raise EServiceException.CreateFmt('Error calling %s.%s - '+
      'received %d parameters (expected %d)',
      [aCaller.fServiceName,aMethodName,res.Count,aExpectedOutputParamsCount]);
end;

{$else}

function TSQLRestClientURI.Connect: boolean;
var Call: TSQLRestURIParams;
begin
  Log(sllInfo,'Connect',self);
  CallBackGet('TimeStamp',[],Call,nil);
  result := Call.OutStatus=HTTP_SUCCESS;
  if not result then
    exit;
  result := SetServerTimeStamp(Call.OutBodyUtf8);
end;

procedure TSQLRestClientURI.CallRemoteService(aCaller: TServiceClientAbstract;
  const aMethodName: string; aExpectedOutputParamsCount: integer;
  const aInputParams: array of variant; out res: TVariantDynArray;
  aReturnsCustomAnswer: boolean);
var Call: TSQLRestURIParams;
    params: TJSONVariantData;
    result: variant;
    bodyerror: string;
    arr: PJSONVariantData;
    i,outID: integer;
begin
  params.Init;
  for i := 0 to high(aInputParams) do
    params.AddValue(aInputParams[i]);
  CallRemoteServiceInternal(Call,aCaller,aMethodName,params.ToJSON);
  if Call.OutStatus<>HTTP_SUCCESS then begin
    HttpBodyToText(Call.OutBody,bodyerror);
    raise EServiceException.CreateFmt('Error calling %s.%s - returned status %d'#13#10'%s',
      [aCaller.fServiceName,aMethodName,Call.OutStatus,bodyerror]);
  end;
  if aReturnsCustomAnswer then begin
    SetLength(res,1);
    res[0] := HttpBodyToVariant(Call.OutBody);
    exit;
  end;
  result := CallGetResult(Call,outID);
  if (aCaller.fInstanceImplementation=sicClientDriven) and (outID<>0) then
    (aCaller as TServiceClientAbstractClientDriven).fClientID := IntToStr(outID);
  if aExpectedOutputParamsCount=0 then
    exit;
  arr := JSONVariantDataSafe(result,jvArray); // Count=0 if not jvArray
  if arr^.Count<>aExpectedOutputParamsCount then
    raise EServiceException.CreateFmt('Error calling %s.%s - '+
      'received %d parameters (expected %d)',
      [aCaller.fServiceName,aMethodName,arr^.Count,aExpectedOutputParamsCount]);
  res := arr^.Values;
end;

{$endif ISSMS}

function TSQLRestClientURI.ExecuteAdd(tableIndex: integer;
  const json: string): TID;
var Call: TSQLRestURIParams;
    location: string;
    i: integer;
begin
  result := 0;
  Call.Init(getURIID(tableIndex,0),'POST',json);
  URI(Call);
  if Call.OutStatus<>HTTP_CREATED then begin
    Log(sllError,'Error creating %s with %s',[Model.Info[tableIndex].Name,json]);
    exit;
  end;
  location := GetOutHeader(Call,'location');
  for i := length(location) downto 1 do
    if not (ord(location[i]) in [ord('0')..ord('9')]) then begin
      result := StrToInt64Def(Copy(location,i+1,length(location)),0);
      break; // 'Location: root/People/11012' e.g.
    end;
  Log(sllDB,'%s.ID=%d created from %s',[Model.Info[tableIndex].Name,result,json]);
end;

function TSQLRestClientURI.Delete(Table: TSQLRecordClass;
  ID: TID): boolean;
var Call: TSQLRestURIParams;
    tableIndex: integer;
begin
  result := false;
  if ID<=0 then
    exit;
  tableIndex := Model.GetTableIndexExisting(Table);
  Call.Init(getURIID(tableIndex,ID),'DELETE','');
  URI(Call);
  if Call.OutStatus=HTTP_SUCCESS then
    result := true;
  Log(LOGLEVELDB[result],'Delete %s.ID=%d',[Model.Info[tableIndex].Name,ID]);
end;

function TSQLRestClientURI.ExecuteUpdate(tableIndex: integer; ID: TID;
  const json: string): boolean; 
var Call: TSQLRestURIParams;
begin
  Call.Init(getURIID(tableIndex,ID),'PUT',json);
  URI(Call);
  result := Call.OutStatus=HTTP_SUCCESS;
  Log(LOGLEVELDB[result],'Update %s.ID=%d with %s',[Model.Info[tableIndex].Name,ID,json]);
end;

function TSQLRestClientURI.SetUser(aAuthenticationClass: TSQLRestServerAuthenticationClass;
  const aUserName, aPassword: string; aHashedPassword: Boolean): boolean;
var aKey, aSessionID: string;
    i: integer;
begin
  result := false;
  if fAuthentication<>nil then
    SessionClose;
  if aAuthenticationClass=nil then
    exit;
  fAuthentication := aAuthenticationClass.Create(aUserName,aPassword,aHashedPassword);
  try
    aKey := fAuthentication.ClientComputeSessionKey(self);
    i := 1;
    GetNextCSV(aKey,i,aSessionID,'+');
    if TryStrToInt(aSessionID,i) and (i>0) then begin
      fAuthentication.SetSessionID(i);
      Log(sllUserAuth,'Session %d created for "%s" with %s',
        [i,aUserName,fAuthentication.ClassName]);
      result := true;
    end else begin
      fAuthentication.Free;
      fAuthentication := nil;
    end;
  except
    fAuthentication.Free;
    fAuthentication := nil;
  end;
  if fAuthentication=nil then
    Log(sllError,'Session not created for "%s"',[aUserName]);
end;

procedure TSQLRestClientURI.SessionClose;
var Call: TSQLRestURIParams;
begin
  if (self<>nil) and (fAuthentication<>nil) then
    try // notify Server to end of session
      CallBackGet('Auth',['UserName',fAuthentication.User.LogonName,
        'Session',fAuthentication.SessionID],Call,nil);
    finally
      fAuthentication.Free;
      fAuthentication := nil;
    end;
end;

{$ifndef ISSMS}
constructor TSQLRestClientURI.Create(aModel: TSQLModel; aOwnModel: boolean);
begin
  fRunningClientDriven := TStringList.Create;
  inherited Create(aModel,aOwnModel);
end;
{$endif}

destructor TSQLRestClientURI.Destroy;
begin
  {$ifndef ISSMS}
  fRunningClientDriven.Free;
  {$endif}
  SessionClose;
  inherited Destroy;
end;

{ TSQLRestClientHTTP }

constructor TSQLRestClientHTTP.Create(const aServer: string;
  aPort: integer; aModel: TSQLModel; aOwnModel, aHttps: boolean
  {$ifndef ISSMS}; const aProxyName, aProxyByPass: string;
  aSendTimeout, aReceiveTimeout, aConnectionTimeOut: Cardinal{$endif});
begin
  inherited Create(aModel,aOwnModel);
  fParameters.Server := aServer;
  fParameters.Port := aPort;
  fParameters.Https := aHttps;
  {$ifndef ISSMS}
  fParameters.ProxyName := aProxyName;
  fParameters.ProxyByPass := aProxyByPass;
  fParameters.ConnectionTimeOut := aConnectionTimeOut;
  fParameters.SendTimeout := aSendTimeout;
  fParameters.ReceiveTimeout := aReceiveTimeout;
  {$endif}
  fKeepAlive := 20000;
end;

destructor TSQLRestClientHTTP.Destroy;
begin
  inherited;
  fAuthentication.Free;
  fConnection.Free;
end;

procedure TSQLRestClientHTTP.InternalURI(var Call: TSQLRestURIParams);
var inType: string;
    retry: integer;
begin
  inType := FindHeader(Call.InHead,'content-type: ');
  if inType='' then begin
    if OnlyJSONRequests then
      inType := JSON_CONTENT_TYPE else
      inType := 'text/plain'; // avoid slow CORS preflighted requests
    Call.InHead := trim(Call.InHead+#13#10'content-type: '+inType);
  end;
  if fCustomHttpHeader<>'' then
    Call.InHead := trim(Call.InHead+fCustomHttpHeader);
  for retry := 0 to 1 do begin
    if fConnection=nil then
      try
        fConnection := HttpConnectionClass.Create(fParameters);
        // TODO: handle SynLZ compression and SHA/AES encryption?
      except
        on E: Exception do begin
          Log(E);
          fConnection.Free;
          fConnection := nil;
        end;
      end;
    if fConnection=nil then begin
      Call.OutStatus := HTTP_NOTIMPLEMENTED;
      break;
    end;
    try
      fConnection.URI(Call,inType,fKeepAlive);
      break; // do not retry on transmission success, or asynchronous request
    except
      on E: Exception do begin
        Log(E);
        fConnection.Free;
        fConnection := nil;
        Call.OutStatus := HTTP_NOTIMPLEMENTED;
        if fForceTerminate then
          break;
      end; // will retry once (e.g. if connection broken)
    end;
  end;
end;

procedure TSQLRestClientHTTP.SetHttpBasicAuthHeaders(const aUserName, aPasswordClear: RawUTF8);
var base64: RawUTF8;
begin
  base64 := aUsername+':'+aPasswordClear;
  {$ifdef ISSMS}
  base64 := window.btoa(base64);
  {$else}
  base64 := BytesToBase64JSONString(TByteDynArray(TextToHttpBody(base64)),false);
  {$endif}
  fCustomHttpHeader := #13#10'Authorization: Basic '+base64;
end;


{ TSQLAuthUser }

{$ifdef ISSMS} // manual RTTI for SMS

class function TSQLAuthUser.ComputeRTTI: TRTTIPropInfos;
begin
  result := TRTTIPropInfos.Create(
    ['Data','Group','LogonName','DisplayName','PasswordHashHexa'],
    [sftBlob]);
end;

procedure TSQLAuthUser.SetProperty(FieldIndex: integer; const Value: variant);
begin
  case FieldIndex of
  0: fID := Value;
  1: fData := Value;
  2: fGroup := Value;
  3: fLogonName := Value;
  4: fDisplayName := Value;
  5: fPasswordHashHexa := Value;
  end;
end;

function TSQLAuthUser.GetProperty(FieldIndex: integer): variant;
begin
  case FieldIndex of
  0: result := fID;
  1: result := fData;
  2: result := fGroup;
  3: result := fLogonName;
  4: result := fDisplayName;
  5: result := fPasswordHashHexa;
  end;
end;

{$endif}

function SHA256Compute(const Values: array of string): string;
var buf: THttpBody;
    a: integer;
    sha: TSHA256;
begin
  sha := TSHA256.Create;
  try
    for a := 0 to high(Values) do begin
      buf := TextToHttpBody(Values[a]);
      sha.Update(buf);
    end;
    result := sha.Finalize;
  finally
    sha.Free;
  end;
end;

procedure TSQLAuthUser.SetPasswordPlain(const Value: string);
begin
  PasswordHashHexa := SHA256Compute(['salt',Value]);
end;


{ TSQLRestServerAuthentication }

constructor TSQLRestServerAuthentication.Create(const aUserName, aPassword: string;
  aHashedPassword: Boolean);
begin
  fUser := TSQLAuthUser.Create;
  fUser.LogonName := aUserName;
  if aHashedPassword then
    fUser.PasswordHashHexa := aPassword else
    fUser.PasswordPlain := aPassword;
end;

destructor TSQLRestServerAuthentication.Destroy;
begin
  fUser.Free;
  inherited;
end;

procedure TSQLRestServerAuthentication.SetSessionID(Value: Cardinal);
begin
  fSessionID := Value;
  fSessionIDHexa8 := LowerCase(IntToHex(Value,8));
end;

{ TSQLRestServerAuthenticationDefault }

function TSQLRestServerAuthenticationDefault.ClientComputeSessionKey(
  Sender: TSQLRestClientURI): string;
var aServerNonce, aClientNonce, aPassHash: string;
begin
  if fUser.LogonName='' then
    exit;
  aServerNonce := Sender.CallBackGetResult('Auth',['UserName',User.LogonName],nil);
  if aServerNonce='' then
    exit;
  aClientNonce := SHA256Compute([Copy(NowToIso8601,1,16)]);
  aPassHash := Sha256Compute([Sender.Model.Root,aServerNonce,aClientNonce,
    User.LogonName,User.PasswordHashHexa]);
  result := Sender.CallBackGetResult('Auth',['UserName',User.LogonName,
    'Password',aPassHash,'ClientNonce',aClientNonce],nil);
  fSessionPrivateKey := crc32ascii(crc32ascii(0,result),fUser.fPasswordHashHexa);
end;

function TSQLRestServerAuthenticationDefault.ClientSessionComputeSignature(
  Sender: TSQLRestClientURI; const url: string): string;
var nonce: string;
begin
  nonce := LowerCase(IntToHex(trunc(Now*(24*60*60)),8));
  result := fSessionIDHexa8+nonce+LowerCase(IntToHex(
    crc32ascii(crc32ascii(fSessionPrivateKey,nonce),url),8));
end;

{ TSQLRestServerAuthenticationNone }

function TSQLRestServerAuthenticationNone.ClientComputeSessionKey(
  Sender: TSQLRestClientURI): string;
begin
  result := Sender.CallBackGetResult('Auth',['UserName',User.LogonName],nil);
end;

function TSQLRestServerAuthenticationNone.ClientSessionComputeSignature(
  Sender: TSQLRestClientURI; const url: string): string;
begin
  result := fSessionIDHexa8;
end;

{$ifdef ISSMS}

{ TSQLAuthGroup }  // manual RTTI for SMS

class function TSQLAuthGroup.ComputeRTTI: TRTTIPropInfos;
begin
  result := TRTTIPropInfos.Create(
    ['Ident','SessionTimeOut','AccessRights'],[]);
end;

procedure TSQLAuthGroup.SetProperty(FieldIndex: integer; const Value: variant);
begin
  case FieldIndex of
  0: fID := Value;
  1: fIdent := Value;
  2: fSessionTimeOut := Value;
  3: fAccessRights := Value;
  end;
end;

function TSQLAuthGroup.GetProperty(FieldIndex: integer): variant;
begin
  case FieldIndex of
  0: result := fID;
  1: result := fIdent;
  2: result := fSessionTimeOut;
  3: result := fAccessRights;
  end;
end;


function VariantToBlob(const Value: variant): TSQLRawBlob;
begin
  if TVariant.IsString(Value) then begin
    var s: string := Value;
    if s='' then
      result := null else
      result := BrowserAPI.Window.atob(s);
  end else
    result := null;
end;

function BlobToVariant(const Blob: TSQLRawBlob): variant;
begin
  if TVariant.IsString(Blob) then
    result := BrowserAPI.Window.btoa(Blob) else
    result := null;
end;

function VariantToGUID(const value: variant): TGUID; inline;
begin
  result := value; // no-op since TGUID=string
end;

function GUIDToVariant(const GUID: TGUID): variant; inline;
begin
  result := GUID; // no-op since TGUID=string
end;

function VariantToHttpBody(const value: variant): THttpBody;
begin
  result := value; // no-op since THttpBody=string
end;

function HttpBodyToVariant(const HttpBody: THttpBody): variant;
begin
  result := HttpBody; // no-op since THttpBody=string
end;

{$else}

{$ifdef FPC} // original VarIsStr() does not handle varByRef as expected :(
function VarIsStr(const Value: variant): boolean; inline;
begin
  result := Variants.VarIsStr(PVariant(FindVarData(Value))^);
end;
{$endif}

function VariantToBlob(const Value: variant): TSQLRawBlob;
begin
  if VarIsStr(Value) then // avoid conversion error from null to string
    Base64JSONStringToBytes(Value,result) else
    Finalize(result);
end;

function BlobToVariant(const Blob: TSQLRawBlob): variant;
begin
  if Blob=nil then
    result := null else
    result := BytesToBase64JSONString(Blob);
end;

function VariantToGUID(const value: variant): TGUID;
var S: string;
begin
  FillChar(result,SizeOf(result),0);
  if not VarIsStr(value) then
    exit;
  S := string(Value);
  if S<>'' then
    try
      result := SysUtils.StringToGUID('{'+s+'}');
    except
      ; // ignore any conversion error and return void TGUID
    end;
end;

function GUIDToVariant(const GUID: TGUID): variant;
begin
  try
    result := Copy(SysUtils.GUIDToString(GUID),2,36);
  except
    result := ''; // should not happen
  end;
end;

const
  varHttpBody = {$ifdef UNICODE}varUString{$else}varString{$endif};

function VariantToHttpBody(const value: variant): THttpBody;
var P: PCardinal;
    Len: cardinal;
begin
  result := nil;
  with TVarData(value) do begin
    if (VType<>varHttpBody) or (VAny=nil) then
      exit;
    P := VAny;
    {$ifdef UNICODE}
    Len := P^;
    if Len>=cardinal(length(UnicodeString(VAny))*2) then
      exit; // input does not come from HttpBodyToVariant() -> avoid GPF
    inc(P);
    {$else}
    Len := length(RawByteString(VAny));
    {$endif}
    SetLength(result,Len);
    move(P^,pointer(result)^,len);
  end;
end;

function HttpBodyToVariant(const HttpBody: THttpBody): variant;
var P: PCardinal;
    Len: cardinal;
begin
  VarClear(result);
  Len := length(HttpBody);
  if Len>0 then
  with TVarData(result) do begin
    VType := varHttpBody;
    VAny := nil;
    {$ifdef UNICODE}
    SetLength(UnicodeString(VAny),Len shr 1+2);
    P := VAny;
    P^ := Len;
    inc(P);
    {$else}
    SetLength(RawByteString(VAny),Len);
    P := VAny;
    {$endif}
    move(pointer(HttpBody)^,P^,Len);
  end;
end;

{$endif ISSMS}

function VariantToEnum(const Value: variant; const TextValues: array of string): integer;
{$ifdef ISSMS}
begin
  if TVariant.IsNumber(Value) then
    result := Value else begin
    result := TextValues.IndexOf(string(Value));
    if result>=0 then
      exit;
{$else}
var str: string;
begin
  if VarIsOrdinal(Value) then
    result := Value else begin
    str := Value;
    if str<>'' then
      for result := 0 to high(TextValues) do
        if str=TextValues[result] then
          exit;
{$endif}
    result := 0; // return first item by default
  end;
end;

function ObjectToVariant(value: TSQLRecord): variant;
begin
  result := value.ToVariant;
end;


{ TServiceClientAbstract }

constructor TServiceClientAbstract.Create(aClient: TSQLRestClientURI);
var Call: TSQLRestURIParams; // manual synchronous call
    dummyID: integer;
    result: variant;
    contract: string;
begin
  if (fServiceName='') or (fServiceURI='') then
    raise EServiceException.CreateFmt(
      'Overriden %s.Create should have set properties',[ClassName]);
  if aClient=nil then
    raise EServiceException.CreateFmt('%s.Create(nil)',[ClassName]);
  fClient := aClient;
  fClient.CallRemoteServiceInternal(Call,self,SERVICE_PSEUDO_METHOD[imContract],'[]');
  result := CallGetResult(Call,dummyID);
  {$ifdef ISSMS}
  if VariantType(result)=jvArray then
    contract := result[0] else
    contract := result.contract;  // if ResultAsJSONObject=true
  {$else}
  with JSONVariantDataSafe(result,jvArray)^ do // Count=0 if not jvArray
    if Count=1 then
      contract := Values[0] else
      contract := Value['contract']; // if ResultAsJSONObject=true
  {$endif}
  if contract<>fContractExpected then
    raise EServiceException.CreateFmt('Invalid contract "%s" for %s: expected "%s"',
      [contract,ClassName,fContractExpected]);
end;

function TServiceClientAbstract.GetClient: TSQLRestClientURI;
begin
  result := fClient;
end;

function TServiceClientAbstract.GetContractExpected: string;
begin
  result := fContractExpected;
end;

function TServiceClientAbstract.GetInstanceImplementation: TServiceInstanceImplementation;
begin
  result := fInstanceImplementation;
end;

function TServiceClientAbstract.GetRunningInstance: TServiceClientAbstract;
begin
  result := self;
end;

function TServiceClientAbstract.GetServiceName: string;
begin
  result := fServiceName;
end;

function TServiceClientAbstract.GetServiceURI: string;
begin
  result := fServiceURI;
end;


{ TServiceClientAbstractClientDriven }

constructor TServiceClientAbstractClientDriven.Create(aClient: TSQLRestClientURI);
begin
  if fInstanceImplementation<>sicClientDriven then
    raise EServiceException.CreateFmt(
      'Overriden %s.Create should have set sicClientDriven',[ClassName]);
  if aClient.fRunningClientDriven.IndexOf(fServiceName)>=0 then
    raise EServiceException.CreateFmt('Only ONE instance of %s is allowed at once',
      [ClassName]);
  inherited Create(aClient); // will synchronously check the contract from server
  aClient.fRunningClientDriven.Add(fServiceName); // mark as opened
end;

destructor TServiceClientAbstractClientDriven.Destroy;
var ndx: integer;
    {$ifndef ISSMS}
    res: TVariantDynArray;
    {$endif}
begin
  if fClient<>nil then
  try
    if fClientID<>'' then
      {$ifdef ISSMS}
      fClient.CallRemoteServiceAsynch(self,SERVICE_PSEUDO_METHOD[imFree],0,[],nil,nil);
      {$else}
      try // synchronous blocking call
        fClient.CallRemoteService(self,SERVICE_PSEUDO_METHOD[imFree],0,[],res);
      except
        ; // ignore, since the connection may be broken (will timeout on server)
      end;
      {$endif}
  finally
    ndx := fClient.fRunningClientDriven.IndexOf(ServiceName);
    if ndx>=0 then
      fClient.fRunningClientDriven.Delete(ndx); // mark as closed
  end;
  inherited;
end;

{ TSQLRestRoutingREST }

class procedure TSQLRestRoutingREST.ClientSideInvoke(var uri: String;
  const method: String; const params: String; const clientDrivenID: String;
  var sent: String);
begin
  if clientDrivenID<>'' then
    uri := uri+'.'+method+'/'+clientDrivenID else
    uri := uri+'.'+method;
  sent := params; // we may also encode them within the URI
end;

{ TSQLRestRoutingJSON_RPC }

class procedure TSQLRestRoutingJSON_RPC.ClientSideInvoke(var uri: String;
  const method: String; const params: String; const clientDrivenID: String;
  var sent: String);
begin
  sent := '{"method":"'+method+'","params":'+params;
  if clientDrivenID='' then
    sent := sent+'}' else
    sent := sent+',"id":'+clientDrivenID+'}';
end;

/// SmartMobileStudio does not like constant sets with the Delphi syntax
var nfo: TSynLogInfo;
initialization
  include(LOG_STACKTRACE,sllLastError);
  include(LOG_STACKTRACE,sllError);
  include(LOG_STACKTRACE,sllException);
  include(LOG_STACKTRACE,sllExceptionOS);
  for nfo := succ(sllNone) to high(nfo) do
    include(LOG_VERBOSE,nfo);
end.
