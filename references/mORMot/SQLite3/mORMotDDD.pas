/// Domain-Driven-Design toolbox for mORMot
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotDDD;

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

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
{$ifdef MSWINDOWS}
  Windows,
{$else}
  {$ifdef FPC}
  SynFPCLinux,
  {$endif FPC}
{$endif MSWINDOWS}
  SysUtils,
  Classes,
  Contnrs,
  Variants,
  SyncObjs,
  SynCrtSock,
  SynCommons,
  SynLog,
  SynCrypto,
  SynTable, // for TSynFilter and TSynValidate
  mORMot;

{ some mORMot conventions about DDD implementation:

 * most services methods should return an enumerate:
  - Exceptions should be raised only in case of a real failure (e.g. unexpected
    execution context, service shutdown...) but most of the time an enumerate
    will be used to manage errors
  - no textual error message should be sent by the application layer: it is up
    to the end-user application to react to a given unexpected result
  - within the DDD services, CQRS methods will use a TCQRSResult enumerate
    and advanced error process, with an optional error text for debugging
  - first value should mean success, so that TInterfaceStub would let the
    test pass by returning the default 0 ordinal

 * persistence ignorance is mostly implemented via CQRS repository services:
  - we implement the "Command Query Responsibility Segregation" pattern
    to potentially increase scaling ability of reading, and allow distributed
    transactions via a service-based two-phase commit, implementing the Unit Of
    Work pattern
  - no ID should be transmitted most of the time, but write commands have to
    follow a read query which would specify the corresponding item so that
    the ID would be stored during the process
  - I*Query interfaces should have some SelectBy*() methods to change the
    current selected aggregate, which could later on be retrieved by a
    Get(out aAggregate: T....) method
  - I*Command instances would be our way of implementing the Unit Of Work
  - I*Command interfaces should have standard Add/Update/Delete methods,
    expecting a previous SelectBy*() call for Update/Delete: those methods
    should prepare the corresponding data change
  - I*Command interfaces should have Commit to perform all the pending
    changes (which may be implemented by transactions or via TSQLRestBatch)
  - I*Command interfaces should abort the process if the instance is
    released without any prior call to Commit (e.g. rollback the transaction
    or free any pending TSQLRestBatch instance)

}

{ *********** Some Domain-Driven-Design Common Definitions }

type
  /// abstract ancestor for all Domain-Driven Design related Exceptions
  EDDDException = class(ESynException);

  /// Exception type linked to CQRS repository service methods
  ECQRSException = class(EDDDException);

  /// abstract ancestor for any Domain-Driven Design infrastructure Exceptions
  EDDDInfraException = class(EDDDException);


{ ----- Persistence / Repository Interfaces }

type
  /// result enumerate for I*Query/I*Command CQRS repository service methods
  // - cqrsSuccess will map the default TInterfaceStub returned value
  // - cqrsSuccessWithMoreData would be used e.g. for versioned publish/
  // subscribe to notify the caller that there are still data available, and
  // the call should be reiterated until cqrsSuccess is returned
  // - cqrsBadRequest would indicate that the method was not called in the
  // expected workflow sequence
  // - cqrsNotFound appear after a I*Query SelectBy*() method with no match
  // - cqrsNoMoreData indicates a GetNext*() method has no more matching data
  // - cqrsDataLayerError indicates a low-level error at database level
  // - cqrsInvalidCallback is returned if a callback is required for this method
  // - cqrsInternalError for an unexpected issue, like an Exception raised
  // - cqrsDDDValidationFailed will be trigerred when
  // - cqrsInvalidContent for any I*Command method with invalid aggregate input
  // value (e.g. a missing field)
  // - cqrsAlreadyExists for a I*Command.Add method with a primay key conflict
  // - cqrsNoPriorQuery for a I*Command.Update/Delete method with no prior
  // call to SelectBy*()
  // - cqrsNoPriorCommand for a I*Command.Commit with no prior Add/Update/Delete
  // - cqrsNoMatch will notify that a command did not have any match
  // - cqrsNotImplemented may be returned when there is no code yet for a method
  // - cqrsBusy is returned if the command could not be executed, since it is
  // currently processing a request
  // - cqrsTimeout indicates that the method didn't succeed in the expected time
  // - otherwise, cqrsUnspecifiedError will be used for any other kind of error
  TCQRSResult =
    (cqrsSuccess, cqrsSuccessWithMoreData,
     cqrsUnspecifiedError, cqrsBadRequest, cqrsNotFound,
     cqrsNoMoreData, cqrsDataLayerError, cqrsInvalidCallback,
     cqrsInternalError, cqrsDDDValidationFailed,
     cqrsInvalidContent, cqrsAlreadyExists,
     cqrsNoPriorQuery, cqrsNoPriorCommand,
     cqrsNoMatch, cqrsNotImplemented,
     cqrsBusy, cqrsTimeout);

  /// generic interface, to be used for CQRS I*Query and I*Command types definition
  // - TCQRSService class will allow to easily implement LastError* members
  // - all CQRS services, which may be executed remotely, would favor a function
  // result as TCQRSResult enumeration for error handling, rather than a local
  // Exception, which is not likely to be transferred easily on consummer side
  ICQRSService = interface(IInvokable)
    /// should return the last error as an enumerate
    // - when stubed or mocked via TInterfaceStub, any method interface would
    // return 0, i.e. cqrsSuccess by default, to let the test pass
    function GetLastError: TCQRSResult;
    /// should return addition information for the last error
    // - may be a plain string, or a JSON document stored as TDocVariant
    function GetLastErrorInfo: variant;
  end;

/// returns the text equivalency of a CQRS result enumeration
function ToText(res: TCQRSResult): PShortString; overload;

const
  /// successfull result enumerates for I*Query/I*Command CQRS
  // - those items would generate no log entry
  // - i.e. any command not included in CQRSRESULT_SUCCESS nor CQRSRESULT_WARNING
  // would trigger a sllDDDError log entry
  CQRSRESULT_SUCCESS = [
    cqrsSuccess, cqrsSuccessWithMoreData, cqrsNoMoreData, cqrsNotFound];

  /// dubious result enumerates for I*Query/I*Command CQRS
  // - those items would generate a sllDDDInfo log entry
  // - i.e. any command not included in CQRSRESULT_SUCCESS nor CQRSRESULT_WARNING
  // would trigger a sllDDDError log entry
  CQRSRESULT_WARNING = [
    cqrsNotFound, cqrsNoMatch];


{ ----- Services / Daemon Interfaces }

type
  /// generic interface, to be used so that you may retrieve a running state
  IMonitored = interface(IInvokable)
    ['{7F5E1569-E06B-48A0-954C-95784EC23363}']
    /// retrieve the current status of the instance
    // - the status is returned as a schema-less value (typically a TDocVariant
    // document), which may contain statistics about the current processing
    // numbers, timing and throughput
    function RetrieveState(out Status: variant): TCQRSResult;
  end;

  /// generic interface, to be used so that you may manage a service/daemon instance
  IMonitoredDaemon = interface(IMonitored)
    ['{F5717AFC-5D0E-4E13-BD5B-25C08CB177A7}']
    /// launch the service/daemon
    // - should first stop any previous running instance (so may be used to
    // restart a service on demand)
    function Start: TCQRSResult;
    /// abort the service/daemon, returning statistics about the whole execution
    function Stop(out Information: variant): TCQRSResult;
  end;

  /// generic interface, to manage a service/daemon instance from an executable
  // - in addition to Start/Stop methods, Halt would force the whole executable
  // to abort its execution, SubscribeLog allows log monitoring, and
  // DatabaseList/DatabaseExecute remote SQL/SOA execution on one or several
  // logicial REST servers
  // - those methods would allow a single administration daemon (installed e.g.
  // as a Windows Service) to be able to launch and monitor child processes as
  // individual executables, or via a custom DDD's ToolsAdmin tool
  // - since SubscribeLog() uses a callback, this REST server should be
  // published via supported transmission protocol, e.g. WebSockets
  IAdministratedDaemon = interface(IMonitoredDaemon)
    ['{BD02919E-56ED-4559-967A-EFBC0E85C031}']
    /// will Stop the service/daemon process, then quit the executable
    // - the returned Information and TCQRSResult are passed directly from
    // the Stop() method
    function Halt(out Information: variant): TCQRSResult;
    /// returns a list of internal database names, exposed by this daemon
    // - in practice, each database name should identify a TSQLRest instance
    // - the database name should be supplied to DatabaseExecute() as target
    function DatabaseList: TRawUTF8DynArray;
    /// returns a list of tables, stored in the internal database names
    // - the database name should match one existing in the DatabaseList
    // - in practice, returns all TSQLRecord table of the TSQLRest instance
    function DatabaseTables(const DatabaseName: RawUTF8): TRawUTF8DynArray;
    /// execute a SQL query on an internal database
    // - the database name should match one existing in the DatabaseList
    // - the supplied SQL parameter may be #cmd internal commands: in this case,
    // the database name may not be mandatory
    // - will return JSON most of the time, but may return binary if needed
    function DatabaseExecute(const DatabaseName,SQL: RawUTF8): TServiceCustomAnswer;
    /// used to subscribe for real-time remote log monitoring
    // - allows to track the specified log events, with a callback
    // - you can specify a number of KB of existing log content to send to the
    // monitoring tool, before the actual real-time process: Callback.Log()
    // would be called first with Level=sllNone and all the existing text
    procedure SubscribeLog(const Level: TSynLogInfos; const Callback: ISynLogCallback;
      ReceiveExistingKB: cardinal);
    /// will be called when a callback is released on the client side
    // - this method matches IServiceWithCallbackReleased signature
    // - will be used to unsubscribe any previous ISynLogCallback notification
    procedure CallbackReleased(const callback: IInvokable; const interfaceName: RawUTF8);
  end;

  /// any service/daemon implementing this interface would be able to redirect
  // all the administration process to another service/daemon
  // - i.e. would work as a safe proxy service, over several networks
  IAdministratedDaemonAsProxy = interface(IAdministratedDaemon)
    ['{5B7A9086-3D96-48F2-8E27-C6624B2EB45A}']
    /// allows to connect to another service/daemon IAdministratedDaemon
    // - detailed connection definition would be supplied as a TDocVariantData
    // object, serialized from dddInfraApp.pas TDDDRestClientSettings
    function StartProxy(const aDDDRestClientSettings: variant): TCQRSResult;
  end;

  /// allow persistence of any TObject settings storage
  IDDDSettingsStorable = interface
    ['{713A9C16-4BBD-4FB6-A7A6-566162767622}']
    /// persist the settings if needed
    // - will call the virtual InternalPersist method
    procedure StoreIfUpdated;
  end;



{ *********** Cross-Cutting Layer Implementation}

{ ----- Persistence / Repository CQRS Implementation }

type
  /// which kind of process is about to take place after an CqrsBeginMethod()
  TCQRSQueryAction = (
    qaNone,
    qaSelect, qaGet,
    qaCommandDirect, qaCommandOnSelect,
    qaCommit);

  /// define one or several process to take place after an CqrsBeginMethod()
  TCQRSQueryActions = set of TCQRSQueryAction;

  /// the current step of a TCQRSQuery state machine
  // - basic state diagram is defined by the methods execution:
  // - qsNone refers to the default state, with no currently selected values,
  // nor any pending write request
  // - qsQuery corresponds to a successful I*Query.Select*(), expecting
  // either a I*Query.Get*(), or a I*Command.Add/Update/Delete
  // - qsCommand corresponds to a successful I*Command.Add/Update/Delete,
  // expected a I*Command.Commit
  TCQRSQueryState = (qsNone, qsQuery, qsCommand);

  /// to be inherited to implement CQRS I*Query or I*Command services extended
  // error process
  // - you should never assign directly a cqrs* value to a method result, but
  // rather use the CqrsBeginMethod/CqrsSetResult/CqrsSetResultMsg methods provided by this class:
  // ! function TMyService.MyMethod: TCQRSResult;
  // ! begin
  // !   CqrsBeginMethod(qsNone,result); // reset the error information to cqrsUnspecifiedError
  // !   ... // do some work
  // !   if error then
  // !     CqrsSetResultMsg(cqrsUnspecifiedError,'Oups! For "%"',[name],result) else
  // !     CqrsSetResult(cqrsSuccess,result); // instead of result := cqrsSuccess
  // !   end;
  // - the methods are implemented as a simple state machine, following
  // the TCQRSQueryAction and TCQRSQueryState definitions
  // - warning: by definition, fLastError* access is NOT thread-safe so the
  // CqrsBeginMethod/CqrsSetResult feature should be used in a single context
  TCQRSService = class(TInjectableObject, ICQRSService)
  protected
    fLastError: TCQRSResult;
    fLastErrorContext: variant;
    fAction: TCQRSQueryAction;
    fState: TCQRSQueryState;
    {$ifdef WITHLOG}
    fLog: TSynLogFamily;
    {$endif}
    fSafe: TSynLocker;
    // method to be called at first for LastError process
    function CqrsBeginMethod(aAction: TCQRSQueryAction; var aResult: TCQRSResult;
      aError: TCQRSResult=cqrsUnspecifiedError): boolean; virtual;
    function CqrsSetResultError(aError: TCQRSResult): TCQRSResult; virtual;
    // methods to be used to set the process end status
    procedure CqrsSetResult(Error: TCQRSResult; var Result: TCQRSResult); overload;
    procedure CqrsSetResult(E: Exception; var Result: TCQRSResult); overload;
    procedure CqrsSetResultSuccessIf(SuccessCondition: boolean; var Result: TCQRSResult;
      ErrorIfFalse: TCQRSResult=cqrsDataLayerError);
    procedure CqrsSetResultMsg(Error: TCQRSResult; const ErrorMessage: RawUTF8;
        var Result: TCQRSResult); overload;
    procedure CqrsSetResultMsg(Error: TCQRSResult; const ErrorMsgFmt: RawUTF8;
      const ErrorMsgArgs: array of const; var Result: TCQRSResult); overload;
    procedure CqrsSetResultString(Error: TCQRSResult; const ErrorMessage: string;
      var Result: TCQRSResult);
    procedure CqrsSetResultDoc(Error: TCQRSResult; const ErrorInfo: variant;
      var Result: TCQRSResult);
    procedure CqrsSetResultJSON(Error: TCQRSResult; const JSONFmt: RawUTF8;
      const Args,Params: array of const; var Result: TCQRSResult);
    function GetLastError: TCQRSResult;
    function GetLastErrorInfo: variant; virtual;
    procedure InternalCqrsSetResult(Error: TCQRSResult; var Result: TCQRSResult); virtual;
    procedure AfterInternalCqrsSetResult; virtual;
  public
    /// initialize the instance
    constructor Create; override;
    /// finalize the instance
    destructor Destroy; override;
    {$ifdef WITHLOG}
    /// where logging should take place
    property Log: TSynLogFamily read fLog write fLog;
    {$endif}
  published
    /// the last error, as an enumerate
    property LastError: TCQRSResult read GetLastError;
    /// the last error extended information, as a string or TDocVariant
    property LastErrorInfo: variant read GetLastErrorInfo;
    /// the action currently processing
    property Action: TCQRSQueryAction read fAction;
    /// current step of the TCQRSService state machine
    property State: TCQRSQueryState read fState;
  end;

  /// a CQRS Service, which maintains an internal list of "Subscribers"
  // - allow to notify in cascade when a callback is released
  TCQRSServiceSubscribe = class(TCQRSService)
  protected
    fSubscriber: array of IServiceWithCallbackReleased;
    // will call all fSubscriber[].CallbackReleased() methods
    procedure CallbackReleased(const callback: IInvokable; const interfaceName: RawUTF8);
  end;

  /// a CQRS Service, ready to implement a set of synchronous (blocking) commands
  // over an asynchronous (non-blocking) service
  // - you may use this class e.g. at API level, over a blocking REST server,
  // and communicate with the Domain event-driven services via asynchronous calls
  // - this class won't inherit from TCQRSService, since it would be called
  // from multiple threads at once, so all CQRSSetResult() methods would fail
  TCQRSServiceSynch = class(TInterfacedObject)
  protected
    fSharedCallbackRef: IUnknown;
  public
    constructor Create(const sharedcallback: IUnknown); reintroduce;
  end;

  /// used to acknowledge asynchronous CQRS Service calls
  // - e.g. to implement TCQRSServiceSynch
  TCQRSServiceAsynchAck = class(TInterfacedObject)
  protected
    fLog: TSynLogClass;
    fCalls: TBlockingProcessPool;
  public
    destructor Destroy; override;
  end;

  /// class-reference type (metaclass) of TCQRSService
  TCQRSServiceClass = class of TCQRSService;

/// returns the text equivalency of a CQRS state enumeration
function ToText(res: TCQRSQueryState): PShortString; overload;


{ ----- Persistence / Repository Implementation using mORMot's ORM }

type
  TDDDRepositoryRestFactory = class;
  TDDDRepositoryRestQuery = class;

  /// class-reference type (metaclass) to implement I*Query or I*Command
  // interface definitions using our RESTful ORM
  TDDDRepositoryRestClass = class of TDDDRepositoryRestQuery;

  /// abstract ancestor for all persistence/repository related Exceptions
  EDDDRepository = class(ESynException)
  public
    /// constructor like FormatUTF8() which will also serialize the caller info
    constructor CreateUTF8(Caller: TDDDRepositoryRestFactory;
      const Format: RawUTF8; const Args: array of const);
  end;

  /// store reference of several factories, each with one mapping definition
  TDDDRepositoryRestFactoryObjArray = array of TDDDRepositoryRestFactory;

  /// home repository of several DDD Entity factories using REST storage
  // - this shared class will be can to manage a service-wide repositories,
  // e.g. manage actual I*Query/I*Command implementation classes accross a
  // set of TSQLRest instances
  // - is designed to optimize BATCH or transactional process
  TDDDRepositoryRestManager = class
  protected
    fFactory: TDDDRepositoryRestFactoryObjArray;
  public
    /// finalize all factories
    destructor Destroy; override;
    /// register one DDD Entity repository over an ORM's TSQLRecord
    // - will raise an exception if the aggregate has already been defined
    function AddFactory(
      const aInterface: TGUID; aImplementation: TDDDRepositoryRestClass;
      aAggregate: TClass; aRest: TSQLRest; aTable: TSQLRecordClass;
      const TableAggregatePairs: array of RawUTF8): TDDDRepositoryRestFactory;
    /// retrieve the registered definition of a given DDD Entity in Factory[]
    // - returns -1 if the TPersistence class is unknown
    function GetFactoryIndex(const aInterface: TGUID): integer;
    /// retrieve the registered Factory definition of a given DDD Entity
    // - raise an EDDDRepository exception if the TPersistence class is unknown
    function GetFactory(const aInterface: TGUID): TDDDRepositoryRestFactory;
    /// read-only access to all defined DDD Entity factories
    property Factory: TDDDRepositoryRestFactoryObjArray read fFactory;
  end;

  /// implement a DDD Entity factory over one ORM's TSQLRecord
  // - it will centralize some helper classes and optimized class mapping
  // - the Entity class may be defined as any TPersistent or TSynPersistent, with
  // an obvious preference for TSynPersistent and TSynAutoCreateFields classes
  TDDDRepositoryRestFactory = class(TInterfaceResolverForSingleInterface)
  protected
    fOwner: TDDDRepositoryRestManager;
    fInterface: TInterfaceFactory;
    fRest: TSQLRest;
    fTable: TSQLRecordClass;
    fAggregate: TClassInstance;
    fAggregateRTTI: TSQLPropInfoList;
    // stored in fGarbageCollector, following fAggregateProp[]
    fGarbageCollector: TObjectDynArray;
    fFilter: array of array of TSynFilter;
    fValidate: array of array of TSynValidate;
    // TSQLPropInfoList correspondance, as filled by ComputeMapping:
    fAggregateToTable: TSQLPropInfoObjArray;
    fAggregateProp: TSQLPropInfoRTTIObjArray;
    fAggregateID: TSQLPropInfoRTTI;
    // store custom field mapping between TSQLRecord and Aggregate
    fPropsMapping: TSQLRecordPropertiesMapping;
    fPropsMappingVersion: cardinal;
    procedure ComputeMapping;
    function GetAggregateName: string;
    function GetTableName: string;
    // override those methods to customize the data marshalling
    procedure AggregatePropToTable(
      aAggregate: TObject; aAggregateProp: TSQLPropInfo;
      aRecord: TSQLRecord; aRecordProp: TSQLPropInfo); virtual;
    procedure TablePropToAggregate(
      aRecord: TSQLRecord; aRecordProp: TSQLPropInfo;
      aAggregate: TObject; aAggregateProp: TSQLPropInfo); virtual;
    function GetAggregateRTTIOptions: TSQLPropInfoListOptions; virtual;
    // main IoC/DI method, returning a TDDDRepositoryRest instance
    function CreateInstance: TInterfacedObject; override;
  public
    /// will compute the ORM TSQLRecord* source code type definitions
    // corresponding to DDD aggregate objects into a a supplied file name
    // - will generate one TSQLRecord* per aggregate class level, following the
    // inheritance hierarchy
    // - dedicated DDD types will be translated into native ORM types (e.g. RawUTF8)
    // - if no file name is supplied, it will generate a dddsqlrecord.inc file
    // in the executable folder
    // - could be used as such:
    // ! TDDDRepositoryRestFactory.ComputeSQLRecord([TPersonContactable,TAuthInfo]);
    // - once created, you may refine the ORM definition, by adding
    // ! ...  read f.... write f... stored AS_UNIQUE;
    // for fields which should be unique, and/or
    // ! ... read f... write f... index #;
    // to specify an optional textual field width (VARCHAR n) for SQL storage
    // - most advanced ORM-level filters/validators, or low-level implementation
    // details (like the Sqlite3 collation) may be added by overriding this method:
    // !protected
    // !  class procedure InternalDefineModel(Props: TSQLRecordProperties); override;
    // ! ...
    // !class procedure TSQLRecordMyAggregate.InternalDefineModel(
    // !  Props: TSQLRecordProperties);
    // !begin
    // !  AddFilterNotVoidText(['HashedPassword']);
    // !  Props.SetCustomCollation('Field','BINARY');
    // !  Props.AddFilterOrValidate('Email',TSynValidateEmail.Create);
    // !end;
    class procedure ComputeSQLRecord(const aAggregate: array of TClass;
      DestinationSourceCodeFile: TFileName='');
    /// initialize the DDD Aggregate factory using a mORMot ORM class
    // - by default, field names should match on both sides - but you can
    // specify a custom field mapping as TSQLRecord,Aggregate pairs
    // - any missing or unexpected field on any side will just be ignored
    constructor Create(
      const aInterface: TGUID; aImplementation: TDDDRepositoryRestClass;
      aAggregate: TClass; aRest: TSQLRest; aTable: TSQLRecordClass;
      const TableAggregatePairs: array of RawUTF8;
      aOwner: TDDDRepositoryRestManager=nil); reintroduce; overload;
    /// initialize the DDD Aggregate factory using a mORMot ORM class
    // - this overloaded constructor does not expect any custom fields
    // - any missing or unexpected field on any side will just be ignored
    constructor Create(
      const aInterface: TGUID; aImplementation: TDDDRepositoryRestClass;
      aAggregate: TClass; aRest: TSQLRest; aTable: TSQLRecordClass;
      aOwner: TDDDRepositoryRestManager=nil); reintroduce; overload;
    /// finalize the factory
    destructor Destroy; override;
    /// register a custom filter or validator to some Aggregate's fields
    // - once added, the TSynFilterOrValidate instance will be owned to
    // this factory, until it is released
    // - the field names should be named from their full path (e.g. 'Email' or
    // 'Address.Country.Iso') unless aFieldNameFlattened is TRUE, which will
    // expect ORM-like naming (e.g. 'Address_Country')
    // - if '*' is specified as field name, it will be applied to all text
    // fields, so the following will ensure that all text fields will be
    // trimmed for spaces:
    // !   AddFilterOrValidate(['*'],TSynFilterTrim.Create);
    // - filters and validators will be applied to a specified aggregate using
    // AggregateFilterAndValidate() method
    // - the same filtering classes as with the ORM can be applied to DDD's
    // aggregates, e.g. TSynFilterUpperCase, TSynFilterLowerCase or
    // TSynFilterTrim
    // - the same validation classes as with the ORM can be applied to DDD's
    // aggregates, e.g. TSynValidateText.Create for a void field,
    // TSynValidateText.Create('{MinLength:5}') for a more complex test
    // (including custom password strength validation if TSynValidatePassWord
    // is not enough), TSynValidateIPAddress.Create or TSynValidateEmail.Create
    // for some network settings, or TSynValidatePattern.Create()
    // - you should not define TSynValidateUniqueField here, which could't be
    // checked at DDD level, but rather set a "stored AS_UNIQUE" attribute
    // in the corresponding property of the TSQLRecord type definition
    procedure AddFilterOrValidate(const aFieldNames: array of RawUTF8;
      aFilterOrValidate: TSynFilterOrValidate; aFieldNameFlattened: boolean=false); virtual;
    /// clear all properties of a given DDD Aggregate
    procedure AggregateClear(aAggregate: TObject);
    /// create a new DDD Aggregate instance
    function AggregateCreate: TObject; {$ifdef HASINLINE}inline;{$endif}
    /// perform filtering and validation on a supplied DDD Aggregate
    // - all logic defined by AddFilterOrValidate() will be processed
    function AggregateFilterAndValidate(aAggregate: TObject;
      aInvalidFieldIndex: PInteger=nil; aValidator: PSynValidate=nil): RawUTF8; virtual;
    /// serialize a DDD Aggregate as JSON
    // - you can optionaly force the generated JSON to match the mapped
    // TSQLRecord fields, so that it would be compatible with ORM's JSON
    procedure AggregateToJSON(aAggregate: TObject; W: TJSONSerializer;
      ORMMappedFields: boolean; aID: TID); overload;
    /// serialize a DDD Aggregate as JSON RawUTF8
    function AggregateToJSON(aAggregate: TObject; ORMMappedFields: boolean;
      aID: TID): RawUTF8; overload;
    /// convert a DDD Aggregate into an ORM TSQLRecord instance
    procedure AggregateToTable(aAggregate: TObject; aID: TID; aDest: TSQLRecord);
    /// convert a ORM TSQLRecord instance into a DDD Aggregate
    procedure AggregateFromTable(aSource: TSQLRecord; aAggregate: TObject);
    /// convert ORM TSQLRecord.FillPrepare instances into a DDD Aggregate ObjArray
    procedure AggregatesFromTableFill(aSource: TSQLRecord; var aAggregateObjArray);
    /// the home repository owning this factory
    property Owner: TDDDRepositoryRestManager read fOwner;
    /// the DDD's Entity class handled by this factory
    // - may be any TPersistent, but very likely a TSynAutoCreateFields class
    property Aggregate: TClass read fAggregate.ItemClass;
    /// the ORM's TSQLRecord used for actual storage
    property Table: TSQLRecordClass read fTable;
    /// the mapped DDD's Entity class published properties RTTI
    property Props: TSQLPropInfoList read fAggregateRTTI;
    /// access to the Aggregate / ORM field mapping
    property FieldMapping: TSQLRecordPropertiesMapping read fPropsMapping;
  published
    /// the associated I*Query / I*Command repository interface
    property Repository: TInterfaceFactory read fInterface;
    /// the associated TSQLRest instance
    property Rest: TSQLRest read fRest;
    /// the DDD's Entity class name handled by this factory
    property AggregateClass: string read GetAggregateName;
    /// the ORM's TSQLRecord class name used for actual storage
    property TableClass: string read GetTableName;
  end;

  /// abstract repository class to implement I*Query interface using RESTful ORM
  // - actual repository implementation will just call the ORM*() protected
  // method from the published Aggregate-oriented CQRS service interface
  TDDDRepositoryRestQuery = class(TCQRSService)
  protected
    fFactory: TDDDRepositoryRestFactory;
    fCurrentORMInstance: TSQLRecord;
    function CqrsBeginMethod(aAction: TCQRSQueryAction; var aResult: TCQRSResult;
      aError: TCQRSResult=cqrsUnspecifiedError): boolean; override;
    // one-by-one retrieval in local ORM: TSQLRecord
    function ORMSelectOne(const ORMWhereClauseFmt: RawUTF8;
      const Bounds: array of const; ForcedBadRequest: boolean=false): TCQRSResult;
    function ORMSelectID(const ID: TID; RetrieveRecord: boolean=true;
       ForcedBadRequest: boolean=false): TCQRSResult; overload;
    function ORMSelectID(const ID: RawUTF8; RetrieveRecord: boolean=true;
       ForcedBadRequest: boolean=false): TCQRSResult; overload;
    function ORMGetAggregate(aAggregate: TObject): TCQRSResult;
    // list retrieval - using cursor-like access via ORM.FillOne
    function ORMSelectAll(const ORMWhereClauseFmt: RawUTF8;
      const Bounds: array of const; ForcedBadRequest: boolean=false): TCQRSResult;
    function ORMGetNextAggregate(aAggregate: TObject; aRewind: boolean=false): TCQRSResult;
    function ORMGetAllAggregates(var aAggregateObjArray): TCQRSResult;
    function ORMSelectCount(const ORMWhereClauseFmt: RawUTF8; const Args,Bounds: array of const;
      out aResultCount: integer; ForcedBadRequest: boolean=false): TCQRSResult;
  public
    /// you should not have to use this constructor, since the instances would
    // be injected by TDDDRepositoryRestFactory.TryResolve()
    constructor Create(aFactory: TDDDRepositoryRestFactory); reintroduce; virtual;
    /// finalize the used memory
    destructor Destroy; override;
    /// return the number all currently selected aggregates
    // - returns 0 if no select was available, 1 if it was a ORMGetSelectOne(),
    // or the number of items after a ORMGetSelectAll()
    // - this is a generic operation which would work for any class
    // - if you do not need this method, just do not declare it in I*Command
    function GetCount: integer; virtual;
    /// returns the associated TSQLRest instance used in the associated factory
    // - this method is able to extract it from a I*Query/I*Command instance,
    // if it is implemented by a TDDDRepositoryRestQuery class
    // - returns nil if the supplied Service is not recognized
    class function GetRest(const Service: ICQRSService): TSQLRest;
  published
    /// access to the associated factory
    property Factory: TDDDRepositoryRestFactory read fFactory;
    /// access to the current state of the underlying mapped TSQLRecord
    // - is nil if no query was run yet
    // - contains the queried object after a successful Select*() method
    // - is either a single object, or a list of objects, via its internal
    // CurrentORMInstance.FillTable cursor
    property CurrentORMInstance: TSQLRecord read fCurrentORMInstance;
  end;

  /// abstract class to implement I*Command interface using ORM's TSQLRecord
  // - it will use an internal TSQLRestBatch for dual-phase commit, therefore
  // implementing a generic Unit Of Work / Transaction pattern
  TDDDRepositoryRestCommand = class(TDDDRepositoryRestQuery)
  protected
    fBatch: TSQLRestBatch;
    fBatchAutomaticTransactionPerRow: cardinal;
    fBatchOptions: TSQLRestBatchOptions;
    fBatchResults: TIDDynArray;
    procedure ORMEnsureBatchExists; virtual;
    // this default implementation will check the status vs command,
    // call DDD's + ORM's FilterAndValidate, then add to the internal BATCH
    // - you should override it, if you need a specific behavior
    // - if aAggregate is nil, fCurrentORMInstance field values would be used
    // - if aAggregate is set, its fields would be set to fCurrentORMInstance
    procedure ORMPrepareForCommit(aCommand: TSQLOccasion;
      aAggregate: TObject; var Result: TCQRSResult; aAllFields: boolean=false); virtual;
    /// minimal implementation using AggregateToTable() conversion
    function ORMAdd(aAggregate: TObject; aAllFields: boolean=false): TCQRSResult; virtual;
    function ORMUpdate(aAggregate: TObject; aAllFields: boolean=false): TCQRSResult; virtual;
    /// this default implementation will send the internal BATCH
    // - you should override it, if you need a specific behavior
    procedure InternalCommit(var Result: TCQRSResult); virtual;
    /// on rollback, delete the internal BATCH - called by Destroy
    procedure InternalRollback; virtual;
  public
    /// this constructor will set default fBatch options
    constructor Create(aFactory: TDDDRepositoryRestFactory); override;
    /// finalize the Unit Of Work context
    // - any uncommited change will be lost
    destructor Destroy; override;
    /// perform a deletion on the currently selected aggregate
    // - this is a generic operation which would work for any class
    // - if you do not need this method, just do not declare it in I*Command
    function Delete: TCQRSResult; virtual;
    /// perform a deletion on all currently selected aggregates
    // - this is a generic operation which would work for any class
    // - if you do not need this method, just do not declare it in I*Command
    function DeleteAll: TCQRSResult; virtual;
    /// write all pending changes prepared by Add/Update/Delete methods
    // - this is the only mandatory method, to be declared in your I*Command
    // - in practice, will send the current internal BATCH to the REST instance
    function Commit: TCQRSResult; virtual;
    /// flush any pending changes prepared by Add/Update/Delete methods
    // - if you do not need this method, just do not publish it in I*Command
    // - the easiest to perform a roll-back would be to release the I*Command
    // instance - but you may explictly reset the pending changes by calling
    // this method
    // - in practice, will release the internal BATCH instance
    function Rollback: TCQRSResult; virtual;
    /// access to the low-level BATCH instance, used for dual-phase commit
    // - you should not need to access it directly, but rely on Commit and
    // Rollback methods to
    property Batch: TSQLRestBatch read fBatch;
  end;

  /// abstract CQRS class tied to a TSQLRest instance for low-level persistence
  // - not used directly by the DDD repositories (since they will rely on
  // a TDDDRepositoryRestFactory for the actual ORM process), but may be the
  // root class for any Rest-based infrastructure cross-cutting features
  TCQRSQueryObjectRest = class(TCQRSService)
  protected
    fRest: TSQLRest;
  public
    /// this constructor would identify a TServiceContainer SOA resolver
    // and set the Rest property
    // - when called e.g. by TServiceFactoryServer.CreateInstance()
    constructor CreateWithResolver(aResolver: TInterfaceResolver;
      aRaiseEServiceExceptionIfNotFound: boolean); overload; override;
    /// reintroduced constructor, allowing to specify the associated REST instance
    constructor Create(aRest: TSQLRest); reintroduce; virtual;
    /// reintroduced constructor, associating a REST instance with the supplied
    // IoC resolvers
    constructor CreateWithResolver(aRest: TSQLRest; aResolver: TInterfaceResolver;
      aRaiseEServiceExceptionIfNotFound: boolean=true); reintroduce; overload;
    /// reintroduced constructor, associating a REST instance with the supplied
    // IoC resolvers (may be stubs/mocks, resolver classes or single instances)
    constructor CreateInjected(aRest: TSQLRest;
      const aStubsByGUID: array of TGUID;
      const aOtherResolvers: array of TInterfaceResolver;
      const aDependencies: array of TInterfacedObject); reintroduce;
    /// access to the associated REST instance
    property Rest: TSQLRest read FRest;
  end;


{ ----- Services / Daemon Implementation }

type
  TDDDMonitoredDaemon = class;

  /// the current state of a process thread
  TDDDMonitoredDaemonProcessState = (
    dpsPending, dpsProcessing, dpsProcessed, dpsFailed);

  /// abstract process thread class with monitoring abilities
  TDDDMonitoredDaemonProcess = class(TThread)
  protected
    fDaemon: TDDDMonitoredDaemon;
    fIndex: integer;
    fProcessIdleDelay: cardinal;
    fMonitoring: TSynMonitorWithSize;
    /// the main thread loop, which will call the protected Execute* methods
    procedure Execute; override;
  protected
    /// check for any pending task, and mark it as started
    // - will be executed within fDaemon.fProcessLock so that it would be
    // atomic among all fDaemon.fProcess[] threads - overriden implementation
    // should therefore ensure that this method executes as fast as possible
    // to minimize contention
    // - returns FALSE if there is no pending task
    // - returns TRUE if there is a pending task, and ExecuteProcessAndSetResult
    // should be called by Execute outside the fDaemon.fProcessLock
    function ExecuteRetrievePendingAndSetProcessing: boolean; virtual; abstract;
    /// execute the task, and set its resulting state
    // - resulting state may be e.g. "processed" or "failed"
    // - should return the number of bytes processed, for fMonitoring update
    // - will be executed outside fDaemon.fProcessLock so that overriden
    // implementation may take as much time as necessary for its process
    function ExecuteProcessAndSetResult: QWord;  virtual; abstract;
    /// finalize the pending task
    // - will always be called, even if ExecuteRetrievePendingAndSetProcessing
    // returned FALSE
    procedure ExecuteProcessFinalize; virtual; abstract;
    /// is called when there is no more pending task
    // - may be used e.g. to release a connection or some resource (e.g. an
    // ORM TSQLRestBatch instance)
    procedure ExecuteIdle; virtual; abstract;
    /// will be called on any exception in Execute
    // - this default implementation will call fMonitoring.ProcessError()
    procedure ExecuteOnException(E: Exception); virtual;
  public
    /// initialize the process thread for a given Service/Daemon instance
    constructor Create(aDaemon: TDDDMonitoredDaemon; aIndexInDaemon: integer); virtual;
    /// finalize the process thread
    destructor Destroy; override;
    /// milliseconds delay defined before getting the next pending tasks
    // - equals TDDDMonitoredDaemon.ProcessIdleDelay, unless a fatal exception
    // occurred during TDDDMonitoredDaemonProcess.ExecuteIdle method: in this
    // case, the delay would been increased to 500 ms
    property IdleDelay: cardinal read fProcessIdleDelay;
  end;

  /// abstract process thread class with monitoring abilities, using the ORM
  // for pending tasks persistence
  // - a protected TSQLRecord instance will be maintained to store the
  // processing task and its current state
  TDDDMonitoredDaemonProcessRest = class(TDDDMonitoredDaemonProcess)
  protected
    /// the internal ORM instance used to maintain the current task
    // - it should contain the data to be processed, the processing state
    // (e.g. at least "processing", "processed" and "failed"), and optionally
    // the resulting content (if any)
    // - overriden ExecuteRetrievePendingAndSetProcessing method should create
    // fPendingTask then save fPendingTask.State to "processing"
    // - overriden ExecuteProcessAndSetResult method should perform the task,
    // then save fPendingTask.State to "processed" or "failed"
    fPendingTask: TSQLRecord;
    /// finalize the pending task: will free fPendingTask and set it to nil
    procedure ExecuteProcessFinalize; override;
  end;


  /// class-reference type (metaclass) to determine which actual thread class
  // will implement the monitored process
  TDDDMonitoredDaemonProcessClass = class of TDDDMonitoredDaemonProcess;

  /// abstract class using several process threads and with monitoring abilities
  // - able to implement any DDD Daemon/Service, with proper statistics gathering
  // - each TDDDMonitoredDaemon will own its TDDDMonitoredDaemonProcess
  TDDDMonitoredDaemon = class(TCQRSQueryObjectRest,IMonitoredDaemon)
  protected
    fProcess: array of TDDDMonitoredDaemonProcess;
    fProcessClass: TDDDMonitoredDaemonProcessClass;
    fProcessMonitoringClass: TSynMonitorClass;
    fProcessLock: IAutoLocker;
    fProcessTimer: TPrecisionTimer;
    fProcessThreadCount: integer;
    fProcessIdleDelay: integer;
    fMonitoringClass: TSynMonitorClass;
    function GetStatus: variant; virtual;
  public
    /// abstract constructor, which should not be called by itself
    constructor Create(aRest: TSQLRest); overload; override;
    /// you should override this constructor to set the actual process
    // - i.e. define the fProcessClass protected property
    constructor Create(aRest: TSQLRest; aProcessThreadCount: integer); reintroduce; overload;
    /// finalize the Daemon
    destructor Destroy; override;
    /// monitor the Daemon/Service by returning some information as a TDocVariant
    // - its Status.stats sub object will contain global processing statistics,
    // and Status.threadstats similar information, detailled by running thread
    function RetrieveState(out Status: variant): TCQRSResult;
    /// launch all processing threads
    // - any previous running threads are first stopped
    function Start: TCQRSResult; virtual;
    /// finalize all processing threads
    // - and returns updated statistics as a TDocVariant
    function Stop(out Information: variant): TCQRSResult; virtual;
  published
    /// how many process threads should be created by this Daemon/Service
    property ProcessThreadCount: integer read fProcessThreadCount;
    /// how many milliseconds each process thread should wait before checking
    // for pending tasks
    // - default value is 50 ms, which seems good enough in practice
    property ProcessIdleDelay: integer read fProcessIdleDelay write fProcessIdleDelay;
  end;

  /// current status of an administrable service/daemon
  TDDDAdministratedDaemonStatus = (
    dsUnknown, dsCreated, dsStarted, dsStopped, dsHalted);

  /// abstract class to implement an administrable service/daemon
  // - a single administration daemon (running e.g. as a Windows Service) would
  // be able to launch and administrate such process, via a remote REST link
  // - inherited class should override the Internal* virtual abstract protected
  // methods to supply the actual process (e.g. set a background thread)
  TDDDAdministratedDaemon = class(TCQRSService, IAdministratedDaemon)
  protected
    {$ifdef WITHLOG}
    fLog: TSynLogFamily;
    {$endif}
    fStatus: TDDDAdministratedDaemonStatus;
    fFinished: TEvent;
    fRemoteLog: TSynLogCallbacks;
    fInternalDatabases: TSQLRestDynArray;
    fInternalSettings: TObject;
    fInternalSettingsFolder: TFileName;
    fAdministrationServer: TSQLRestServer;
    fAdministrationServerOwned: boolean;
    fAdministrationHTTPServer: TObject;
    // return TRUE e.g. if TDDDAdministratedThreadDaemon.fThread<>nil
    function InternalIsRunning: boolean; virtual; abstract;
    // should start the daemon: e.g. set TDDDAdministratedThreadDaemon.fThread
    procedure InternalStart; virtual; abstract;
    // return TRUE and set Status (e.g. from monitoring info) on success
    // - this default implement returns the system memory info as current state
    function InternalRetrieveState(var Status: variant): boolean; virtual;
    // should end the daemon: e.g. TDDDAdministratedThreadDaemon.fThread := nil
    procedure InternalStop; virtual;
    // set the list of published TSQLRestInstances - InternalStop would release it
    procedure PublishORMTables(const Rest: array of TSQLRest); virtual;
    function PublishedORM(const DatabaseName: RawUTF8): TSQLRest;
    procedure SetInternalSettings(Settings: TObject); virtual;
  public
    /// initialize the administrable service/daemon
    // - aAdministrationServer.ServiceDefine(IAdministratedDaemon) will be
    // called to publish the needed methods over it, to allow remote
    // administration from a single administration daemon (installed e.g. as a
    // Windows Service)
    // - this constructor won't start the associated process, which would be
    // idle until the Start method is called
    constructor Create(aAdministrationServer: TSQLRestServer); reintroduce; overload; virtual;
    /// initialize the administrable service/daemon with its own TSQLRestServer
    // - will initialize and own its dedicated TSQLRestServerFullMemory
    // - if aUserName is specified, authentication will be enabled, and a
    // single TSQLAuthUser will be created, with the supplied credentials
    // (the password matching TSQLAuthUser.PasswordHashHexa expectations)
    // - under Windows, you can export the administration server as named pipe,
    // if the optional aServerNamedPipe parameter is set
    constructor Create(const aUserName,aHashedPassword: RawUTF8;
      const aRoot: RawUTF8='admin'; const aServerNamedPipe: TFileName=''); reintroduce; overload;
    /// finalize the service/daemon
    // - will call Halt() if the associated process is still running
    destructor Destroy; override;
    /// monitor the Daemon/Service by returning some information as a TDocVariant
    function RetrieveState(out Status: variant): TCQRSResult; virtual;
    /// IAdministratedDaemon command to launch the associated process
    // - if the process was already running, returns cqrsAlreadyExists
    function Start: TCQRSResult; virtual;
    /// IAdministratedDaemon command to finalize the associated process
    // - and returns updated statistics as a TDocVariant
    function Stop(out Information: variant): TCQRSResult; virtual;
    /// IAdministratedDaemon command to Stop the associated process, then
    // quit the executable
    // - returning the same output information than Stop()
    function Halt(out Information: variant): TCQRSResult; virtual;
    /// IAdministratedDaemon command to retrieve all internal databases names
    // - will return fInternalDatabases[].Model.Root values
    function DatabaseList: TRawUTF8DynArray; virtual;
    /// IAdministratedDaemon command to return the table names of an internal database
    function DatabaseTables(const DatabaseName: RawUTF8): TRawUTF8DynArray; virtual;
    /// IAdministratedDaemon command to execute a SQL query on an internal database
    // - you may override this method to implement addition "pseudo-SQL" commands
    function DatabaseExecute(const DatabaseName,SQL: RawUTF8): TServiceCustomAnswer; virtual;
    /// IAdministratedDaemon command to subscribe to a set of events for
    // real-time remote monitoring of the specified log events
    procedure SubscribeLog(const Levels: TSynLogInfos; const Callback: ISynLogCallback;
      ReceiveExistingKB: cardinal); virtual;
    /// IAdministratedDaemon command called when a callback is released on the client side
    procedure CallbackReleased(const callback: IInvokable; const interfaceName: RawUTF8);
    /// run the daemon, until it is halted
    // - if RemotelyAdministrated is FALSE, it will Start the process, then
    // wait until the [Enter] key is pressed (to be used in pure console mode)
    // - if RemotelyAdministrated is TRUE, it will follow remote activation
    // from its administration server
    // - both modes will log some minimal message on the console (if any)
    procedure Execute(RemotelyAdministrated: boolean);
    /// this method will wait until Halt() is executed
    // - i.e. protected fFinished TEvent is notified
    procedure WaitUntilHalted; virtual;
    /// returns the daemon name
    // - e.g. TMyOwnDaemon would return 'MyOwn' text
    function DaemonName: RawUTF8; virtual;
    {$ifdef WITHLOG}
    /// access to the associated loging class
    property Log: TSynLogFamily read fLog;
    {$endif}
    /// access to the associated internal settings
    // - is defined as an opaque TObject instance, to avoid unneeded dependencies
    property InternalSettings: TObject read fInternalSettings write SetInternalSettings;
    /// access to the associated internal settings storage folder
    property InternalSettingsFolder: TFileName
      read fInternalSettingsFolder write fInternalSettingsFolder;
    /// reference to the WebSockets/HTTP server publishing AdministrationServer
    // - is defined as an opaque TObject instance, to avoid unneeded dependencies
    property AdministrationHTTPServer: TObject
      read fAdministrationHTTPServer write fAdministrationHTTPServer;
  published
    /// the current status of the service/daemon
    property Status: TDDDAdministratedDaemonStatus read fStatus;
    /// reference to the REST server publishing IAdministratedDaemon service
    // - e.g. from named pipe local communication on Windows
    property AdministrationServer: TSQLRestServer read fAdministrationServer;
  end;

  /// type used to define a class kind of TDDDAdministratedDaemon
  TDDDAdministratedDaemonClass = class of TDDDAdministratedDaemon;

  /// abstract class to implement an TThread-based administrable service/daemon
  // - inherited class should override InternalStart and InternalRetrieveState
  // abstract methods, and set the protected fThread with the processing thread
  TDDDAdministratedThreadDaemon = class(TDDDAdministratedDaemon)
  protected
    fThread: TThread;
    // return TRUE if fThread<>nil (i.e. has been set by InternalStart)
    function InternalIsRunning: boolean; override;
    // end the daemon, i.e fThread := nil
    procedure InternalStop; override;
  end;

  /// abstract class to implement a TSQLRest-based administrable service/daemon
  // - inherited class should override InternalStart and InternalRetrieveState
  // abstract methods, and set the protected fRest with the processing TSQLRest
  TDDDAdministratedRestDaemon = class(TDDDAdministratedDaemon)
  protected
    fRest: TSQLRestServer;
    // return TRUE if fRest<>nil (i.e. has been set by InternalStart)
    function InternalIsRunning: boolean; override;
    // end the daemon, i.e. FreeAndNil(fRest)
    procedure InternalStop; override;
  public
    /// read-only access to the associated REST instance
    // - is assigned only between daemon Start/Stop
    property Rest: TSQLRestServer read fRest;
  end;

  /// abstract class to monitor an administrable service/daemon
  // - including Input/Output statistics and connected Clients count
  // - including OS Memory information
  TDDDAdministratedDaemonMonitor = class(TSynAutoCreateFields)
  protected
    fServer: TSynMonitorServer;
    function GetMemory: variant;
  public
    procedure ProcessException(E: Exception); virtual;
  published
    /// information about the REST server process
    property Server: TSynMonitorServer read fServer;
    /// information about the main System memory, as returned by the OS
    property SystemMemory: variant read GetMemory;
  end;


{ *********** Application Layer Implementation }

type
  /// abstract class for implementing an Application Layer service
  // - is defined as an TInjectableAutoCreateFields, so that any published
  // properties defined as interfaces would be resolved at creation, and
  // published properties defined as TPersistent/TSynPersistent will be
  // managed by this instance, i.e. created and released with it
  TDDDApplication = class(TInjectableAutoCreateFields)
  protected
  public
  end;



implementation

{ *********** Persistence / Repository Interfaces }

var
  TCQRSResultText: array[TCQRSResult] of PShortString;

function ToText(res: TCQRSResult): PShortString;
begin
  result := TCQRSResultText[res];
end;

function ToText(res: TCQRSQueryState): PShortString; overload;
begin
  result := GetEnumName(TypeInfo(TCQRSQueryState),ord(res));
end;


{ TCQRSService }

constructor TCQRSService.Create;
begin
  inherited Create;
  fSafe.Init;
  {$ifdef WITHLOG}
  fLog := SQLite3Log.Family; // may be overriden
  {$endif}
end;

function TCQRSService.GetLastError: TCQRSResult;
begin
  result := fLastError;
end;

function TCQRSService.GetLastErrorInfo: Variant;
begin
  result := fLastErrorContext;
end;

const
  NEEDS_QUERY   = [qaGet, qaCommandOnSelect];
  NEEDS_COMMAND = [qaCommit];
  ACTION_TO_STATE: array[TCQRSQueryAction] of TCQRSQueryState = (
    // qsNone = no state change after this action
    qsNone, qsQuery, qsNone, qsCommand, qsCommand, qsNone);

function TCQRSService.CqrsBeginMethod(aAction: TCQRSQueryAction;
  var aResult: TCQRSResult; aError: TCQRSResult): boolean;
begin
  aResult := aError;
  VarClear(fLastErrorContext);
  if (aAction in NEEDS_QUERY) and (fState<qsQuery) then begin
    CqrsSetResult(cqrsNoPriorQuery, aResult);
    result := false;
    exit;
  end;
  if (aAction in NEEDS_COMMAND) and (fState<qsCommand) then begin
    CqrsSetResult(cqrsNoPriorCommand, aResult);
    result := false;
    exit;
  end;
  fAction := aAction;
  result := true;
end;

function TCQRSService.CqrsSetResultError(aError: TCQRSResult): TCQRSResult;
begin
  CqrsBeginMethod(qaNone,result);
  CqrsSetResult(aError,result);
end;

procedure TCQRSService.CqrsSetResult(Error: TCQRSResult; var Result: TCQRSResult);
begin
  InternalCqrsSetResult(Error,Result);
  AfterInternalCqrsSetResult;
end;

procedure TCQRSService.CqrsSetResult(E: Exception; var Result: TCQRSResult);
begin
  InternalCqrsSetResult(cqrsInternalError,Result);
  _ObjAddProps(['Exception',ObjectToVariantDebug(E)],fLastErrorContext);
  AfterInternalCqrsSetResult;
end;

procedure TCQRSService.InternalCqrsSetResult(Error: TCQRSResult; var Result: TCQRSResult);
begin
  Result := Error;
  fLastError := Error;
  if Error<>cqrsSuccess then
    fLastErrorContext := ObjectToVariantDebug(self,'%',[NowToString]) else
    if ACTION_TO_STATE[fAction]<>qsNone then
      fState := ACTION_TO_STATE[fAction];
  fAction := qaNone;
end;

procedure TCQRSService.AfterInternalCqrsSetResult;
{$ifdef WITHLOG}
var
  level: TSynLogInfo;
{$endif}
begin
  {$ifdef WITHLOG}
  if fLastError in CQRSRESULT_SUCCESS then
    exit;
  if fLastError in CQRSRESULT_WARNING then
    level := sllDDDInfo else
    level := sllDDDError;
  if level in fLog.Level then
      fLog.SynLog.Log(level, 'CqrsSetResult(%) state=% %',
        [ToText(fLastError)^, ToText(fState)^, fLastErrorContext], self);
  {$endif}
end;

procedure TCQRSService.CqrsSetResultSuccessIf(SuccessCondition: boolean;
  var Result: TCQRSResult; ErrorIfFalse: TCQRSResult);
begin
  if SuccessCondition then
    CqrsSetResult(cqrsSuccess,Result) else
    CqrsSetResult(ErrorIfFalse,Result);
end;

procedure TCQRSService.CqrsSetResultDoc(Error: TCQRSResult; const ErrorInfo: variant;
  var Result: TCQRSResult);
begin
  InternalCqrsSetResult(Error,Result);
  _ObjAddProps(['ErrorInfo',ErrorInfo],fLastErrorContext);
  AfterInternalCqrsSetResult;
end;

procedure TCQRSService.CqrsSetResultJSON(Error: TCQRSResult;
  const JSONFmt: RawUTF8; const Args,Params: array of const; var Result: TCQRSResult);
begin
  CqrsSetResultDoc(Error,_JsonFastFmt(JSONFmt,Args,Params),Result);
end;

procedure TCQRSService.CqrsSetResultMsg(Error: TCQRSResult;
  const ErrorMessage: RawUTF8; var Result: TCQRSResult);
begin
  InternalCqrsSetResult(Error,Result);
  _ObjAddProps(['Msg',ErrorMessage],fLastErrorContext);
  AfterInternalCqrsSetResult;
end;

procedure TCQRSService.CqrsSetResultString(Error: TCQRSResult;
  const ErrorMessage: string; var Result: TCQRSResult);
begin
  InternalCqrsSetResult(Error,Result);
  _ObjAddProps(['Msg',ErrorMessage],fLastErrorContext);
  AfterInternalCqrsSetResult;
end;

procedure TCQRSService.CqrsSetResultMsg(Error: TCQRSResult;
  const ErrorMsgFmt: RawUTF8; const ErrorMsgArgs: array of const; var Result: TCQRSResult);
begin
  CqrsSetResultMsg(Error,FormatUTF8(ErrorMsgFmt,ErrorMsgArgs),Result);
end;

destructor TCQRSService.Destroy;
begin
  inherited Destroy;
  fSafe.Done;
end;


{ TCQRSServiceSubscribe }

procedure TCQRSServiceSubscribe.CallbackReleased(const callback: IInvokable;
  const interfaceName: RawUTF8);
var i: integer;
begin
  fSafe.Lock;
  try
    {$ifdef WITHLOG}
    fLog.SynLog.Log(sllTrace,'CallbackReleased(%,"%") callback=%',
      [callback,interfaceName,ObjectFromInterface(callback)],Self);
    {$endif}
    for i := 0 to high(fSubscriber) do // try to release on ALL subscribers
      fSubscriber[i].CallbackReleased(callback, interfaceName);
  finally
    fSafe.UnLock;
  end;
end;


{ TCQRSServiceSynch }

constructor TCQRSServiceSynch.Create(const sharedcallback: IInterface);
begin
  inherited Create;
  if sharedcallback = nil then
    raise EDDDException.CreateUTF8('%.Create(nil)', [self]);
  fSharedCallbackRef := sharedcallback;
end;


{ TCQRSServiceAsynchAck }

destructor TCQRSServiceAsynchAck.Destroy;
begin
  fCalls.Free; // would force evTimeOut if some WaitFor are still pending
  inherited Destroy;
end;


{ ----- Persistence / Repository Implementation using mORMot's ORM }

{ EDDDRepository }

constructor EDDDRepository.CreateUTF8(Caller: TDDDRepositoryRestFactory;
  const Format: RawUTF8; const Args: array of const);
begin
  if Caller=nil then
    inherited CreateUTF8(Format,Args) else
    inherited CreateUTF8('% - %',[FormatUTF8(Format,Args),ObjectToJSONDebug(Caller)]);
end;


{ TDDDRepositoryRestFactory }

constructor TDDDRepositoryRestFactory.Create(
  const aInterface: TGUID; aImplementation: TDDDRepositoryRestClass;
  aAggregate: TClass; aRest: TSQLRest; aTable: TSQLRecordClass;
  const TableAggregatePairs: array of RawUTF8; aOwner: TDDDRepositoryRestManager);
begin
  fInterface := TInterfaceFactory.Get(aInterface);
  if fInterface=nil then
    raise EDDDRepository.CreateUTF8(self,
     '%.Create(%): Interface not registered - you could use TInterfaceFactory.'+
     'RegisterInterfaces()',[self,GUIDToShort(aInterface)]);
  inherited Create(fInterface.InterfaceTypeInfo,aImplementation);
  fOwner := aOwner;
  fRest := aRest;
  fTable := aTable;
  if (aAggregate=nil) or (fRest=nil) or (fTable=nil) then
    raise EDDDRepository.CreateUTF8(self,'Invalid %.Create(nil)',[self]);
  fAggregate.Init(aAggregate);
  fPropsMapping.Init(aTable,RawUTF8(aAggregate.ClassName),aRest,false,[]);
  fPropsMapping.MapFields(['ID','####']); // no ID/RowID for our aggregates
  fPropsMapping.MapFields(TableAggregatePairs);
  fAggregateRTTI := TSQLPropInfoList.Create(aAggregate, GetAggregateRTTIOptions);
  SetLength(fAggregateToTable,fAggregateRTTI.Count);
  SetLength(fAggregateProp,fAggregateRTTI.Count);
  ComputeMapping;
  {$ifdef WITHLOG}
  Rest.LogClass.Add.Log(sllDDDInfo,'Started % implementing % for % over %',
    [self,fInterface.InterfaceName,aAggregate,fTable],self);
  {$endif}
end;

constructor TDDDRepositoryRestFactory.Create(const aInterface: TGUID;
  aImplementation: TDDDRepositoryRestClass; aAggregate: TClass;
  aRest: TSQLRest; aTable: TSQLRecordClass;
  aOwner: TDDDRepositoryRestManager);
begin
  Create(aInterface,aImplementation,aAggregate,aRest,aTable,[],aOwner);
end;

function TDDDRepositoryRestFactory.GetAggregateRTTIOptions: TSQLPropInfoListOptions;
begin
  Result := [pilAllowIDFields,pilSubClassesFlattening,pilIgnoreIfGetter];
end;

destructor TDDDRepositoryRestFactory.Destroy;
begin
  {$ifdef WITHLOG}
  Rest.LogClass.Add.Log(sllDDDInfo,'Destroying %',[self],self);
  {$endif}
  fAggregateRTTI.Free;
  ObjArrayClear(fGarbageCollector);
  inherited;
end;

class procedure TDDDRepositoryRestFactory.ComputeSQLRecord(
  const aAggregate: array of TClass; DestinationSourceCodeFile: TFileName);
const RAW_TYPE: array[TSQLFieldType] of RawUTF8 = (
    // values left to '' will use the RTTI type
    '',                // sftUnknown
    'RawUTF8',         // sftAnsiText
    'RawUTF8',         // sftUTF8Text
    '',                // sftEnumerate
    '',                // sftSet
    '',                // sftInteger
    '',                // sftID = TSQLRecord(aID)
    'TRecordReference',// sftRecord = TRecordReference
    'boolean',         // sftBoolean
    'double',          // sftFloat
    'TDateTime',       // sftDateTime
    'TTimeLog',        // sftTimeLog
    'currency',        // sftCurrency
    '',                // sftObject
    'variant',         // sftVariant
    '',                // sftNullable
    'TSQLRawBlob',     // sftBlob
    'variant',         // sftBlobDynArray T*ObjArray=JSON=variant (RawUTF8?)
    '',                // sftBlobCustom
    'variant',         // sftUTF8Custom
    '',                // sftMany
    'TModTime',        // sftModTime
    'TCreateTime',     // sftCreateTime
    '',                // sftTID
    'TRecordVersion',  // sftRecordVersion = TRecordVersion
    'TSessionUserID',  // sftSessionUserID
    '',                // sftDateTimeMS
    '',                // sftUnixTime
    '');               // sftUnixMSTime
var hier: TClassDynArray;
    a,i,f: integer;
    code,aggname,recname,parentrecname,typ: RawUTF8;
    map: TSQLPropInfoList;
    rectypes: TRawUTF8DynArray;
begin
  {$ifdef KYLIX3} hier := nil; {$endif to make compiler happy}
  if DestinationSourceCodeFile='' then
    DestinationSourceCodeFile := ExeVersion.ProgramFilePath+'ddsqlrecord.inc';
  for a := 0 to high(aAggregate) do begin
    hier := ClassHierarchyWithField(aAggregate[a]);
    code := code+#13#10'type';
    parentrecname := 'TSQLRecord';
    for i := 0 to high(hier) do begin
      aggname := RawUTF8(hier[i].ClassName);
      recname := 'TSQLRecord'+copy(aggname,2,100);
      map := TSQLPropInfoList.Create(hier[i],
        [pilSingleHierarchyLevel,pilAllowIDFields,
         pilSubClassesFlattening,pilIgnoreIfGetter]);
      try
        code := FormatUTF8('%'#13#10+
          '  /// ORM class corresponding to % DDD aggregate'#13#10+
          '  % = class(%)'#13#10'  protected'#13#10,
          [code,aggname,recname,parentrecname]);
        SetLength(rectypes,map.count);
        for f := 0 to map.Count-1 do
        with map.List[f] do begin
          rectypes[f] := RAW_TYPE[SQLFieldType];
          if rectypes[f]='' then
            if SQLFieldType=sftInteger then begin
              rectypes[f] := 'Int64';
              if InheritsFrom(TSQLPropInfo) then
                with TSQLPropInfoRTTI(map.List[f]).PropType^ do
                  if (Kind=tkInteger) and (OrdType<>otULong) then
                    rectypes[f] := 'integer'; // cardinal -> Int64
            end else
              rectypes[f] := SQLFieldRTTITypeName;
          code := FormatUTF8('%    f%: %; // %'#13#10,
            [code,Name,rectypes[f],SQLFieldRTTITypeName]);
        end;
        code := code+'  published'#13#10;
        for f := 0 to map.Count-1 do
        with map.List[f] do begin
          typ := SQLFieldRTTITypeName;
          if IdemPropNameU(typ, rectypes[f]) then
            typ := '' else
            typ := ' ('+typ+')';
          code := FormatUTF8('%    /// maps %.%%'#13#10+
            '    property %: % read f% write f%;'#13#10, [code,aggname,
            NameUnflattened,typ,Name,rectypes[f],Name,Name]);
        end;
        code := code+'  end;'#13#10;
      finally
        map.Free;
      end;
      parentrecname := recname;
    end;
  end;
  FileFromString(code,DestinationSourceCodeFile);
end;

procedure TDDDRepositoryRestFactory.ComputeMapping;

  procedure EnsureCompatible(agg,rec: TSQLPropInfo);
  { note about dynamic arrays (e.g. TRawUTF8DynArray or T*ObjArray) published fields:
      TOrder = class(TSynAutoCreateFields)
      published
        property Lines: TOrderLineObjArray
    In all cases, T*ObjArray should be accessible directly, using ObjArray*()
    wrapper functions, and other dynamic arrays too.
    Storage at TSQLRecord level would use JSON format, i.e. a variant in the
    current implementation - you may use a plain RawUTF8 field if the on-the-fly
    conversion to/from TDocVariant appears to be a bottleneck. }
  begin
    if agg.SQLDBFieldType=rec.SQLDBFieldType then
      exit; // very same type at DB level -> OK
    if (agg.SQLFieldType=sftBlobDynArray) and
       (rec.SQLFieldType in [sftVariant,sftUTF8Text]) then
      exit; // allow array <-> JSON/TEXT <-> variant/RawUTF8 marshalling
    raise EDDDRepository.CreateUTF8(self,
      '% types do not match at DB level: %.%:%=% and %.%:%=%',[self,
      Aggregate,agg.Name,agg.SQLFieldRTTITypeName,agg.SQLDBFieldTypeName^,
      fTable,rec.Name,rec.SQLFieldRTTITypeName,rec.SQLDBFieldTypeName^]);
  end;

var i,ndx: integer;
    ORMProps: TSQLPropInfoObjArray;
    agg: TSQLPropInfoRTTI;
begin
  fAggregateID := nil;
  ORMProps := fTable.RecordProps.Fields.List;
  for i := 0 to fAggregateRTTI.Count-1 do begin
    agg := fAggregateRTTI.List[i] as TSQLPropInfoRTTI;
    fAggregateProp[i] := agg;
    ndx := fPropsMapping.ExternalToInternalIndex(agg.Name);
    if ndx=-1 then // ID/RowID mapped with an existing String/Hexa field
      if agg.SQLDBFieldType in [ftInt64,ftUTF8] then begin
        fAggregateID := agg;
        fAggregateToTable[i] := nil;
      end else
        raise EDDDRepository.CreateUTF8(self,'% types error: %.%:%=% and %.RowID',
          [self,Aggregate,agg.Name,agg.SQLFieldRTTITypeName,agg.SQLDBFieldTypeName^,fTable]) else
    if ndx<0 then // e.g. TSynPersistent property flattened in TSQLRecord
      fAggregateToTable[i] := nil else begin
      fAggregateToTable[i] := ORMProps[ndx];
      EnsureCompatible(agg,fAggregateToTable[i]);
    end;
  end;
  fPropsMappingVersion := fPropsMapping.MappingVersion;
end;

procedure TDDDRepositoryRestFactory.AggregatePropToTable(
  aAggregate: TObject; aAggregateProp: TSQLPropInfo;
  aRecord: TSQLRecord; aRecordProp: TSQLPropInfo);

  procedure ProcessID;
  var v: RawUTF8;
      id: TID;
  begin
    fAggregateID.GetValueVar(aAggregate,false,v,nil);
    case fAggregateID.SQLDBFieldType of
    ftInt64:
      SetID(pointer(v),id);
    ftUTF8:
      if not HexDisplayToBin(pointer(v),@id,sizeof(id)) then
        id := 0;
    end;
    aRecord.IDValue := id;
  end;

begin
  if fAggregateID=aAggregateProp then
    ProcessID else
    if aRecordProp<>nil then
      aAggregateProp.CopyProp(aAggregate,aRecordProp,aRecord);
end;

procedure TDDDRepositoryRestFactory.TablePropToAggregate(
  aRecord: TSQLRecord; aRecordProp: TSQLPropInfo; aAggregate: TObject;
  aAggregateProp: TSQLPropInfo);

  procedure ProcessID;
  var v: RawUTF8;
  begin
    case fAggregateID.SQLDBFieldType of
    ftInt64: begin
      Int64ToUtf8(aRecord.IDValue,v);
      fAggregateID.SetValue(aAggregate,pointer(v),false);
    end;
    ftUTF8: begin
      Int64ToHex(aRecord.IDValue,v);
      fAggregateID.SetValue(aAggregate,pointer(v),true);
    end;
    end;
  end;

begin
  if fAggregateID=aAggregateProp then
    ProcessID else
    if aRecordProp=nil then
      aAggregateProp.SetValue(aAggregate,nil,false) else
      aRecordProp.CopyProp(aRecord,aAggregateProp,aAggregate);
end;

function TDDDRepositoryRestFactory.CreateInstance: TInterfacedObject;
begin
  result := TDDDRepositoryRestClass(fImplementation.ItemClass).Create(self);
end;

procedure TDDDRepositoryRestFactory.AggregateClear(aAggregate: TObject);
var i: integer;
begin
  if aAggregate<>nil then
    for i := 0 to high(fAggregateProp) do
      with fAggregateProp[i] do
        SetValue(Flattened(aAggregate),nil,false);
end;

function TDDDRepositoryRestFactory.AggregateCreate: TObject;
begin
  result := fAggregate.CreateNew;
end;

procedure TDDDRepositoryRestFactory.AggregateToJSON(aAggregate: TObject;
  W: TJSONSerializer; ORMMappedFields: boolean; aID: TID);
var i: integer;
begin
  if fPropsMapping.MappingVersion<>fPropsMappingVersion then
    ComputeMapping;
  if aAggregate=nil then begin
    W.AddShort('null');
    exit;
  end;
  W.Add('{');
  if aID<>0 then begin
    W.AddShort('"RowID":');
    W.Add(aID);
    W.Add(',');
  end;
  for i := 0 to high(fAggregateProp) do begin
    if ORMMappedFields then
      if fAggregateToTable[i]=nil then
        continue else
        W.AddFieldName(fAggregateToTable[i].Name) else
        W.AddFieldName(fAggregateProp[i].Name);
    with fAggregateProp[i] do
      GetJSONValues(Flattened(aAggregate),W);
    W.Add(',');
  end;
  W.CancelLastComma;
  W.Add('}');
end;

function TDDDRepositoryRestFactory.AggregateToJSON(
  aAggregate: TObject; ORMMappedFields: boolean; aID: TID): RawUTF8;
var W: TJSONSerializer;
begin
  if aAggregate=nil then begin
    result := 'null';
    exit;
  end;
  W := TJSONSerializer.CreateOwnedStream;
  try
    AggregateToJSON(aAggregate,W,ORMMappedFields,aID);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure TDDDRepositoryRestFactory.AggregateToTable(
  aAggregate: TObject; aID: TID; aDest: TSQLRecord);
var i: integer;
begin
  if fPropsMapping.MappingVersion<>fPropsMappingVersion then
    ComputeMapping;
  if aDest=nil then
    raise EDDDRepository.CreateUTF8(self,'%.AggregateToTable(%,%,%=nil)',
      [self,aAggregate,aID,fTable]);
  aDest.ClearProperties;
  aDest.IDValue := aID;
  if aAggregate<>nil then
    for i := 0 to length(fAggregateProp)-1 do
      AggregatePropToTable(aAggregate,fAggregateProp[i],aDest,fAggregateToTable[i]);
end;

procedure TDDDRepositoryRestFactory.AggregateFromTable(
  aSource: TSQLRecord; aAggregate: TObject);
var i: integer;
begin
  if fPropsMapping.MappingVersion<>fPropsMappingVersion then
    ComputeMapping;
  if aAggregate=nil then
    raise EDDDRepository.CreateUTF8(self,'%.AggregateFromTable(%=nil)',[self,Aggregate]);
  if aSource=nil then
    AggregateClear(aAggregate) else
    for i := 0 to length(fAggregateProp)-1 do
      TablePropToAggregate(aSource,fAggregateToTable[i],aAggregate,fAggregateProp[i]);
end;

procedure TDDDRepositoryRestFactory.AggregatesFromTableFill(
  aSource: TSQLRecord; var aAggregateObjArray);
var res: TObjectDynArray absolute aAggregateObjArray;
    i: integer;
begin
  SetLength(res, aSource.FillTable.RowCount);
  i := 0;
  if aSource.FillRewind then
    while aSource.FillOne do begin
      res[i] := fAggregate.CreateNew;
      AggregateFromTable(aSource,res[i]);
      inc(i);
    end;
  if i <> length(res) then
    ObjArrayClear(res);
end;

function TDDDRepositoryRestFactory.GetAggregateName: string;
begin
  if (self=nil) or (Aggregate=nil) then
    result := '' else
    result := string(Aggregate.ClassName);
end;

function TDDDRepositoryRestFactory.GetTableName: string;
begin
  if (self=nil) or (fTable=nil) then
    result := '' else
    result := fTable.ClassName;
end;

procedure TDDDRepositoryRestFactory.AddFilterOrValidate(
  const aFieldNames: array of RawUTF8; aFilterOrValidate: TSynFilterOrValidate;
  aFieldNameFlattened: boolean);
var f,ndx: integer;
    arr: ^TPointerDynArray;
begin
  if aFilterOrValidate=nil then
    exit;
  ObjArrayAdd(fGarbageCollector,aFilterOrValidate);
  for f := 0 to high(aFieldNames) do begin
    if aFilterOrValidate.InheritsFrom(TSynValidate) then
      arr := @fValidate else
      arr := @fFilter;
    if arr^=nil then
      SetLength(arr^,fAggregateRTTI.Count);
    if aFieldNames[f]='*' then begin // apply to all text fields
      for ndx := 0 to high(fAggregateProp) do
        if fAggregateProp[ndx].SQLFieldType in RAWTEXT_FIELDS then
          aFilterOrValidate.AddOnce(TSynFilterOrValidateObjArray(arr^[ndx]),false);
    end else begin
      if aFieldNameFlattened then
        ndx := fAggregateRTTI.IndexByNameUnflattenedOrExcept(aFieldNames[f]) else
        ndx := fAggregateRTTI.IndexByNameOrExcept(aFieldNames[f]);
      aFilterOrValidate.AddOnce(TSynFilterOrValidateObjArray(arr^[ndx]),false);
    end;
  end;
end;

function TDDDRepositoryRestFactory.AggregateFilterAndValidate(
  aAggregate: TObject; aInvalidFieldIndex: PInteger; aValidator: PSynValidate): RawUTF8;
var f,i: integer;
    Value: TRawUTF8DynArray; // avoid twice retrieval
    Old: RawUTF8;
    msg: string;
    str: boolean;
begin
  if (aAggregate=nil) or not aAggregate.ClassType.InheritsFrom(Aggregate) then
    raise EDDDRepository.CreateUTF8(self,'%.AggregateFilterAndValidate(%) '+
      'expects a % instance',[self,aAggregate,Aggregate]);
  // first process all filters
  SetLength(Value,fAggregateRTTI.Count);
  for f := 0 to high(fFilter) do
    if fFilter[f]<>nil then begin
      with fAggregateProp[f] do
        GetValueVar(Flattened(aAggregate),false,Value[f],@str);
      Old := Value[f];
      for i := 0 to high(fFilter[f]) do
        fFilter[f,i].Process(f,Value[f]);
      if Old<>Value[f] then
        with fAggregateProp[f] do
          SetValueVar(Flattened(aAggregate),Value[f],str);
    end;
  // then validate the content
  for f := 0 to high(fValidate) do
    if fValidate[f]<>nil then begin
      if Value[f]='' then // if not already retrieved
        with fAggregateProp[f] do
          GetValueVar(Flattened(aAggregate),false,Value[f],nil);
      for i := 0 to high(fValidate[f]) do
        if not fValidate[f,i].Process(f,Value[f],msg) then begin
          if aInvalidFieldIndex<>nil then
            aInvalidFieldIndex^ := f;
          if aValidator<>nil then
            aValidator^ := fValidate[f,i];
          if msg='' then
            // no custom message -> show a default message
            msg := format(sValidationFailed,[GetCaptionFromClass(fValidate[f,i].ClassType)]);
          result := FormatUTF8('%.%: %',[Aggregate,fAggregateProp[f].NameUnflattened,msg]);
          exit;
        end;
    end;
  result := ''; // if we reached here, there was no error
end;



{ TDDDRepositoryRestManager }

function TDDDRepositoryRestManager.AddFactory(
  const aInterface: TGUID; aImplementation: TDDDRepositoryRestClass;
  aAggregate: TClass; aRest: TSQLRest; aTable: TSQLRecordClass;
  const TableAggregatePairs: array of RawUTF8): TDDDRepositoryRestFactory;
begin
  if GetFactoryIndex(aInterface)>=0 then
    raise EDDDRepository.CreateUTF8(nil,'Duplicated GUID for %.AddFactory(%,%,%)',
      [self,GUIDToShort(aInterface),aImplementation,aAggregate]);
  result := TDDDRepositoryRestFactory.Create(
    aInterface,aImplementation,aAggregate,aRest,aTable,TableAggregatePairs,self);
  ObjArrayAdd(fFactory,result);
  {$ifdef WITHLOG}
  aRest.LogClass.Add.Log(sllDDDInfo,'Added factory % to %',[result,self],self);
  {$endif}
end;

destructor TDDDRepositoryRestManager.Destroy;
begin
  ObjArrayClear(fFactory);
  inherited;
end;

function TDDDRepositoryRestManager.GetFactory(
  const aInterface: TGUID): TDDDRepositoryRestFactory;
var i: integer;
begin
  i := GetFactoryIndex(aInterface);
  if i<0 then
    raise EDDDRepository.CreateUTF8(nil,'%.GetFactory(%)=nil',
      [self,GUIDToShort(aInterface)]);
  result := fFactory[i];
end;

function TDDDRepositoryRestManager.GetFactoryIndex(
  const aInterface: TGUID): integer;
begin
  for result := 0 to length(fFactory)-1 do
    if IsEqualGUID(@fFactory[result].fInterface.InterfaceIID,@aInterface) then
      exit;
  result := -1;
end;


{ TDDDRepositoryRestQuery }

constructor TDDDRepositoryRestQuery.Create(
  aFactory: TDDDRepositoryRestFactory);
begin
  fFactory := aFactory;
  fCurrentORMInstance := fFactory.Table.Create;
  {$ifdef WITHLOG}
  fLog := fFactory.Rest.LogFamily;
  {$endif}
end;

destructor TDDDRepositoryRestQuery.Destroy;
begin
  fCurrentORMInstance.Free;
  inherited;
end;

class function TDDDRepositoryRestQuery.GetRest(const Service: ICQRSService): TSQLRest;
var instance: TObject;
begin
  instance := ObjectFromInterface(Service);
  if (instance=nil) or not instance.InheritsFrom(TDDDRepositoryRestQuery) then
    result := nil else
    result := TDDDRepositoryRestQuery(instance).fFactory.fRest;
end;

function TDDDRepositoryRestQuery.CqrsBeginMethod(aAction: TCQRSQueryAction;
  var aResult: TCQRSResult; aError: TCQRSResult): boolean;
begin
  result := inherited CqrsBeginMethod(aAction,aResult,aError);
  if aAction=qaSelect then
    fCurrentORMInstance.ClearProperties; // reset internal instance
end;

function TDDDRepositoryRestQuery.ORMSelectOne(const ORMWhereClauseFmt: RawUTF8;
  const Bounds: array of const; ForcedBadRequest: boolean): TCQRSResult;
begin
  CqrsBeginMethod(qaSelect,result);
  if ForcedBadRequest then
    CqrsSetResult(cqrsBadRequest,result) else
    CqrsSetResultSuccessIf(Factory.Rest.Retrieve(ORMWhereClauseFmt,[],Bounds,
      fCurrentORMInstance),result,cqrsNotFound);
end;

function TDDDRepositoryRestQuery.ORMSelectID(const ID: TID;
  RetrieveRecord, ForcedBadRequest: boolean): TCQRSResult;
begin
  CqrsBeginMethod(qaSelect,result);
  if ForcedBadRequest or (ID=0) then
    CqrsSetResult(cqrsBadRequest,result) else
  if RetrieveRecord then
    CqrsSetResultSuccessIf(Factory.Rest.Retrieve(ID,fCurrentORMInstance),result,cqrsNotFound)
  else begin
    fCurrentORMInstance.IDValue := ID;
    CqrsSetResult(cqrsSuccess,result);
  end
end;

function TDDDRepositoryRestQuery.ORMSelectID(const ID: RawUTF8;
  RetrieveRecord, ForcedBadRequest: boolean): TCQRSResult;
begin
  result := ORMSelectID(HexDisplayToInt64(ID),RetrieveRecord,ForcedBadRequest);
end;

function TDDDRepositoryRestQuery.ORMSelectAll(
  const ORMWhereClauseFmt: RawUTF8; const Bounds: array of const;
  ForcedBadRequest: boolean): TCQRSResult;
begin
  CqrsBeginMethod(qaSelect,result);
  if ForcedBadRequest then
    CqrsSetResult(cqrsBadRequest,result) else
    CqrsSetResultSuccessIf(fCurrentORMInstance.FillPrepare(
      Factory.Rest,ORMWhereClauseFmt,[],Bounds),result,cqrsNotFound);
end;

function TDDDRepositoryRestQuery.ORMSelectCount(
  const ORMWhereClauseFmt: RawUTF8; const Args,Bounds: array of const;
  out aResultCount: integer; ForcedBadRequest: boolean): TCQRSResult;
var tmp: Int64;
begin
  CqrsBeginMethod(qaNone,result); // qaNone and not qaSelect which would fill ORM
  if ForcedBadRequest then
    CqrsSetResult(cqrsBadRequest,result) else
    if Factory.Rest.OneFieldValue(
        Factory.Table,'count(*)',ORMWhereClauseFmt,Args,Bounds,tmp) then begin
       aResultCount := tmp;
       CqrsSetResult(cqrsSuccess,result)
    end else
      CqrsSetResult(cqrsNotFound,result);
end;

function TDDDRepositoryRestQuery.GetCount: integer;
var dummy: TCQRSResult;
begin
  if not CqrsBeginMethod(qaGet,dummy) then
    result := 0 else
    if fCurrentORMInstance.FillTable<>nil then
      result := fCurrentORMInstance.FillTable.RowCount else
      if fCurrentORMInstance.IDValue=0 then
        result := 0 else
        result := 1;
end;

function TDDDRepositoryRestQuery.ORMGetAggregate(
  aAggregate: TObject): TCQRSResult;
begin
  if CqrsBeginMethod(qaGet,result) then begin
    Factory.AggregateFromTable(fCurrentORMInstance,aAggregate);
    CqrsSetResult(cqrsSuccess,result);
  end;
end;

function TDDDRepositoryRestQuery.ORMGetNextAggregate(
  aAggregate: TObject; aRewind: boolean): TCQRSResult;
begin
  if CqrsBeginMethod(qaGet,result) then
    if (aRewind and fCurrentORMInstance.FillRewind) or
       (not aRewind and fCurrentORMInstance.FillOne) then begin
      Factory.AggregateFromTable(fCurrentORMInstance,aAggregate);
      CqrsSetResult(cqrsSuccess,result);
    end else
      CqrsSetResult(cqrsNoMoreData,result);
end;

function TDDDRepositoryRestQuery.ORMGetAllAggregates(
  var aAggregateObjArray): TCQRSResult;
begin
  if CqrsBeginMethod(qaGet,result) then
  if (fCurrentORMInstance.FillTable=nil) or
     (fCurrentORMInstance.FillTable.RowCount=0) then
    CqrsSetResult(cqrsSuccess,result) else begin
    Factory.AggregatesFromTableFill(fCurrentORMInstance,aAggregateObjArray);
    if Pointer(aAggregateObjArray)=nil then
      CqrsSetResult(cqrsNoMoreData,result) else
      CqrsSetResult(cqrsSuccess,result);
  end;
end;



{ TDDDRepositoryRestCommand }

constructor TDDDRepositoryRestCommand.Create(
  aFactory: TDDDRepositoryRestFactory);
begin
  inherited Create(aFactory);
  fBatchAutomaticTransactionPerRow := 1000; // for better performance
  fBatchOptions := [boExtendedJSON];
end;

destructor TDDDRepositoryRestCommand.Destroy;
begin
  InternalRollback;
  inherited Destroy;
end;

function TDDDRepositoryRestCommand.Delete: TCQRSResult;
begin
  if CqrsBeginMethod(qaCommandOnSelect,result) then
    ORMPrepareForCommit(soDelete,nil,result);
end;

function TDDDRepositoryRestCommand.DeleteAll: TCQRSResult;
var i: integer;
begin
  if CqrsBeginMethod(qaCommandOnSelect,result) then
    if fCurrentORMInstance.FillTable=nil then
      ORMPrepareForCommit(soDelete,nil,result) else
      if fState<qsQuery then
        CqrsSetResult(cqrsNoPriorQuery,result) else begin
        ORMEnsureBatchExists;
        for i := 1 to fCurrentORMInstance.FillTable.RowCount do
          if fBatch.Delete(fCurrentORMInstance.FillTable.IDColumnHiddenValue(i))<0 then begin
            CqrsSetResult(cqrsDataLayerError,result);
            exit;
          end;
        CqrsSetResult(cqrsSuccess,result);
      end;
end;

function TDDDRepositoryRestCommand.ORMAdd(aAggregate: TObject;
  aAllFields: boolean): TCQRSResult;
begin
  if CqrsBeginMethod(qaCommandDirect,result) then
    ORMPrepareForCommit(soInsert,aAggregate,result,aAllFields);
end;

function TDDDRepositoryRestCommand.ORMUpdate(aAggregate: TObject;
  aAllFields: boolean): TCQRSResult;
begin
  if CqrsBeginMethod(qaCommandOnSelect,result) then
    ORMPrepareForCommit(soUpdate,aAggregate,result,aAllFields);
end;

procedure TDDDRepositoryRestCommand.ORMEnsureBatchExists;
begin
  if fBatch=nil then
    fBatch := TSQLRestBatch.Create(Factory.Rest,Factory.Table,
      fBatchAutomaticTransactionPerRow,fBatchOptions);
end;

procedure TDDDRepositoryRestCommand.ORMPrepareForCommit(
  aCommand: TSQLOccasion; aAggregate: TObject; var Result: TCQRSResult;
  aAllFields: boolean);
var msg: RawUTF8;
    validator: TSynValidate;
    ndx: integer;
    fields: TSQLFieldBits;

  procedure SetValidationError(default: TCQRSResult);
  begin
    if (validator<>nil) and
       (validator.ClassType=TSynValidateUniqueField) then
      CqrsSetResultMsg(cqrsAlreadyExists,msg,Result) else
      CqrsSetResultMsg(default,msg,Result);
  end;

begin
  case aCommand of
  soSelect: begin
    CqrsSetResult(cqrsBadRequest,Result);
    exit;
  end;
  soUpdate,soDelete:
    if (fState<qsQuery) or (fCurrentORMInstance.IDValue=0) then begin
      CqrsSetResult(cqrsNoPriorQuery,Result);
      exit;
    end;
  end;
  if aCommand in [soUpdate,soInsert] then begin
    if aAggregate<>nil then begin
      msg := Factory.AggregateFilterAndValidate(aAggregate,nil,@validator);
      if msg<>'' then begin
        SetValidationError(cqrsDDDValidationFailed);
        exit;
      end;
      Factory.AggregateToTable(aAggregate,fCurrentORMInstance.IDValue,fCurrentORMInstance);
    end;
    msg := fCurrentORMInstance.FilterAndValidate(
      Factory.Rest,[0..MAX_SQLFIELDS-1],@validator);
    if msg<>'' then begin
      SetValidationError(cqrsDataLayerError);
      exit;
    end;
  end;
  ORMEnsureBatchExists;
  ndx := -1;
  if aAllFields then
    fields := ALL_FIELDS else
    fields := [];
  case aCommand of
  soInsert: ndx := fBatch.Add(fCurrentORMInstance,true,fFactory.fAggregateID<>nil,fields);
  soUpdate: ndx := fBatch.Update(fCurrentORMInstance,fields);
  soDelete: ndx := fBatch.Delete(fCurrentORMInstance.IDValue);
  end;
  CqrsSetResultSuccessIf(ndx>=0,Result);
end;

procedure TDDDRepositoryRestCommand.InternalCommit(var Result: TCQRSResult);
begin
  if fBatch.Count=0 then
    CqrsSetResult(cqrsBadRequest,Result) else begin
    CqrsSetResultSuccessIf(Factory.Rest.BatchSend(fBatch,fBatchResults)=HTTP_SUCCESS,Result);
    FreeAndNil(fBatch);
  end;
end;

procedure TDDDRepositoryRestCommand.InternalRollback;
begin
  FreeAndNil(fBatch);
  fBatchResults := nil;
end;

function TDDDRepositoryRestCommand.Commit: TCQRSResult;
begin
  if CqrsBeginMethod(qaCommit,result) then
    InternalCommit(result);
end;

function TDDDRepositoryRestCommand.Rollback: TCQRSResult;
begin
  CqrsBeginMethod(qaNone,result,cqrsSuccess);
  if fBatch.Count=0 then
    CqrsSetResult(cqrsNoPriorCommand,result) else
    InternalRollback;
end;


{ TCQRSQueryObjectRest }

constructor TCQRSQueryObjectRest.Create(aRest: TSQLRest);
begin
  fRest := aRest;
  if (fResolver<>nil) or (aRest=nil) or (aRest.Services=nil) then
    inherited Create else
    inherited CreateWithResolver(aRest.Services);
end;

constructor TCQRSQueryObjectRest.CreateInjected(aRest: TSQLRest;
  const aStubsByGUID: array of TGUID;
  const aOtherResolvers: array of TInterfaceResolver;
  const aDependencies: array of TInterfacedObject);
begin
  if not Assigned(aRest) then
    raise ECQRSException.CreateUTF8('%.CreateInjected(Rest=nil)',[self]);
  Create(aRest);
  inherited CreateInjected(aStubsByGUID,aOtherResolvers,
    aDependencies,true);
end;

constructor TCQRSQueryObjectRest.CreateWithResolver(aRest: TSQLRest;
  aResolver: TInterfaceResolver; aRaiseEServiceExceptionIfNotFound: boolean);
begin
  if not Assigned(aRest) then
    raise ECQRSException.CreateUTF8('%.CreateWithResolver(Rest=nil)',[self]);
  fRest := aRest;
  inherited CreateWithResolver(aResolver,aRaiseEServiceExceptionIfNotFound);
end;

constructor TCQRSQueryObjectRest.CreateWithResolver(
  aResolver: TInterfaceResolver; aRaiseEServiceExceptionIfNotFound: boolean);
begin
  if (aResolver<>nil) and aResolver.InheritsFrom(TServiceContainer) then
    fRest := TServiceContainer(aResolver).Rest;
  inherited CreateWithResolver(aResolver,aRaiseEServiceExceptionIfNotFound);
end;


{ TDDDMonitoredDaemonProcess }

constructor TDDDMonitoredDaemonProcess.Create(aDaemon: TDDDMonitoredDaemon;
  aIndexInDaemon: integer);
begin
  fDaemon := aDaemon;
  if fDaemon.fProcessMonitoringClass=nil then
    fMonitoring := TSynMonitorWithSize.Create(aDaemon.Rest.Model.Root) else
    fMonitoring := fDaemon.fProcessMonitoringClass.Create(aDaemon.Rest.Model.Root)
      as TSynMonitorWithSize;
  fProcessIdleDelay := fDaemon.ProcessIdleDelay;
  fIndex := aIndexInDaemon;
  inherited Create(False);
end;

destructor TDDDMonitoredDaemonProcess.Destroy;
begin
  fMonitoring.Free;
  inherited;
end;

procedure TDDDMonitoredDaemonProcess.Execute;
begin
  fDaemon.Rest.BeginCurrentThread(self);
  try
    repeat
      SleepHiRes(fProcessIdleDelay);
      try
        try
          repeat
            if Terminated then
              exit;
            try
              try
                fDaemon.fProcessLock.Enter; // atomic unqueue via pending.Status
                try
                  fMonitoring.ProcessStart;
                  if not ExecuteRetrievePendingAndSetProcessing then
                    break; // no more pending tasks
                  fMonitoring.ProcessDoTask;
                finally
                  fDaemon.fProcessLock.Leave;
                end;
                // always set, even if Terminated
                fMonitoring.AddSize(ExecuteProcessAndSetResult);
              finally
                fMonitoring.ProcessEnd;
                ExecuteProcessFinalize;
              end;
            except
              on E: Exception do begin
                ExecuteOnException(E);
                break; // will call ExecuteIdle then go to idle state
              end;
            end;
          until false;
        finally
          ExecuteIdle;
        end;
      except
        on E: Exception do begin
          ExecuteOnException(E); // exception during ExecuteIdle should not happen
          if fProcessIdleDelay<500 then
            fProcessIdleDelay := 500; // avoid CPU+resource burning
        end;
      end;
    until false;
  finally
    fDaemon.Rest.EndCurrentThread(self);
  end;
end;

procedure TDDDMonitoredDaemonProcess.ExecuteOnException(E: Exception);
begin
  fMonitoring.ProcessError(ObjectToVariantDebug(
    E,'{threadindex:?,daemon:?}',[fIndex,fDaemon.GetStatus]));
end;


{ TDDDMonitoredDaemonProcessRest }

procedure TDDDMonitoredDaemonProcessRest.ExecuteProcessFinalize;
begin
  FreeAndNil(fPendingTask);
end;


{ TDDDMonitoredDaemon }

constructor TDDDMonitoredDaemon.Create(aRest: TSQLRest);
begin
  fProcessIdleDelay := 50;
  fProcessLock := TAutoLocker.Create;
  if fProcessThreadCount<1 then
    fProcessThreadCount := 1 else
  if fProcessThreadCount>20 then
    fProcessThreadCount := 20;
  inherited Create(aRest);
end;

constructor TDDDMonitoredDaemon.Create(aRest: TSQLRest;
  aProcessThreadCount: integer);
begin
  fProcessThreadCount := aProcessThreadCount;
  Create(aRest);
end;

destructor TDDDMonitoredDaemon.Destroy;
var dummy: variant;
begin
  Stop(dummy);
  inherited Destroy;
end;

function TDDDMonitoredDaemon.GetStatus: variant;
var i,working: integer;
    stats: TSynMonitor;
    pool: TDocVariantData;
begin
  {$ifdef WITHLOG}
  Rest.LogClass.Enter('GetStatus',[],self);
  {$endif}
  VarClear(result);
  fProcessLock.Enter;
  try
    try
      working := 0;
      if fMonitoringClass=nil then
        if fProcessMonitoringClass=nil then
          stats := TSynMonitorWithSize.Create else
          stats := fProcessMonitoringClass.Create else
        stats := fMonitoringClass.Create;
      try
        pool.InitArray([],JSON_OPTIONS[true]);
        for i := 0 to High(fProcess) do
        with fProcess[i] do begin

          if fMonitoring.Processing then
            inc(working);
          pool.AddItem(fMonitoring.ComputeDetails);
          stats.Sum(fMonitoring);
        end;
        result := ObjectToVariantDebug(self);
        _ObjAddProps(['working',working, 'stats',stats.ComputeDetails,
          'threadstats',variant(pool)],result);
      finally
        stats.Free;
      end;
    except
      on E: Exception do
        result := ObjectToVariantDebug(E);
    end;
  finally
    fProcessLock.Leave;
  end;
end;

function TDDDMonitoredDaemon.RetrieveState(
  out Status: variant): TCQRSResult;
begin
  CqrsBeginMethod(qaNone,result,cqrsSuccess);
  Status := GetStatus;
end;

function TDDDMonitoredDaemon.Start: TCQRSResult;
var i: integer;
    {$ifdef WITHLOG}
    Log: ISynLog;
    {$endif}
    dummy: variant;
begin
  {$ifdef WITHLOG}
  Log := Rest.LogClass.Enter('Start %',[fProcessClass],self);
  {$endif}
  if fProcessClass=nil then
    raise EDDDException.CreateUTF8('%.Start with no fProcessClass',[self]);
  Stop(dummy); // ignore any error when stopping
  fProcessTimer.Resume;
  {$ifdef WITHLOG}
  if Log<>nil then
    Log.Log(sllTrace,'Start %',[self],self);
  {$endif}
  CqrsBeginMethod(qaNone,result,cqrsSuccess);
  SetLength(fProcess,fProcessThreadCount);
  for i := 0 to fProcessThreadCount-1 do
    fProcess[i] := fProcessClass.Create(self,i);
  SleepHiRes(1); // some time to actually start the threads
end;


function TDDDMonitoredDaemon.Stop(out Information: variant): TCQRSResult;
var i: integer;
    allfinished: boolean;
begin
  CqrsBeginMethod(qaNone,result);
  try
    if fProcess<>nil then begin
      fProcessTimer.Pause;
      Information := GetStatus;
      {$ifdef WITHLOG}
      Rest.LogClass.Enter('Stop % process %',[fProcessClass,Information],self);
      {$endif}
      fProcessLock.Enter;
      try
        for i := 0 to high(fProcess) do
          fProcess[i].Terminate;
      finally
        fProcessLock.Leave;
      end;
      repeat
        SleepHiRes(5);
        allfinished := true;
        fProcessLock.Enter;
        try
          for i := 0 to high(fProcess) do
            if fProcess[i].fMonitoring.Processing then begin
              allfinished := false;
              break;
            end;
        finally
          fProcessLock.Leave;
        end;
      until allfinished;
      fProcessLock.Enter;
      try
        ObjArrayClear(fProcess);
      finally
        fProcessLock.Leave;
      end;
    end;
    CqrsSetResult(cqrsSuccess,result);
  except
    on E: Exception do
      CqrsSetResult(E,result);
  end;
end;


{ TDDDAdministratedDaemon }

constructor TDDDAdministratedDaemon.Create(
  aAdministrationServer: TSQLRestServer);
begin
  if aAdministrationServer=nil then
    raise ECQRSException.CreateUTF8('%.Create(aAdministrationServer=nil)',[self]);
  fAdministrationServer := aAdministrationServer;
  {$ifdef WITHLOG}
  fLog := fAdministrationServer.LogFamily;
  {$endif}
  CreateWithResolver(fAdministrationServer.ServiceContainer);
  fAdministrationServer.ServiceDefine(self,[IAdministratedDaemon]);
  fFinished := TEvent.Create(nil,false,false,'');
  fStatus := dsCreated;
end;

constructor TDDDAdministratedDaemon.Create(
  const aUserName, aHashedPassword, aRoot: RawUTF8; const aServerNamedPipe: TFileName);
var server: TSQLRestServer;
begin
  server := TSQLRestServerFullMemory.CreateWithOwnedAuthenticatedModel([],
    aUserName,aHashedPassword,aRoot);
  server.Options := server.Options+[rsoSecureConnectionRequired];
  Create(server);
  if FRefCount=2 then
    dec(FRefCount); // circumvent a Delphi compiler bug
  fAdministrationServerOwned := true;
  if aServerNamedPipe<>'' then
    {$ifdef MSWINDOWS}
    fAdministrationServer.ExportServerNamedPipe(aServerNamedPipe);
    {$else}
    {$ifdef WITHLOG}
    fLog.SynLog.Log(sllTrace,'Ignored AuthNamedPipeName=% under Linux',
      [aServerNamedPipe],self);
    {$endif}
    {$endif}
end;

destructor TDDDAdministratedDaemon.Destroy;
var dummy: variant;
{$ifdef WITHLOG}
    log: ISynLog;
begin
  log := fLog.SynLog.Enter(self, 'Destroy');
{$else}
begin
{$endif}
  if InternalIsRunning then
    Halt(dummy);
  try
    FreeAndNil(fAdministrationHTTPServer);
    inherited Destroy;
    fFinished.Free;
    FreeAndNil(fRemoteLog);
  finally
    if fAdministrationServerOwned then
      FreeAndNil(fAdministrationServer);
  end;
end;

function TDDDAdministratedDaemon.Start: TCQRSResult;
{$ifdef WITHLOG}
var log: ISynLog;
begin
  log := fLog.SynLog.Enter(self, 'Start');
{$else}
begin
{$endif}
  CqrsBeginMethod(qaNone,result);
  if not (fStatus in [dsCreated,dsStopped]) then
    CqrsSetResultError(cqrsBadRequest) else
  if InternalIsRunning then
    CqrsSetResult(cqrsAlreadyExists,result) else
    try
      {$ifdef WITHLOG}
      if log<>nil then
        log.Log(sllDDDInfo,'Starting',self);
      {$endif}
      InternalStart;
      fStatus := dsStarted;
      CqrsSetResult(cqrsSuccess,result);
    except
      on E: Exception do
      try
        CqrsSetResult(E,result);
        InternalStop; // automatically release resources on starting error
      except
      end;
    end;
end;

function TDDDAdministratedDaemon.RetrieveState(out Status: variant): TCQRSResult;
begin
  CqrsBeginMethod(qaNone,result);
  try
    if not InternalIsRunning then
      CqrsSetResult(cqrsBadRequest,result) else
    if InternalRetrieveState(Status) then
      CqrsSetResult(cqrsSuccess,result) else
      CqrsSetResult(cqrsInternalError,result);
  except
    on E: Exception do
      CqrsSetResult(E,result);
  end;
end;

function TDDDAdministratedDaemon.Stop(out Information: variant): TCQRSResult;
{$ifdef WITHLOG}
var log: ISynLog;
begin
  log := fLog.SynLog.Enter(self, 'Stop');
{$else}
begin
{$endif}
  CqrsBeginMethod(qaNone,result);
  if fStatus<>dsStarted then
    CqrsSetResultError(cqrsBadRequest) else begin
    if InternalRetrieveState(Information) then
    try
      {$ifdef WITHLOG}
      if log<>nil then
        log.Log(sllDDDInfo,'Stopping %',[Information],self);
      {$endif}
      InternalStop; // always stop
      fStatus := dsStopped;
      {$ifdef WITHLOG}
      if log<>nil then
        log.Log(sllDDDInfo,'Stopped: %',[Information],self);
      {$endif}
      CqrsSetResult(cqrsSuccess,result);
    except
      on E: Exception do
        CqrsSetResult(E,result);
    end else
      CqrsSetResult(cqrsInternalError,result);
  end;
end;

function TDDDAdministratedDaemon.Halt(out Information: variant): TCQRSResult;
{$ifdef WITHLOG}
var log: ISynLog;
begin
  log := fLog.SynLog.Enter(self, 'Halt');
{$else}
begin
{$endif}
  CqrsBeginMethod(qaNone,result);
  if InternalIsRunning then
  try
    {$ifdef WITHLOG}
    if log<>nil then
      log.Log(sllDDDInfo,'Halting',self);
    {$endif}
    CqrsSetResult(Stop(Information),result);
  except
    on E: Exception do
      CqrsSetResult(E,result);
  end else
    CqrsSetResult(cqrsSuccess,result);
  fStatus := dsHalted;
  fFinished.SetEvent;
end;

procedure TDDDAdministratedDaemon.WaitUntilHalted;
begin
  if fStatus in [dsUnknown] then
    exit;
  {$ifdef WITHLOG}
  with fLog.SynLog.Enter(self, 'WaitUntilHalted') do
  {$endif}
  FixedWaitForever(fFinished);
end;

procedure TDDDAdministratedDaemon.Execute(RemotelyAdministrated: boolean);
var name: string;
    msg: RawUTF8;
{$ifdef WITHLOG}
    log: ISynLog;
begin
  log := fLog.SynLog.Enter(self, 'Execute');
{$else}
begin
{$endif}
  name := ClassName;
  {$I-}
  if RemotelyAdministrated then begin
    writeln(name,' expecting commands');
    WaitUntilHalted;
  end else begin
    if Start=cqrsSuccess then
      writeln(name,' is running') else begin
      writeln(#10,name,' failed to Start');
      if _Safe(LastErrorInfo)^.GetAsRawUTF8('Exception',msg) then begin
        TextColor(ccLightMagenta);
        writeln(msg);
        TextColor(ccLightGray);
      end;
    end;
    writeln('Press [Enter] to quit');
    ioresult;
    readln;
    {$ifdef LINUX}
    if ioresult<>0 then // e.g. when redirected from "nohup daemon &" command
      WaitUntilHalted;
    {$endif}
  end;
  writeln('Shutting down server');
  ioresult;
  {$I+}
end;

procedure TDDDAdministratedDaemon.CallbackReleased(
  const callback: IInvokable; const interfaceName: RawUTF8);
begin
  if IdemPropNameU(interfaceName,'ISynLogCallback') and (fRemoteLog<>nil) then
    fRemoteLog.Unsubscribe(ISynLogCallback(callback));
end;

procedure TDDDAdministratedDaemon.SubscribeLog(const Levels: TSynLogInfos;
  const Callback: ISynLogCallback; ReceiveExistingKB: cardinal);
{$ifdef WITHLOG}
var previousContentSize: integer;
begin
  if fRemoteLog=nil then
    fRemoteLog := TSynLogCallbacks.Create(fLog);
  previousContentSize := fRemoteLog.Subscribe(Levels,Callback,ReceiveExistingKB);
  fLog.SynLog.Log(sllTrace,'SubscribeLog sent % bytes as previous content',
    [previousContentSize],self);
{$else}
begin
{$endif}
end;

function TDDDAdministratedDaemon.PublishedORM(const DatabaseName: RawUTF8): TSQLRest;
var i: integer;
begin
  for i := 0 to high(fInternalDatabases) do begin
    result := fInternalDatabases[i];
    if IdemPropNameU(result.Model.Root,DatabaseName) then
      exit;
  end;
  result := nil;
end;

function TDDDAdministratedDaemon.DatabaseExecute(const DatabaseName,SQL: RawUTF8): TServiceCustomAnswer;
var rest: TSQLRest;
    name,value: RawUTF8;
    doc: TDocVariantData;
    valid: Boolean;
    status: variant;
    res: TCQRSResult;
    cmd: integer;
    store: IDDDSettingsStorable;
begin
  result.Header := JSON_CONTENT_TYPE_HEADER_VAR;
  result.Status := HTTP_SUCCESS;
  if SQL='' then
    exit;
  if SQL[1]='#' then begin
    cmd := IdemPCharArray(@SQL[2],['STATE','SETTINGS','VERSION','COMPUTER','LOG',
      'CHAT','STARTDAEMON','STOPDAEMON','RESTARTDAEMON','HELP','INFO']);
    case cmd of
    0: begin
      if InternalRetrieveState(status) then
        result.Content := VariantSaveJSON(status) else
        result.Content := '"Daemon seems stopped"';
      exit;
    end;
    1: if fInternalSettings<>nil then begin
        if SQL[10]=' ' then begin
          name := copy(SQL,11,maxInt);
          if PosExChar('=',name)>0 then begin
            Split(name,'=',name,value);
            if (name<>'') and (value<>'') then begin
              VariantLoadJSON(status,pointer(value));
              doc.InitObjectFromPath(name,status);
              JsonToObject(fInternalSettings,pointer(doc.ToJSON),valid);
            end;
          end else
          if IdemPropNameU(name,'save') then begin
            if fInternalSettings.GetInterface(IDDDSettingsStorable,store) then begin
              store.StoreIfUpdated;
              result.Content := FormatUTF8('"% saved"',[fInternalSettings.ClassType]);
            end else
              result.Content := FormatUTF8('"% does not implement IDDDSettingsStorable"',
                [fInternalSettings.ClassType]);
            exit;
          end else
          if fInternalSettingsFolder<>'' then begin
            AdministrationExecuteGetFiles(fInternalSettingsFolder,
              '*.config;*.settings',name,result);
            exit;
          end;
        end;
        result.Content := ObjectToJSON(fInternalSettings,[woEnumSetsAsText]);
        exit;
      end;
    2:
      result.Content := JSONEncode(['daemon',DaemonName,
        'exe',ExeVersion.ProgramFileName,'version',ExeVersion.Version.Detailed,
        'buildTime',DateTimeToIso8601(ExeVersion.Version.BuildDateTime,true),
        'framework',SYNOPSE_FRAMEWORK_FULLVERSION,'compiler',GetDelphiCompilerVersion]);
      // no exit: #version handled e.g. in TSQLRestServerDB.AdministrationExecute
    3: begin
      {$ifdef MSWINDOWS_INCLUDESENV}
      P := pointer(GetEnvironmentStringsA);
      while P^<>#0 do begin
        L := StrLen(P);
        if (L>0) and (P^<>'=') then begin
          if value<>'' then
            value := value+#10;
          SetString(name,PAnsiChar(P),L);
          value := value+name;
        end;
        inc(P,table+1);
      end;
      {$endif}
      result.Content := SystemInfoJson;
      {$ifdef MSWINDOWS}
      result.Content[length(result.Content)] := ',';
      result.Content := result.Content+'"ip":"'+GetIPAddressesText+'"}';
      {$endif}
      exit;
    end;
    {$ifdef WITHLOG}
    4: begin
      split(SQL,' ',name,value);
      if value='' then
        result.Content := ObjectToJSON(fLog.SynLog,[woEnumSetsAsText]) else
        AdministrationExecuteGetFiles(fLog.DestinationPath,'*.log;*.synlz',value,result);
      exit;
    end;
    5: begin
      fLog.SynLog.Log(sllMonitoring,'[CHAT] % %',
         [ServiceContext.Request.InHeader['remoteip'],copy(SQL,7,maxInt)]);
      exit;
    end;
    {$endif}
    6,7,8: begin // 6=start/7=stop/8=restart
      if cmd=6 then
        res := cqrsSuccess else
        res := Stop(status);
      if res=cqrsSuccess then begin
        if cmd=8 then
          SleepHiRes(200); // leave some time between stop and start
        if cmd<>7 then
          res := Start;
        if res=cqrsSuccess then
         result.Content := VariantSaveJSON(status) else
         result.Content := JSONEncode(['errorStart',ToText(res)^,
           'stopStatus',status]);
       end else
         result.Content := JSONEncode(['errorStop',ToText(res)^]);
       exit;
    end;
    9:
      result.Content := '"Enter either a SQL request, or one of the following commands:|'+
        '|#state|#settings [full.path=value/*/save/filename]|#version|#computer|#log [*/filename]|'+
        '#startdaemon|#stopdaemon|#restartdaemon|#help"';
    10: begin
      result.Content := JSONEncode(['daemon',DaemonName]);
      if (DatabaseName='') and (fInternalDatabases<>nil) then begin
        fInternalDatabases[0].AdministrationExecute('',SQL,result);
        exit;
      end;
    end;
    end;
  end;
  rest := PublishedORM(DatabaseName);
  if rest<>nil then
    rest.AdministrationExecute(DatabaseName,SQL,result);
end;

function TDDDAdministratedDaemon.DatabaseList: TRawUTF8DynArray;
var i,n: integer;
begin
  n := length(fInternalDatabases);
  SetLength(result,n);
  for i := 0 to n-1 do
    result[i] := fInternalDatabases[i].Model.Root;
end;

function TDDDAdministratedDaemon.DatabaseTables(const DatabaseName: RawUTF8): TRawUTF8DynArray;
var rest: TSQLRest;
    i: integer;
begin
  rest := PublishedORM(DatabaseName);
  if rest=nil then
    result := nil else
    with rest.Model do begin
      SetLength(result,TablesMax+1);
      for i := 0 to TablesMax do
        result[i] := TableProps[i].Props.SQLTableName;
    end;
end;

procedure TDDDAdministratedDaemon.PublishORMTables(const Rest: array of TSQLRest);
var i,n: integer;
begin
  SetLength(fInternalDatabases,length(Rest));
  n := 0;
  for i := 0 to high(Rest) do
    if Rest[i]<>nil then begin
      fInternalDatabases[n] := Rest[i];
      inc(n);
    end;
  SetLength(fInternalDatabases,n);
end;

procedure TDDDAdministratedDaemon.InternalStop;
begin
  fInternalDatabases := nil;
end;

function TDDDAdministratedDaemon.InternalRetrieveState(
  var Status: variant): boolean;
begin
  Status := _ObjFast(['SystemMemory',TSynMonitorMemory.ToVariant]);
  result := true;
end;

procedure TDDDAdministratedDaemon.SetInternalSettings(Settings: TObject);
begin
  fInternalSettings := Settings;
end;

function TDDDAdministratedDaemon.DaemonName: RawUTF8;
var L: integer;
begin
  result := RawUTF8(ClassName);
  if result[1]='T' then
    delete(result,1,1);
  L := length(result);
  if copy(result,L-5,6)='Daemon' then
    SetLength(result,L-6);
end;


{ TDDDAdministratedThreadDaemon }

function TDDDAdministratedThreadDaemon.InternalIsRunning: boolean;
begin
  result := fThread<>nil;
end;

procedure TDDDAdministratedThreadDaemon.InternalStop;
begin
  inherited InternalStop; // fInternalDatabases := []
  FreeAndNil(fThread); // should terminate and wait for the thread to finish
end;


{ TDDDAdministratedRestDaemon }

function TDDDAdministratedRestDaemon.InternalIsRunning: boolean;
begin
  result := fRest<>nil;
end;

procedure TDDDAdministratedRestDaemon.InternalStop;
begin
  inherited InternalStop; // fInternalDatabases := []
  FreeAndNil(fRest);
end;


{ TDDDAdministratedDaemonMonitor }

function TDDDAdministratedDaemonMonitor.GetMemory: variant;
begin
  result := TSynMonitorMemory.ToVariant;
end;

procedure TDDDAdministratedDaemonMonitor.ProcessException(E: Exception);
begin
  Server.ProcessError(ObjectToVariantDebug(E));
end;

initialization
  {$ifndef ISDELPHI2010}
  {$ifndef HASINTERFACERTTI} // circumvent a old FPC bug
  TTextWriter.RegisterCustomJSONSerializerFromTextSimpleType([
    TypeInfo(TCQRSResult), TypeInfo(TCQRSQueryAction), TypeInfo(TCQRSQueryState),
    TypeInfo(TDDDAdministratedDaemonStatus)]);
  {$endif}
  {$endif}
  GetEnumNames(TypeInfo(TCQRSResult), @TCQRSResultText);
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(IMonitored),TypeInfo(IMonitoredDaemon),
    TypeInfo(IAdministratedDaemon),TypeInfo(IAdministratedDaemonAsProxy)]);
end.
