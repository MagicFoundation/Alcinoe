/// direct optimized MongoDB access for mORMot's ORM
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotMongoDB;

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
  - complex WHERE clause with a MongoDB Query object instead of SQL syntax;
    mitigated by the fact that most SQL queries are translated into BSON
    Query Object at runtime - needed only for most complex features like
    in-object inspection?
  - handle TSQLRawBlob fields with GridFS (and rely on TByteDynArray to store
    smaller BLOBs - < 16 MB - within the document, or in a separated collection)
  - allow PolyMorphic schemas: the same MongoDB collection may be able to
    store a hierarchy of TSQLRecord classes, storing only relevant fields in
    each document - this may be a huge benefit in common OOP work - could be
    implemented in mORMot.pas for any DB - see [bf459fe126] and [da0bccd89e]
  - SQLite3 Virtual Table mode, for full integration with mORMotDB - certainly
    in a dedicated mORMotDBMongoDB unit (but perhaps we may loose interest)

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$endif}
  {$ifdef KYLIX3}
  LibC,
  {$endif}
  {$ifdef HASINLINENOTX86}
  Contnrs,
  {$endif}
  SysUtils,
  Classes,
  Variants,
  SynCommons,
  SynTable, // for TSynTableStatement
  SynLog,
  mORMot,
  SynMongoDB;

type
  /// exception class raised by this units
  EORMMongoDBException = class(EORMException);

  /// how TSQLRestStorageMongoDB would compute the next ID to be inserted
  // - you may choose to retrieve the last inserted ID via
  // $ {$query:{},$orderby:{_id:-1}}
  // or search for the current maximum ID in the collection via
  // $ {$group:{_id:null,max:{$max:"$_id"}}}
  // - eacLastIDOnce and eacMaxIDOnce would execute the request once when
  // the storage instance is first started, whereas eacLastIDEachTime and
  // eacMaxIDEachTime would be execute before each insertion
  // - with big amount of data, retrieving the maximum ID (eacMaxID*) performs
  // a full scan, which would be very slow: the last inserted ID (eacLastID*)
  // would definitively be faster
  // - in all cases, to ensure that a centralized MongoDB server has unique
  // ID, you should better pre-compute the ID using your own algorithm
  // depending on your nodes topology, and not rely on the ORM, e.g. using
  // SetEngineAddComputeIdentifier() method, which would allocate a
  // TSynUniqueIdentifierGenerator and associate eacSynUniqueIdentifier
  TSQLRestStorageMongoDBEngineAddComputeID = (
    eacLastIDOnce, eacLastIDEachTime,
    eacMaxIDOnce, eacMaxIDEachTime,
    eacSynUniqueIdentifier);

  /// REST server with direct access to a MongoDB external database
  // - handle all REST commands via direct SynMongoDB call
  // - is used by TSQLRestServer.URI for faster RESTful direct access
  // - JOINed SQL statements are not handled yet
  TSQLRestStorageMongoDB = class(TSQLRestStorage)
  protected
    fCollection: TMongoCollection;
    fEngineLastID: TID;
    fEngineGenerator: TSynUniqueIdentifierGenerator;
    fEngineAddCompute: TSQLRestStorageMongoDBEngineAddComputeID;
    fBSONProjectionSimpleFields: variant;
    fBSONProjectionBlobFields: variant;
    fBSONProjectionBlobFieldsNames: TRawUTF8DynArray;
    // multi-thread BATCH process is secured via Lock/UnLock critical section
    fBatchMethod: TSQLURIMethod;
    fBatchWriter: TBSONWriter;
    fBatchIDs: TIDDynArray;
    fBatchIDsCount: integer;
    function EngineNextID: TID;
    function DocFromJSON(const JSON: RawUTF8; Occasion: TSQLOccasion;
      var Doc: TDocVariantData): TID;
    procedure JSONFromDoc(var doc: TDocVariantData; var result: RawUTF8);
    function BSONProjectionSet(var Projection: variant; WithID: boolean;
      const Fields: TSQLFieldBits; BSONFieldNames: PRawUTF8DynArray;
      const SubFields: TRawUTF8DynArray): integer;
    function GetJSONValues(const Res: TBSONDocument;
      const extFieldNames: TRawUTF8DynArray; W: TJSONSerializer): integer;
    // overridden methods calling the MongoDB external server
    function EngineRetrieve(TableModelIndex: integer; ID: TID): RawUTF8; override;
    function EngineList(const SQL: RawUTF8; ForceAJAX: Boolean=false; ReturnedRowCount: PPtrInt=nil): RawUTF8; override;
    function EngineAdd(TableModelIndex: integer; const SentData: RawUTF8): TID; override;
    function EngineUpdate(TableModelIndex: integer; ID: TID; const SentData: RawUTF8): boolean; override;
    function EngineUpdateField(TableModelIndex: integer;
      const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean; override;
    function EngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
      const FieldName: RawUTF8; Increment: Int64): boolean; override;
    function EngineDeleteWhere(TableModelIndex: Integer;const SQLWhere: RawUTF8;
      const IDs: TIDDynArray): boolean; override;
    // BLOBs should be accessed directly, not through slower JSON Base64 encoding
    function EngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PPropInfo; out BlobData: TSQLRawBlob): boolean; override;
    function EngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PPropInfo; const BlobData: TSQLRawBlob): boolean; override;
    // method not implemented: always return false
    function EngineExecute(const aSQL: RawUTF8): boolean; override;
    /// TSQLRestServer.URI use it for Static.EngineList to by-pass virtual table
    // - overridden method which allows return TRUE, i.e. always by-pass
    // virtual tables process
    function AdaptSQLForEngineList(var SQL: RawUTF8): boolean; override;
    // overridden method returning TRUE for next calls to EngineAdd/Delete
    // will properly handle operations until InternalBatchStop is called
    // BatchOptions is ignored with MongoDB (yet)
    function InternalBatchStart(Method: TSQLURIMethod;
      BatchOptions: TSQLRestBatchOptions): boolean; override;
    // internal method called by TSQLRestServer.RunBatch() to process fast
    // BULK sending to remote MongoDB database
    procedure InternalBatchStop; override;
  public
    /// initialize the direct access to the MongoDB collection
    // - in practice, you should not have to call this constructor, but rather
    // StaticMongoDBRegister() with a TMongoDatabase instance
    constructor Create(aClass: TSQLRecordClass; aServer: TSQLRestServer); override;
    /// release used memory
    destructor Destroy; override;

     /// overridden method for one single update call to the MongoDB server
    function UpdateBlobFields(Value: TSQLRecord): boolean; override;
     /// overridden method for one single read call to the MongoDB server
    function RetrieveBlobFields(Value: TSQLRecord): boolean; override;
    /// get the row count of a specified table
    // - return -1 on error
    // - return the row count of the table on success
    function TableRowCount(Table: TSQLRecordClass): Int64; override;
    /// check if there is some data rows in a specified table
    function TableHasRows(Table: TSQLRecordClass): boolean; override;
    /// delete a row, calling the current MongoDB server
    // - made public since a TSQLRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TSQLRestServer
    function EngineDelete(TableModelIndex: integer; ID: TID): boolean; override;
    /// create one index for all specific FieldNames at once
    function CreateSQLMultiIndex(Table: TSQLRecordClass; const FieldNames: array of RawUTF8;
      Unique: boolean; IndexName: RawUTF8=''): boolean; override;
    /// search for a field value, according to its SQL content representation
    // - return true on success (i.e. if some values have been added to ResultID)
    // - store the results into the ResultID dynamic array
    // - faster than OneFieldValues method, which creates a temporary JSON content
    function SearchField(const FieldName, FieldValue: RawUTF8;
      out ResultID: TIDDynArray): boolean; override;

    /// drop the whole table content
    // - in practice, dropping the whole MongoDB database would be faster
    // - but you can still add items to it - whereas Collection.Drop would
    // trigger GPF issues
    procedure Drop;
    /// initialize an internal time-based unique ID generator, linked to
    // a genuine process identifier
    // - will allocate a local TSynUniqueIdentifierGenerator
    // - EngineAddCompute would be set to eacSynUniqueIdentifier
    procedure SetEngineAddComputeIdentifier(aIdentifier: word);
  published
    /// the associated MongoDB collection instance
    property Collection: TMongoCollection read fCollection;
    /// how the next ID would be compute at each insertion
    // - default eacLastIDOnce may be the fastest, but other options are
    // available, and may be used in some special cases
    // - consider using SetEngineAddComputeIdentifier() which is both safe
    // and fast, with a cloud of servers sharing the same MongoDB collection
    property EngineAddCompute: TSQLRestStorageMongoDBEngineAddComputeID
      read fEngineAddCompute write fEngineAddCompute;
  end;


/// creates and register a static class on the Server-side to let a given
// ORM class be stored on a remote MongoDB server
// - will associate the supplied class with a MongoDB collection for a
// specified MongoDB database
// - to be called before Server.CreateMissingTables
// - by default, the collection name will match TSQLRecord.SQLTableName, but
// you can customize it with the corresponding parameter
// - the TSQLRecord.ID (RowID) field is always mapped to MongoDB's _id field
// - will call create needed indexes
// - you can later call aServer.InitializeTables to create any missing index and
// initialize the void tables (e.g. default TSQLAuthGroup and TSQLAuthUser records)
// - after registration, you can tune the field-name mapping by calling
// ! aModel.Props[aClass].ExternalDB.MapField(..)
// (just a regular external DB as defined in mORMotDB.pas unit) - it may be
// a good idea to use short field names on MongoDB side, to reduce the space
// used for storage (since they will be embedded within the document data)
// - it will return the corresponding TSQLRestStorageMongoDB instance -
// you can access later to it and its associated collection e.g. via:
// ! (aServer.StaticDataServer[TSQLMyTable] as TSQLRestStorageMongoDB)
// - you can set aMapAutoFieldsIntoSmallerLength to compute a field name
// mapping with minimal length, so that the stored BSON would be smaller:
// by definition, ID/RowID will be mapped as 'id', but other fields will
// use their first letter, and another other letter if needed (after a '_',
// or in uppercase, or the next one) e.g. FirstName -> 'f', LastName -> 'l',
// LockedAccount: 'la'...
function StaticMongoDBRegister(aClass: TSQLRecordClass; aServer: TSQLRestServer;
  aMongoDatabase: TMongoDatabase; aMongoCollectionName: RawUTF8='';
  aMapAutoFieldsIntoSmallerLength: boolean=false): TSQLRestStorageMongoDB;

type
  /// all possible options for StaticMongoDBRegisterAll/TSQLRestMongoDBCreate functions
  // - by default, TSQLAuthUser and TSQLAuthGroup tables will be handled via the
  // external DB, but you can avoid it for speed when handling session and security
  // by setting mrDoNotRegisterUserGroupTables
  // - you can set mrMapAutoFieldsIntoSmallerLength to compute a field name
  // mapping with minimal length, so that the stored BSON would be smaller:
  // by definition, ID/RowID will be mapped as 'id', but other fields will
  // use their first letter, and another other letter if needed (after a '_',
  // or in uppercase, or the next one) e.g. FirstName -> 'f', LastName -> 'l',
  // LockedAccount: 'la'... - WARNING: not yet implemented
  TStaticMongoDBRegisterOption = (
    mrDoNotRegisterUserGroupTables,
    mrMapAutoFieldsIntoSmallerLength
    );
  /// set of options for StaticMongoDBRegisterAll/TSQLRestMongoDBCreate functions
  TStaticMongoDBRegisterOptions = set of TStaticMongoDBRegisterOption;

/// create and register ALL classes of a given model to access a MongoDB server
// - the collection names will follow the class names
// - this function will call aServer.InitializeTables to create any missing
// index or populate default collection content
// - if aMongoDBIdentifier is not 0, then SetEngineAddComputeIdentifier()
// would be called
function StaticMongoDBRegisterAll(aServer: TSQLRestServer;
  aMongoDatabase: TMongoDatabase; aOptions: TStaticMongoDBRegisterOptions=[];
  aMongoDBIdentifier: word=0): boolean;

/// create a new TSQLRest instance, possibly using MongoDB for its ORM process
// - if aDefinition.Kind matches a TSQLRest registered class, one new instance
// of this kind will be created and returned
// - if aDefinition.Kind is 'MongoDB' or 'MongoDBS', it will instantiate an
// in-memory TSQLRestServerDB or a TSQLRestServerFullMemory instance (calling
// TSQLRestServer.CreateInMemoryForAllVirtualTables), then call
// StaticMongoDBRegisterAll() with a TMongoClient initialized from
// aDefinition.ServerName ('server' or 'server:port') - optionally with TLS
// enabled if Kind equals 'MongoDBS' - and a TMongoDatabase created from
// aDefinition.DatabaseName, using authentication if aDefinition.User/Password
// credentials are set
// - it will return nil if the supplied aDefinition is invalid
// - if aMongoDBIdentifier is not 0, then SetEngineAddComputeIdentifier()
// would be called for all created TSQLRestStorageMongoDB
function TSQLRestMongoDBCreate(aModel: TSQLModel;
  aDefinition: TSynConnectionDefinition; aHandleAuthentication: boolean;
  aOptions: TStaticMongoDBRegisterOptions; aMongoDBIdentifier: word=0): TSQLRest; overload;


function ToText(eac: TSQLRestStorageMongoDBEngineAddComputeID): PShortString; overload;

implementation

function ToText(eac: TSQLRestStorageMongoDBEngineAddComputeID): PShortString;
begin
  result := GetEnumName(TypeInfo(TSQLRestStorageMongoDBEngineAddComputeID),ord(eac));
end;


function StaticMongoDBRegister(aClass: TSQLRecordClass; aServer: TSQLRestServer;
  aMongoDatabase: TMongoDatabase; aMongoCollectionName: RawUTF8;
  aMapAutoFieldsIntoSmallerLength: boolean): TSQLRestStorageMongoDB;
var Props: TSQLModelRecordProperties;
begin
  result := nil;
  if (aServer=nil) or (aClass=nil) or (aMongoDatabase=nil) then
    exit; // avoid GPF
  {$ifdef WITHLOG}
  if aMongoDatabase.Client.Log=nil then
    aMongoDatabase.Client.SetLog(aServer.LogClass);
  with aServer.LogClass.Enter do
  {$endif WITHLOG}
  begin
    Props := aServer.Model.Props[aClass];
    if Props=nil then
      exit; // if aClass is not part of the model
    if aMongoCollectionName='' then
      aMongoCollectionName := Props.Props.SQLTableName;
    Props.ExternalDB.Init(aClass,aMongoCollectionName,
      aMongoDatabase.CollectionOrCreate[aMongoCollectionName],true,[]);
    Props.ExternalDB.MapField('ID','_id');
    result := TSQLRestStorageMongoDB.Create(aClass,aServer);
    aServer.StaticDataAdd(result);
  end;
end;

function StaticMongoDBRegisterAll(aServer: TSQLRestServer;
  aMongoDatabase: TMongoDatabase; aOptions: TStaticMongoDBRegisterOptions;
  aMongoDBIdentifier: word): boolean;
var i: integer;
    storage: TSQLRestStorageMongoDB;
begin
  if (aServer=nil) or (aMongoDatabase=nil) then begin
    result := false;
    exit; // avoid GPF
  end;
  result := true;
  with aServer.Model do
  for i := 0 to high(Tables) do
    if (mrDoNotRegisterUserGroupTables in aOptions) and
       (Tables[i].InheritsFrom(TSQLAuthGroup) or
        Tables[i].InheritsFrom(TSQLAuthUser)) then
      continue else begin
      storage := StaticMongoDBRegister(Tables[i],aServer,aMongoDatabase,'');
      if storage=nil then
        result := false else
        if aMongoDBIdentifier<>0 then
          storage.SetEngineAddComputeIdentifier(aMongoDBIdentifier);
      end;
  if result then // ensure TSQLRecord.InitializeTable() is called
    aServer.InitializeTables([]); // will create indexes and default data
end;

function TSQLRestMongoDBCreate(aModel: TSQLModel;
  aDefinition: TSynConnectionDefinition; aHandleAuthentication: boolean;
  aOptions: TStaticMongoDBRegisterOptions; aMongoDBIdentifier: word): TSQLRest;
var client: TMongoClient;
    database: TMongoDatabase;
    server,port, pwd: RawUTF8;
    tls: boolean;
    p: integer;
begin
  result := nil;
  if aDefinition=nil then
    exit;
  if IdemPChar(pointer(aDefinition.Kind),'MONGODB') then begin
    Split(aDefinition.ServerName,':',server,port);
    if (server='') or (server[1] in ['?','*']) or (aDefinition.DatabaseName='') then
      exit; // check mandatory MongoDB IP and Database
    p := UTF8ToInteger(port,1024,65535,MONGODB_DEFAULTPORT);
    tls := ord(aDefinition.Kind[8]) in [ord('S'),ord('s')]; // 'MongoDBS'
    client := TMongoClient.Create(server,p,tls);
    try
      with aDefinition do
        if (User<>'') and (Password<>'') then begin
          pwd := PasswordPlain;
          database := client.OpenAuth(DatabaseName,User,pwd);
        end else
          database := client.Open(DatabaseName);
      result := TSQLRestServer.CreateInMemoryForAllVirtualTables(
        aModel,aHandleAuthentication);
      StaticMongoDBRegisterAll(TSQLRestServer(result),database,aOptions,aMongoDBIdentifier);
      result.PrivateGarbageCollector.Add(client); // connection owned by server
    except
      FreeAndNil(result);
      client.Free; // avoid memory leak
    end;
  end else
    // not MongoDB -> try if aDefinition.Kind is a TSQLRest class
    result := TSQLRest.CreateTryFrom(aModel,aDefinition,aHandleAuthentication);
end;


{ TSQLRestStorageMongoDB }

constructor TSQLRestStorageMongoDB.Create(aClass: TSQLRecordClass; aServer: TSQLRestServer);
begin
  inherited Create(aClass,aServer);
  // ConnectionProperties should have been set in StaticMongoDBRegister()
  fCollection := fStoredClassMapping^.ConnectionProperties as TMongoCollection;
  InternalLog('will store % using %',[aClass,Collection],sllInfo);
  BSONProjectionSet(fBSONProjectionSimpleFields,true,
    fStoredClassRecordProps.SimpleFieldsBits[soSelect],nil,nil);
  BSONProjectionSet(fBSONProjectionBlobFields,false,
    fStoredClassRecordProps.FieldBits[sftBlob],@fBSONProjectionBlobFieldsNames,nil);
end;

function TSQLRestStorageMongoDB.BSONProjectionSet(var Projection: variant;
  WithID: boolean; const Fields: TSQLFieldBits; BSONFieldNames: PRawUTF8DynArray;
  const SubFields: TRawUTF8DynArray): integer;
var i,n,sf: integer;
    W: TBSONWriter;
    name: RawUTF8;
begin
  sf := length(SubFields);
  W := TBSONWriter.Create(TRawByteStringStream);
  try
    W.BSONDocumentBegin;
    if withID then
      result := 1 else
      result := 0;
    name := fStoredClassMapping^.RowIDFieldName;
    if sf>0 then
      name := name+SubFields[0];
    W.BSONWrite(name,result);
    for i := 0 to fStoredClassRecordProps.Fields.Count-1 do
      if i in Fields then begin
        name := fStoredClassMapping^.ExtFieldNames[i];
        if i+1<sf then
          name := name+SubFields[i+1];
        W.BSONWrite(name,1);
        inc(result);
      end;
    W.BSONDocumentEnd;
    W.ToBSONVariant(Projection);
    if BSONFieldNames<>nil then
    with fStoredClassMapping^ do begin
      SetLength(BSONFieldNames^,result);
      if WithID then begin
        BSONFieldNames^[0] := RowIDFieldName;
        n := 1;
      end else
        n := 0;
      for i := 0 to fStoredClassRecordProps.Fields.Count-1 do
        if i in Fields then begin
          BSONFieldNames^[n] := ExtFieldNames[i];
          inc(n);
        end;
    end;
  finally
    W.Free;
  end;
end;

function TSQLRestStorageMongoDB.CreateSQLMultiIndex(
  Table: TSQLRecordClass; const FieldNames: array of RawUTF8;
  Unique: boolean; IndexName: RawUTF8): boolean;
begin
  if (self=nil) or (fCollection=nil) or (Table<>fStoredClass) then begin
    result := false;
    exit;
  end;
  result := true;
  if (high(FieldNames)=0) and IsRowID(pointer(FieldNames[0])) then
    exit; // ID primary key is always indexed by MongoDB
  try
    fCollection.EnsureIndex(FieldNames,true,Unique);
  except
    result := false;
  end;
end;

procedure TSQLRestStorageMongoDB.Drop;
var DB: TMongoDatabase;
    CollName: RawUTF8;
begin
  DB := Collection.Database;
  CollName := Collection.Name;
  Collection.Drop;
  fCollection := DB.CollectionOrCreate[CollName];
  fEngineLastID := 0;
end;

destructor TSQLRestStorageMongoDB.Destroy;
begin
  inherited;
  FreeAndNil(fBatchWriter);
  fEngineGenerator.Free;
  InternalLog('Destroy for % using %',[fStoredClass,Collection],sllInfo);
end;

function TSQLRestStorageMongoDB.TableHasRows(
  Table: TSQLRecordClass): boolean;
begin
  if (fCollection=nil) or (Table<>fStoredClass) then
    result := false else
    result := not fCollection.IsEmpty;
end;

function TSQLRestStorageMongoDB.TableRowCount(
  Table: TSQLRecordClass): Int64;
begin
  if (fCollection=nil) or (Table<>fStoredClass) then
    result := 0 else
    result := fCollection.Count;
end;

procedure TSQLRestStorageMongoDB.SetEngineAddComputeIdentifier(aIdentifier: word);
begin
  fEngineGenerator.Free;
  fEngineGenerator := TSynUniqueIdentifierGenerator.Create(aIdentifier);
  fEngineAddCompute := eacSynUniqueIdentifier;
end;

function TSQLRestStorageMongoDB.EngineNextID: TID;

  procedure ComputeMax_ID;
  var res: variant;
      timer: TPrecisionTimer;
  begin
    timer.Start;
    case fEngineAddCompute of
    eacLastIDOnce, eacLastIDEachTime: begin
      res := fCollection.FindDoc(BSONVariant('{$query:{},$orderby:{_id:-1}}'),BSONVariant(['_id',1]));
      if not VarIsEmptyOrNull(res) then
        fEngineLastID := _Safe(res)^.I['_id'];
    end;
    eacMaxIDOnce, eacMaxIDEachTime: begin
      res := fCollection.AggregateDocFromJson('{$group:{_id:null,max:{$max:"$_id"}}}');
      if not VarIsEmptyOrNull(res) then
        fEngineLastID := _Safe(res)^.I['max'];
    end;
    else
      raise EORMMongoDBException.CreateUTF8('Unexpected %.EngineNextID with %',
        [self,ToText(fEngineAddCompute)^]);
    end;
    InternalLog('ComputeMax_ID=% in % using %',
      [fEngineLastID,timer.Stop,ToText(fEngineAddCompute)^],sllInfo);
  end;

begin
  if (fEngineAddCompute=eacSynUniqueIdentifier) and (fEngineGenerator<>nil) then begin
    result := fEngineGenerator.ComputeNew;
    fEngineLastID := result;
    exit;
  end;
  EnterCriticalSection(fStorageCriticalSection);
  if (fEngineLastID=0) or (fEngineAddCompute in [eacLastIDEachTime,eacMaxIDEachTime]) then
    ComputeMax_ID;
  inc(fEngineLastID);
  result := fEngineLastID;
  LeaveCriticalSection(fStorageCriticalSection);
end;

function TSQLRestStorageMongoDB.DocFromJSON(const JSON: RawUTF8;
  Occasion: TSQLOccasion; var Doc: TDocVariantData): TID;
var i, ndx: integer;
    dt: TDateTime;
    blob: RawByteString;
    info: TSQLPropInfo;
    typenfo: pointer;
    js, RecordVersionName: RawUTF8;
    MissingID: boolean;
    V: PVarData;
begin
  doc.InitJSON(JSON,[dvoValueCopiedByReference,dvoAllowDoubleValue]);
  if (doc.Kind<>dvObject) and (Occasion<>soInsert) then
    raise EORMMongoDBException.CreateUTF8('%.DocFromJSON: invalid JSON context',[self]);
  if not (Occasion in [soInsert,soUpdate]) then
    raise EORMMongoDBException.CreateUTF8('Unexpected %.DocFromJSON(Occasion=%)',
      [self,ToText(Occasion)^]);
  MissingID := true;
  for i := doc.Count-1 downto 0 do // downwards for doc.Delete(i) below
    if IsRowID(pointer(doc.Names[i])) then begin
      doc.Names[i] := fStoredClassMapping^.RowIDFieldName;
      VariantToInt64(doc.Values[i],Int64(result));
      if (Occasion=soUpdate) or (result=0) then
        doc.Delete(i) else  // update does not expect any $set:{_id:..}
        MissingID := false; // leave true if value is not an integer (=0)
    end else begin
      ndx := fStoredClassRecordProps.Fields.IndexByName(doc.Names[i]);
      if ndx<0 then
        raise EORMMongoDBException.CreateUTF8(
          '%.DocFromJSON: unkwnown field name [%]',[self,doc.Names[i]]);
      doc.Names[i] := fStoredClassMapping^.ExtFieldNames[ndx];
      info := fStoredClassRecordProps.Fields.List[ndx];
      V := @doc.Values[i];
      case V^.VType of
      varInteger:
      case info.SQLFieldType of
        sftBoolean: begin // doc.InitJSON/GetVariantFromJSON store 0,1 as varInteger
          if V^.VInteger=0 then // normalize to boolean BSON
            V^.VBoolean := false else
            V^.VBoolean := true;
          V^.VType := varBoolean;
        end;
        sftUnixTime: begin
          V^.VDate := UnixTimeToDateTime(V^.VInteger); // as MongoDB date/time
          V^.VType := varDate; // direct set to avoid unexpected EInvalidOp
        end;
        sftUnixMSTime: begin
          V^.VDate := UnixMSTimeToDateTime(V^.VInteger); // as MongoDB date/time
          V^.VType := varDate;
        end;
      end;
      varInt64:
      case info.SQLFieldType of
        sftUnixTime: begin
          V^.VDate := UnixTimeToDateTime(V^.VInt64); // as MongoDB date/time
          V^.VType := varDate; // direct set to avoid unexpected EInvalidOp
        end;
        sftUnixMSTime: begin
          V^.VDate := UnixMSTimeToDateTime(V^.VInt64); // as MongoDB date/time
          V^.VType := varDate;
        end;
      end;
      varString: // handle some TEXT values
      case info.SQLFieldType of
        sftDateTime, sftDateTimeMS: begin // ISO-8601 text as MongoDB date/time
          Iso8601ToDateTimePUTF8CharVar(V^.VAny,length(RawByteString(V^.VAny)),dt);
          RawByteString(V^.VAny) := '';
          V^.VType := varDate; // direct set to avoid unexpected EInvalidOp
          V^.VDate := dt;
        end;
        sftBlob, sftBlobCustom: begin // store Base64-encoded BLOB as binary
          blob := BlobToTSQLRawBlob(RawByteString(V^.VAny));
          BSONVariantType.FromBinary(blob,bbtGeneric,Variant(V^));
        end;
        sftBlobDynArray: begin // store dynamic array as object (if has any JSON)
          blob := BlobToTSQLRawBlob(RawByteString(V^.VAny));
          if blob='' then
            SetVariantNull(Variant(V^)) else begin
            typenfo := (info as TSQLPropInfoRTTIDynArray).PropType;
            if (typenfo=TypeInfo(TByteDynArray)) or (typenfo=TypeInfo(TBytes)) then
              js := '' else // embedded BLOB type stored as BSON binary
              js := DynArrayBlobSaveJSON(typenfo,pointer(blob));
            if (js<>'') and (PInteger(js)^ and $00ffffff<>JSON_BASE64_MAGIC) then
              BSONVariantType.FromJSON(pointer(js),Variant(V^)) else
              BSONVariantType.FromBinary(blob,bbtGeneric,Variant(V^));
          end;
        end;
        end;
        // sftObject,sftVariant,sftUTF8Custom were already converted to object from JSON
      end;
    end;
  if Occasion=soInsert then
    if MissingID then begin
      result := EngineNextID;
      doc.AddValue(fStoredClassMapping^.RowIDFieldName,result);
    end else begin
      if fEngineAddCompute=eacSynUniqueIdentifier then
        raise EORMMongoDBException.CreateUTF8('%.DocFromJSON: unexpected set '+
          '%.ID=% with %',[self,fStoredClass,result,fEngineGenerator]);
      EnterCriticalSection(fStorageCriticalSection);
      if result>fEngineLastID then
        fEngineLastID := result;
      LeaveCriticalSection(fStorageCriticalSection);
    end;
  if fStoredClassRecordProps.RecordVersionField<>nil then begin
    RecordVersionName := fStoredClassMapping^.ExtFieldNames[
      fStoredClassRecordProps.RecordVersionField.PropertyIndex];
    if doc.GetValueIndex(RecordVersionName)<0 then
      if Owner=nil then
        raise EORMMongoDBException.CreateUTF8(
          '%.DocFromJSON: unexpected Owner=nil with %.%: TRecordVersion',
          [self,fStoredClass,fStoredClassRecordProps.RecordVersionField.Name]) else
      // compute new monotonic TRecordVersion value if not supplied by sender
      doc.AddValue(RecordVersionName,Owner.RecordVersionCompute);
    if (Owner<>nil) and (Owner.Services<>nil) then
      (Owner.Services as TServiceContainerServer).RecordVersionNotifyAddUpdate(
        Occasion,fStoredClassProps.TableIndex,doc);
  end;
  if doc.Kind<>dvObject then
    raise EORMMongoDBException.CreateUTF8('%.DocFromJSON: Invalid JSON context',[self]);
end;

function TSQLRestStorageMongoDB.EngineAdd(TableModelIndex: integer;
  const SentData: RawUTF8): TID;
var doc: TDocVariantData;
begin
  if (fCollection=nil) or (TableModelIndex<0) or
    (fModel.Tables[TableModelIndex]<>fStoredClass) then
    result := 0 else
    try
      result := DocFromJSON(SentData,soInsert,Doc);
      if fBatchMethod<>mNone then
        if (fBatchMethod<>mPOST) or (fBatchWriter=nil) then
          result := 0 else begin
          inc(fBatchIDsCount);
          fBatchWriter.BSONWriteDoc(doc);
        end else begin
        fCollection.Insert([variant(doc)]);
        if Owner<>nil then begin
          Owner.InternalUpdateEvent(seAdd,TableModelIndex,result,SentData,nil);
          Owner.FlushInternalDBCache;
        end;
      end;
    except
      result := 0;
    end;
end;

function TSQLRestStorageMongoDB.EngineUpdate(TableModelIndex: integer; ID: TID;
  const SentData: RawUTF8): boolean;
var doc: TDocVariantData;
    query,update: variant; // use explicit TBSONVariant for type safety
begin
  if (fCollection=nil) or (ID<=0) or
     (TableModelIndex<0) or (Model.Tables[TableModelIndex]<>fStoredClass) then
    result := false else
    try
      DocFromJSON(SentData,soUpdate,doc);
      query := BSONVariant(['_id',ID]);
      update := BSONVariant(['$set',variant(doc)]);
      fCollection.Update(query,update);
      if Owner<>nil then begin
        Owner.InternalUpdateEvent(seUpdate,TableModelIndex,ID,SentData,nil);
        Owner.FlushInternalDBCache;
      end;
      result := true;
    except
      result := false;
    end;
end;

function TSQLRestStorageMongoDB.EngineUpdateField(TableModelIndex: integer;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean;
var JSON: RawUTF8;
    query,update: variant; // use explicit TBSONVariant for type safety
    id: TBSONIterator;
begin
  if (fCollection=nil) or (TableModelIndex<0) or
     (fModel.Tables[TableModelIndex]<>fStoredClass) or
     (SetFieldName='') or (SetValue='') or (WhereFieldName='') or (WhereValue='') then
    result := false else
    try // use {%:%} here since WhereValue/SetValue are already JSON encoded
      query := BSONVariant('{%:%}',[fStoredClassMapping^.InternalToExternal(
        WhereFieldName),WhereValue],[]);
      update := BSONVariant('{$set:{%:%}}',[fStoredClassMapping^.InternalToExternal(
        SetFieldName),SetValue],[]);
      fCollection.Update(query,update);
      if Owner<>nil then begin
        if Owner.InternalUpdateEventNeeded(TableModelIndex) and
           id.Init(fCollection.FindBSON(query,BSONVariant(['_id',1]))) then begin
          JSONEncodeNameSQLValue(SetFieldName,SetValue,JSON);
          while id.Next do
            Owner.InternalUpdateEvent(seUpdate,TableModelIndex,
              id.Item.DocItemToInteger('_id'),JSON,nil);
        end;
        Owner.FlushInternalDBCache;
      end;
      result := true;
    except
      result := false;
    end;
end;

function TSQLRestStorageMongoDB.EngineUpdateFieldIncrement(TableModelIndex: integer;
  ID: TID; const FieldName: RawUTF8; Increment: Int64): boolean;
var Value: Int64;
begin
  result := false;
  if (ID<=0) or (TableModelIndex<0) or (Model.Tables[TableModelIndex]<>fStoredClass) then
    exit;
  if (Owner<>nil) and Owner.InternalUpdateEventNeeded(TableModelIndex) then
    result := OneFieldValue(fStoredClass,FieldName,'ID=?',[],[ID],Value) and
              UpdateField(fStoredClass,ID,FieldName,[Value+Increment]) else
    try
      fCollection.Update(BSONVariant(['_id',ID]),BSONVariant('{$inc:{%:%}}',
        [fStoredClassMapping^.InternalToExternal(FieldName),Increment],[]));
      if Owner<>nil then
        Owner.FlushInternalDBCache;
      result := true;
    except
      on Exception do
        result := false;
    end;
end;

function TSQLRestStorageMongoDB.EngineUpdateBlob(TableModelIndex: integer; aID: TID;
  BlobField: PPropInfo; const BlobData: TSQLRawBlob): boolean;
var query,update,blob: variant; // use explicit TBSONVariant for type safety
    FieldName: RawUTF8;
    AffectedField: TSQLFieldBits;
begin
  if (fCollection=nil) or (BlobField=nil) or (aID<=0) or
     (TableModelIndex<0) or (Model.Tables[TableModelIndex]<>fStoredClass) then
    result := false else
    try
      query := BSONVariant(['_id',aID]);
      FieldName := fStoredClassMapping^.InternalToExternal(BlobField^.Name);
      BSONVariantType.FromBinary(BlobData,bbtGeneric,blob);
      update := BSONVariant(['$set',BSONVariant([FieldName,blob])]);
      fCollection.Update(query,update);
      if Owner<>nil then begin
        fStoredClassRecordProps.FieldBitsFromBlobField(BlobField,AffectedField);
        Owner.InternalUpdateEvent(seUpdateBlob,TableModelIndex,aID,'',@AffectedField);
        Owner.FlushInternalDBCache;
      end;
      result := true;
    except
      result := false;
    end;
end;

function TSQLRestStorageMongoDB.UpdateBlobFields(
  Value: TSQLRecord): boolean;
var query,blob: variant;
    update: TDocVariantData;
    info: TSQLPropInfo;
    blobRaw: RawByteString;
    aID, f: integer;
begin
  result := false;
  if (fCollection=nil) or (PSQLRecordClass(Value)^<>fStoredClass) or (Value=nil) then
    exit;
  aID := Value.ID;
  if aID<=0 then
    exit;
  query := BSONVariant(['_id',aID]);
  update.Init(JSON_OPTIONS_FAST);
  for f := 0 to fStoredClassRecordProps.Fields.Count-1 do begin
    info := fStoredClassRecordProps.Fields.List[f];
    if info.SQLFieldType=sftBlob then begin
      (info as TSQLPropInfoRTTIRawBlob).GetBlob(Value,blobRaw);
      BSONVariantType.FromBinary(blobRaw,bbtGeneric,blob);
      update.AddValue(fStoredClassMapping^.ExtFieldNames[f],blob);
    end;
  end;
  if update.Count>0 then
    try
      fCollection.Update(query,BSONVariant(['$set',variant(update)]));
      if Owner<>nil then begin
        Owner.InternalUpdateEvent(seUpdateBlob,fStoredClassProps.TableIndex,aID,'',
          @fStoredClassRecordProps.FieldBits[sftBlob]);
        Owner.FlushInternalDBCache;
      end;
      result := true;
    except
      result := false;
    end;
end;

function TSQLRestStorageMongoDB.EngineDelete(TableModelIndex: integer; ID: TID): boolean;
begin
  result := false;
  if (fCollection<>nil) and (TableModelIndex>=0) and
     (Model.Tables[TableModelIndex]=fStoredClass) and (ID>0) then
  try
    if fBatchMethod<>mNone then
      if fBatchMethod<>mDelete then
        exit else
        AddID(fBatchIDs,fBatchIDsCount,ID) else begin
      if Owner<>nil then begin // notify BEFORE deletion
        Owner.InternalUpdateEvent(seDelete,TableModelIndex,ID,'',nil);
        Owner.FlushInternalDBCache;
      end;
      fCollection.RemoveOne(ID);
    end;
    result := true;
  except
    result := false;
  end;
end;

function TSQLRestStorageMongoDB.EngineDeleteWhere(TableModelIndex: Integer;
  const SQLWhere: RawUTF8; const IDs: TIDDynArray): boolean;
var i: integer;
begin // here we use the pre-computed IDs[]
  result := false;
  if (fCollection<>nil) and (TableModelIndex>=0) and
     (Model.Tables[TableModelIndex]=fStoredClass) and (IDs<>nil) then
    try
      if Owner<>nil then // notify BEFORE deletion
        for i := 0 to high(IDs) do
          Owner.InternalUpdateEvent(seDelete,TableModelIndex,IDs[i],'',nil);
      fCollection.Remove(BSONVariant(
        ['_id',BSONVariant(['$in',BSONVariantFromInt64s(TInt64DynArray(IDs))])]));
      if Owner<>nil then
        Owner.FlushInternalDBCache;
      result := true;
    except
      result := false;
    end;
end;

procedure TSQLRestStorageMongoDB.JSONFromDoc(var doc: TDocVariantData;
  var result: RawUTF8);
var i: integer;
    name: RawUTF8;
    W: TTextWriter;
    tmp: TTextWriterStackBuffer;
begin
  if (doc.VarType<>DocVariantType.VarType) or (doc.Kind<>dvObject) or (doc.Count=0) then begin
    result := '';
    exit;
  end;
  W := TTextWriter.CreateOwnedStream(tmp);
  try
    W.Add('{');
    for i := 0 to doc.Count-1 do begin
      name := fStoredClassMapping^.ExternalToInternalOrNull(doc.Names[i]);
      if name='' then
        raise EORMMongoDBException.CreateUTF8(
          '%.JSONFromDoc: Unknown field [%] for %',[self,doc.Names[i],fStoredClass]);
      W.AddProp(pointer(name),Length(name));
      W.AddVariant(doc.Values[i],twJSONEscape);
      W.Add(',');
    end;
    W.CancelLastComma;
    W.Add('}');
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function TSQLRestStorageMongoDB.EngineRetrieve(TableModelIndex: integer;
  ID: TID): RawUTF8;
var doc: variant;
begin
  result := '';
  if (fCollection=nil) or (ID<=0) then
    exit;
  doc := fCollection.FindDoc(BSONVariant(['_id',ID]),fBSONProjectionSimpleFields,1);
  JSONFromDoc(_Safe(doc)^,result);
end;

function TSQLRestStorageMongoDB.EngineRetrieveBlob(TableModelIndex: integer; aID: TID;
  BlobField: PPropInfo; out BlobData: TSQLRawBlob): boolean;
var doc: variant;
    data: TVarData;
    FieldName: RawUTF8;
begin
  if (fCollection=nil) or (BlobField=nil) or (aID<=0) or
     (TableModelIndex<0) or (Model.Tables[TableModelIndex]<>fStoredClass) then
    result := false else
    try
      FieldName := fStoredClassMapping^.InternalToExternal(BlobField^.Name);
      doc := fCollection.FindDoc(BSONVariant(['_id',aID]),BSONVariant([FieldName,1]),1);
      if _Safe(doc)^.GetVarData(FieldName,data) then
        BSONVariantType.ToBlob(variant(data),RawByteString(BlobData));
      result := true;
    except
      result := false;
    end;
end;

function TSQLRestStorageMongoDB.RetrieveBlobFields(
  Value: TSQLRecord): boolean;
var aID, f: Integer;
    doc: variant;
    docv: PDocVariantData;
    blob: TVarData;
    blobRaw: RawByteString;
begin
  result := false;
  if (fCollection=nil) or (PSQLRecordClass(Value)^<>fStoredClass) or (Value=nil) then
    exit;
  aID := Value.ID;
  if aID<=0 then
    exit;
  try
    doc := fCollection.FindDoc(BSONVariant(['_id',aID]),fBSONProjectionBlobFields,1);
    docv := _Safe(doc);
    if docv^.Kind<>dvObject then
      exit; // not found
    for f := 0 to high(fStoredClassRecordProps.BlobFields) do begin
      if (f<docv^.Count) and // optimistic O(1) search
         IdemPropNameU(docv^.Names[f],fBSONProjectionBlobFieldsNames[f]) then
        BSONVariantType.ToBlob(docv^.Values[f],blobRaw) else
      if docv^.GetVarData(fBSONProjectionBlobFieldsNames[f],blob) then
        BSONVariantType.ToBlob(variant(blob),blobRaw) else
        raise EORMMongoDBException.CreateUTF8(
          '%.RetrieveBlobFields(%): field [%] not found',
          [self,Value,fBSONProjectionBlobFieldsNames[f]]);
      (fStoredClassRecordProps.BlobFields[f] as TSQLPropInfoRTTIRawBlob).
        SetBlob(Value,blobRaw);
    end;
    result := true;
  except
    result := false;
  end;
end;

function TSQLRestStorageMongoDB.AdaptSQLForEngineList(
  var SQL: RawUTF8): boolean;
begin
  result := true; // we do not have any Virtual Table yet -> always accept
end;

function TSQLRestStorageMongoDB.SearchField(const FieldName, FieldValue: RawUTF8;
  out ResultID: TIDDynArray): boolean;
var query: variant;
    id: TBSONIterator;
    n: integer; // an external count is actually faster
begin
  if (fCollection = nil) or (FieldName = '') or (FieldValue = '') then
    result := false else
  try
    // use {%:%} here since FieldValue is already JSON encoded
    query := BSONVariant('{%:%}',
      [fStoredClassMapping^.InternalToExternal(FieldName), FieldValue], []);
    // retrieve the IDs for this query
    if id.Init(fCollection.FindBSON(query, BSONVariant(['_id', 1]))) then begin
      n := 0;
      while id.Next do
        AddInt64(TInt64DynArray(ResultID), n, id.Item.DocItemToInteger('_id'));
      SetLength(ResultID, n);
      result := true;
    end else
      result := false;
  except
    result := false;
  end;
end;

function TSQLRestStorageMongoDB.GetJSONValues(const Res: TBSONDocument;
  const extFieldNames: TRawUTF8DynArray; W: TJSONSerializer): integer;
  function itemFind(item: PBSONElement; itemcount,o1ndx: integer;
    const aName: RawUTF8): PBSONElement;
  var aNameLen, i: integer;
  begin
    aNameLen := length(aName);
    if aNameLen<>0 then begin
      if o1ndx<itemcount then begin // O(1) optimistic search
        result := @PAnsiChar(item)[o1ndx*sizeof(item^)];
        if (result^.NameLen=aNameLen) and
           IdemPropNameUSameLen(pointer(aName),result^.Name,aNameLen) then
          exit;
      end;
      result := item;
      for i := 1 to itemcount do // O(n) search if field missing or moved
      if (result^.NameLen=aNameLen) and
         IdemPropNameUSameLen(pointer(aName),result^.Name,aNameLen) then
        exit else
        inc(result);
      end;
    result := nil;
  end;
var col, colCount: integer;
    row: TBSONIterator;
    item: array of TBSONElement;
    itemcount, itemsize: integer;
    itemfound: PBSONElement;
begin
  result := 0; // number of data rows in JSON output
  if W.Expand then
    W.Add('[');
  if row.Init(Res) then begin
    colCount := length(extFieldNames);
    if colCount<>length(W.ColNames) then
      raise EORMMongoDBException.CreateUTF8(
        '%.GetJSONValues(%): column count concern %<>%',
        [self,StoredClass,colCount,length(W.ColNames)]);
    itemsize := colCount;
    SetLength(item,itemsize);
    while row.Next do begin
      // retrieve all values of this BSON document into item[]
      if (row.Item.Kind<>betDoc) or (row.Item.Data.DocList=nil) then
        raise EORMMongoDBException.CreateUTF8('%.GetJSONValues(%): invalid row kind=%',
          [self,StoredClass,ord(row.Item.Kind)]);
      itemcount := 0;
      while row.Item.Data.DocList^<>byte(betEOF) do begin
        if itemcount>=itemsize then begin
          inc(itemsize);
          Setlength(item,itemsize); // a field was deleted from TSQLRecord
        end;
        if not item[itemcount].FromNext(row.Item.Data.DocList) then
          break;
        inc(itemcount);
      end;
      // convert this BSON document as JSON, following expected column order
      if W.Expand then
        W.Add('{');
      for col := 0 to colCount-1 do begin
        if W.Expand then
          W.AddString(W.ColNames[col]);
        itemfound := itemFind(pointer(item),itemcount,col,extFieldNames[col]);
        if itemfound=nil then // this field may not exist (e.g. older schema)
          W.AddShort('null') else
          itemfound^.AddMongoJSON(W,modNoMongo);
        W.Add(',');
      end;
      W.CancelLastComma;
      if W.Expand then
        W.Add('}',',') else
        W.Add(',');
      inc(result);
    end;
  end;
  if (result=0) and W.Expand then begin
    // we want the field names at least, even with no data
    W.Expand := false; //  {"fieldCount":2,"values":["col1","col2"]}
    W.CancelAll;
    fStoredClassRecordProps.SetJSONWriterColumnNames(W,0);
  end;
  W.EndJSONObject(0,result);
end;

function TSQLRestStorageMongoDB.EngineList(const SQL: RawUTF8;
  ForceAJAX: Boolean; ReturnedRowCount: PPtrInt): RawUTF8;
var ResCount: PtrInt;
    Stmt: TSynTableStatement;
    Query: variant;
    TextOrderByField: RawUTF8;
const ORDERBY_FIELD: array[boolean] of Integer=(1,-1);
procedure AddWhereClause(B: TBSONWriter);
var n,w: integer;
    FieldName: RawUTF8;
    joinedOR: boolean;
begin
  n := Length(Stmt.Where);
  if (n>1) and Stmt.Where[1].JoinedOR then begin
    for w := 2 to n-1 do
    if not Stmt.Where[w].JoinedOR then begin
      InternalLog('%.EngineList: mixed AND/OR not supported for [%]',
        [ClassType,SQL],sllError);
      exit;
    end;
    B.BSONDocumentBegin('$or',betArray); // e.g. {$or:[{quantity:{$lt:20}},{price:10}]}
    joinedOR := true;
  end else
    joinedOR := false;
  for w := 0 to n-1 do begin
    if joinedOR then
      B.BSONDocumentBegin(UInt32ToUtf8(w));
    with Stmt.Where[w] do begin
      FieldName := fStoredClassMapping^.FieldNameByIndex(Field-1)+SubField;
      if not B.BSONWriteQueryOperator(FieldName,NotClause,Operator,ValueVariant) then begin
        InternalLog('%.EngineList: operator % not supported for field [%] in [%]',
          [ClassType,ToText(Operator)^,FieldName,SQL],sllError);
        exit;
      end;
    end;
    if joinedOR then
      B.BSONDocumentEnd;
  end;
  if joinedOR then
    B.BSONDocumentEnd;
  B.BSONDocumentEnd;
end;
function ComputeQuery: boolean;
var B: TBSONWriter;
    n,i: integer;
begin // here we compute a BSON query, since it is the fastest
  result := false;
  if Stmt.SQLStatement='' then begin
    InternalLog('%.EngineList: Invalid SQL statement [%]',[ClassType,SQL],sllError);
    exit;
  end;
  if (Stmt.Where=nil) and (Stmt.OrderByField=nil) then begin // no WHERE clause
    result := true;
    SetVariantNull(Query); // void query -> returns all rows
    exit;
  end;
  if Stmt.WhereHasParenthesis then begin
    InternalLog('%.EngineList: parenthesis not supported in [%]',[ClassType,SQL],sllError);
    exit;
  end;
  B := TBSONWriter.Create(TRawByteStringStream);
  try
    B.BSONDocumentBegin;
    if Stmt.OrderByField<>nil then begin
      B.BSONDocumentBegin('$query');
      AddWhereClause(B);
      n := high(Stmt.OrderByField);
      if (n=0) and (Stmt.OrderByField[0]>0) and (Stmt.Limit=0) and
         (Stmt.Offset=0) and (fStoredClassRecordProps.Fields.List[
          Stmt.OrderByField[0]-1].SQLFieldType in [sftAnsiText,sftUTF8Text]) then
        TextOrderByField := fStoredClassMapping^.FieldNameByIndex(Stmt.OrderByField[0]-1) else
      if n>=0 then begin
        B.BSONDocumentBegin('$orderby');
        for i := 0 to n do
          B.BSONWrite(fStoredClassMapping^.FieldNameByIndex(Stmt.OrderByField[i]-1),ORDERBY_FIELD[Stmt.OrderByDesc]);
        B.BSONDocumentEnd;
      end;
      B.BSONDocumentEnd;
    end else
      AddWhereClause(B);
    B.ToBSONVariant(Query);
  finally
    B.Free;
  end;
  result := true; // indicates success
end;
procedure SetCount(aCount: integer);
begin
  result := FormatUTF8('[{"Count(*)":%}]'#$A,[aCount]);
  ResCount := 1;
end;
procedure ComputeAggregate;
type TFunc = (funcMax,funcMin,funcAvg,funcSum,funcCount);
const FUNCT: array[TFunc] of RawUTF8 = ('$max','$min','$avg','$sum','$sum');
var i: integer;
    func: TFunc;
    distinct: integer;
    B: TBSONWriter;
    distinctName,name,value: RawUTF8;
begin
  distinct := -1;
  for i := 0 to high(Stmt.Select) do
    if IdemPropNameU(Stmt.Select[i].FunctionName,'distinct') then
      if distinct>=0 then begin
        InternalLog('%.EngineList: distinct() only allowed once in [%]',
          [ClassType,SQL],sllError);
        exit;
      end else begin
      distinct := Stmt.Select[i].Field;
      distinctName := fStoredClassMapping^.FieldNameByIndex(distinct-1);
    end;
  B := TBSONWriter.Create(TRawByteStringStream);
  try
    B.BSONDocumentBegin;
   if Stmt.Where<>nil then begin
      B.BSONDocumentBeginInArray('$match');
      AddWhereClause(B);
    end;
    B.BSONDocumentBeginInArray('$group');
    if distinct>=0 then begin
      for i := 0 to high(Stmt.GroupByField) do
        if Stmt.GroupByField[i]<>distinct then begin
          InternalLog('%.EngineList: Distinct(%) expected GROUP BY % in [%]',
            [ClassType,distinctName,distinctName,SQL],sllError);
          exit;
        end;
      B.BSONWrite('_id','$'+distinctName);
    end else
    if length(Stmt.GroupByField)=0 then
      B.BSONWrite('_id',betNull) else begin
      B.BSONDocumentBegin('_id');
      for i := 0 to high(Stmt.GroupByField) do begin
        name := fStoredClassMapping^.FieldNameByIndex(Stmt.GroupByField[i]-1);
        B.BSONWrite(name,'$'+name);
      end;
      B.BSONDocumentEnd;
    end;
    for i := 0 to high(Stmt.Select) do
    with Stmt.Select[i] do begin
      if FunctionKnown=funcDistinct then
        continue;
      func := TFunc(FindPropName(['max','min','avg','sum','count'],FunctionName));
      if ord(func)<0 then begin
        InternalLog('%.EngineList: unexpected function %() in [%]',
          [ClassType,FunctionName,SQL],sllError);
        exit;
      end;
      B.BSONDocumentBegin('f'+UInt32ToUTF8(i));
      if func=funcCount then
        B.BSONWrite(FUNCT[func],1) else
        B.BSONWrite(FUNCT[func],'$'+fStoredClassMapping^.FieldNameByIndex(Field-1));
      B.BSONDocumentEnd;
    end;
    B.BSONDocumentEnd;
    if Stmt.OrderByField<>nil then begin
      if (length(Stmt.OrderByField)<>1) or (Stmt.OrderByField[0]<>distinct) then begin
        InternalLog('%.EngineList: ORDER BY should match Distinct(%) in [%]',
          [ClassType,distinctName,SQL],sllError);
        exit;
      end;
      B.BSONDocumentBeginInArray('$sort');
      B.BSONWrite('_id',ORDERBY_FIELD[Stmt.OrderByDesc]);
      B.BSONDocumentEnd;
    end;
    B.BSONDocumentBeginInArray('$project');
    B.BSONWrite('_id',0);
    for i := 0 to high(Stmt.Select) do
    with Stmt.Select[i] do begin
      if Alias<>'' then // name is the output ODM TSQLRecord field
        name := Alias else begin
        if Field=0 then
          name := 'RowID' else
          name := fStoredClassRecordProps.Fields.List[Field-1].Name;
        if SubField<>'' then // 'field.subfield1.subfield2'
          name := name+SubField;
        if FunctionName<>'' then
          if FunctionKnown=funcDistinct then begin
            B.BSONWrite(name,'$_id');
            continue;
          end else
            name := FunctionName+'('+name+')';
      end;
      value := '$f'+UInt32ToUTF8(i);
      if ToBeAdded<>0 then begin
        B.BSONDocumentBegin(name);
        B.BSONDocumentBegin('$add',betArray);
        B.BSONWrite('0',value);
        B.BSONWrite('1',ToBeAdded);
        B.BSONDocumentEnd(2);
      end else
        B.BSONWrite(name,value);
    end;
    B.BSONDocumentEnd(3);
    B.ToBSONVariant(Query,betArray);
  finally
    B.Free;
  end;
  result := fCollection.AggregateJSONFromVariant(Query);
end;
var W: TJSONSerializer;
    MS: TRawByteStringStream;
    Res: TBSONDocument;
    limit: PtrInt;
    extFieldNames, subFields: TRawUTF8DynArray;
    bits: TSQLFieldBits;
    withID: boolean;
    Projection: variant;
begin // same logic as in TSQLRestStorageInMemory.EngineList()
  result := ''; // indicates error occurred
  ResCount := 0;
  if self=nil then
    exit;
  InternalLog(SQL,sllSQL);
  StorageLock(false,'EngineList');
  try
    if IdemPropNameU(fBasicSQLCount,SQL) then
      SetCount(TableRowCount(fStoredClass)) else
    if IdemPropNameU(fBasicSQLHasRows[false],SQL) or
       IdemPropNameU(fBasicSQLHasRows[true],SQL) then
      if TableRowCount(fStoredClass)=0 then begin
        result := '{"fieldCount":1,"values":["RowID"]}'#$A;
        ResCount := 0;
      end else begin // return one row with fake ID=1
        result := '[{"RowID":1}]'#$A;
        ResCount := 1;
      end else begin
      Stmt := TSynTableStatement.Create(SQL,
        fStoredClassRecordProps.Fields.IndexByName,
        fStoredClassRecordProps.SimpleFieldsBits[soSelect]);
      try
        if (Stmt.SQLStatement='') or // parsing failed
          not IdemPropNameU(Stmt.TableName,fStoredClassRecordProps.SQLTableName) then
          // invalid request -> return '' to mark error
          exit;
        if Stmt.SelectFunctionCount<>0 then
          if (length(Stmt.Select)=1) and (Stmt.Select[0].Alias='') and
             IdemPropNameU(Stmt.Select[0].FunctionName,'count') then
          if Stmt.Where=nil then
            // was "SELECT Count(*) FROM TableName;"
            SetCount(TableRowCount(fStoredClass)) else
            // was "SELECT Count(*) FROM TableName WHERE ..."
            if ComputeQuery then
              SetCount(fCollection.FindCount(Query)) else
              exit else
            // e.g. SELECT Distinct(Age),max(RowID) FROM TableName GROUP BY Age
            ComputeAggregate else
        // save rows as JSON from returned BSON
        if ComputeQuery then begin
          if Stmt.HasSelectSubFields then
            SetLength(subFields,fStoredClassRecordProps.Fields.Count+1);
          Stmt.SelectFieldBits(bits,withID,pointer(subFields));
          BSONProjectionSet(Projection,withID,bits,@extFieldNames,subFields);
          if Stmt.Limit=0 then
            limit := maxInt else
            limit := Stmt.Limit;
          Res := fCollection.FindBSON(Query,Projection,limit,Stmt.Offset);
          MS := TRawByteStringStream.Create;
          try
            W := fStoredClassRecordProps.CreateJSONWriter(MS,
              ForceAJAX or (Owner=nil) or not Owner.NoAJAXJSON,withID,bits,0);
            try
              ResCount := GetJSONValues(Res,extFieldNames,W);
              result := MS.DataString;
            finally
              W.Free;
            end;
          finally
            MS.Free;
          end;
          if TextOrderByField<>'' then
            // $orderby is case sensitive with MongoDB -> client-side sort
            with TSQLTableJSON.CreateFromTables(
              [fStoredClass],SQL,pointer(result),length(result)) do
            try
              SortFields(FieldIndex(TextOrderByField),not Stmt.OrderByDesc,nil,sftUTF8Text);
              result := GetJSONValues(W.Expand);
            finally
              Free;
            end;
        end;
      finally
        Stmt.Free;
      end;
    end;
  finally
    StorageUnLock;
  end;
  if ReturnedRowCount<>nil then
    ReturnedRowCount^ := ResCount;
end;

function TSQLRestStorageMongoDB.EngineExecute(const aSQL: RawUTF8): boolean;
begin
  result := false; // it is a NO SQL engine, we said! :)
end;

function TSQLRestStorageMongoDB.InternalBatchStart(
  Method: TSQLURIMethod; BatchOptions: TSQLRestBatchOptions): boolean;
begin
  result := false; // means BATCH mode not supported
  if method in [mPOST,mDELETE] then begin
    StorageLock(true,'InternalBatchStart'); // protected by try..finally in TSQLRestServer.RunBatch
    try
      if (fBatchMethod<>mNone) or (fBatchWriter<>nil) then
        raise EORMException.CreateUTF8('%.InternalBatchStop should have been called',[self]);
      fBatchIDsCount := 0;
      fBatchMethod := Method;
      case Method of
      mPOST: // POST=ADD=INSERT -> EngineAdd() will add to fBatchWriter
        fBatchWriter := TBSONWriter.Create(TRawByteStringStream);
      //mDELETE: // EngineDelete() will add deleted ID to fBatchIDs[]
      end;
      result := true; // means BATCH mode is supported
    finally
      if not result then // release lock on error
        StorageUnLock;
    end;
  end;
end;

procedure TSQLRestStorageMongoDB.InternalBatchStop;
var docs: TBSONDocument;
begin
  try
    case fBatchMethod of
    mPOST: begin // Add/Insert
      if fBatchWriter.TotalWritten=0 then
        exit; // nothing to add
      fBatchWriter.ToBSONDocument(docs);
      fCollection.Insert(docs);
    end;
    mDELETE: begin
      SetLength(fBatchIDs,fBatchIDsCount);
      fCollection.Remove(BSONVariant(
        ['_id',BSONVariant(['$in',BSONVariantFromInt64s(TInt64DynArray(fBatchIDs))])]));
    end;
    else
      raise EORMException.CreateUTF8('%.InternalBatchStop(%) with BatchMethod=%',
        [self,StoredClass,ToText(fBatchMethod)^]);
    end;
  finally
    FreeAndNil(fBatchWriter);
    fBatchIDs := nil;
    fBatchIDsCount := 0;
    fBatchMethod := mNone;
    StorageUnLock;
  end;
end;


end.
