unit Alcinoe.MongoDB.Client;

interface

{$I Alcinoe.inc}

uses
  System.Classes,
  System.SysUtils,
  Alcinoe.MongoDB.Wrapper,
  Alcinoe.Common,
  Alcinoe.JSONDoc;


type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  EALMongoDBException = class(EALException)
  private
    FDomain: uint32_t;
    FCode: uint32_t;
  public
    constructor Create(const ADomain: uint32_t; const ACode: uint32_t; const AMessage: AnsiString); overload;
    constructor Create(const ADomain: uint32_t; const ACode: uint32_t; const AMessage: String); overload;
    constructor CreateFmt(const ADomain: uint32_t; const ACode: uint32_t; const AMessage: ansistring; const Args: array of const); overload;
    constructor CreateFmt(const ADomain: uint32_t; const ACode: uint32_t; const AMessage: string; const Args: array of const); overload;
    property Domain: uint32_t read FDomain;
    property Code: uint32_t read FCode;
  end;

  {----------------------}
  TALMongoDBClient = class
  public
    type
      // -----------------
      // TDocumentCallback
      TOnDocumentObjProc = procedure(Sender: TObject; var ADoc: TALJsonNodeA; const AContext: Pointer) of object;
      TOnDocumentRefProc = reference to procedure(Sender: TObject; var ADoc: TALJsonNodeA; const AContext: Pointer);
  private
    type
      // ---------------------------
      // TOnDocumentToRefProcContext
      TOnDocumentToRefProcContext = class
        OnDocumentRefProc: TOnDocumentRefProc;
        Context: Pointer;
      end;
  private
    // A given thread uses at most one TALMongoDBClient at a time.
    // Therefore, it is safe that FClient and FSession are class threadvars
    // (per-thread globals) rather than being stored per instance.
    class threadvar FClient: Pmongoc_client_t;
    class threadvar FSession: Pmongoc_client_session_t;
  private
    FPool: Pmongoc_client_pool_t;
    procedure OnDocumentToRefProc(Sender: TObject; var ADoc: TALJsonNodeA; const AContext: Pointer);
    procedure OnDocumentToJSon(Sender: TObject; var ADoc: TALJsonNodeA; const AContext: Pointer);
  protected
    function FindAndModify(
               const ADatabaseName: AnsiString;
               const ACollectionName: AnsiString;
               const AQuery: AnsiString;
               const ASort: AnsiString;
               const AUpdateJSON: AnsiString;
               const AUpdateBSON: TBytes;
               const AFields: AnsiString;
               const ARemove: ByteBool;
               const AUpsert: ByteBool;
               const ANew: ByteBool): TALJsonNodeA; overload; virtual;
    function InsertOne(
               const ADatabaseName: AnsiString;
               const ACollectionName: AnsiString;
               const ADocumentJSON: AnsiString;
               const ADocumentBSON: TBytes;
               const AOpts: AnsiString = ''): TALJsonNodeA; overload; virtual;
    function InsertMany(
               const ADatabaseName: AnsiString;
               const ACollectionName: AnsiString;
               const ADocumentJSONs: TArray<AnsiString>;
               const ADocumentBSONs: TArray<TBytes>;
               const AOpts: AnsiString = ''): TALJsonNodeA; overload; virtual;
    function UpdateOne(
               const ADatabaseName: AnsiString;
               const ACollectionName: AnsiString;
               const ASelector: AnsiString;
               const AUpdateJSON: AnsiString;
               const AUpdateBSON: TBytes;
               const AOpts: AnsiString = ''): TALJsonNodeA; overload; virtual;
     function UpdateMany(
               const ADatabaseName: AnsiString;
               const ACollectionName: AnsiString;
               const ASelector: AnsiString;
               const AUpdateJSON: AnsiString;
               const AUpdateBSON: TBytes;
               const AOpts: AnsiString = ''): TALJsonNodeA; overload; virtual;
    function ReplaceOne(
               const ADatabaseName: AnsiString;
               const ACollectionName: AnsiString;
               const ASelector: AnsiString;
               const AReplacementJSON: AnsiString;
               const AReplacementBSON: TBytes;
               const AOpts: AnsiString = ''): TALJsonNodeA; overload; virtual;
  public
    /// <summary>
    ///   Creates a MongoDB client pool from a MongoDB connection URI.
    /// </summary>
    /// <param name="AUri">
    ///   MongoDB connection string (e.g. <c>mongodb://host:27017</c> or <c>mongodb://host:27017/mydb</c>).
    ///   The optional <c>/mydb</c> part is the URI's default database (it does not restrict access; Find()
    ///   still uses ADatabaseName/ACollectionName). URI options are provided via the query string.
    /// </param>
    /// <remarks>
    ///   For URI syntax and all supported options, see:
    ///   https://mongoc.org/libmongoc/current/mongoc_uri_t.html
    /// </remarks>
    constructor Create(const AUri: AnsiString); virtual;
    destructor Destroy; override;
    procedure StartTransaction(
                const ASessionOpts: AnsiString = '';
                const AtransactionOpts: AnsiString = '');
    function CommitTransaction: TALJsonNodeA;
    procedure AbortTransaction;
    procedure Find(
                const AOnDocument: TOnDocumentObjProc;
                const AContext: Pointer;
                const ADatabaseName: AnsiString;
                const ACollectionName: AnsiString;
                const Afilter: AnsiString;
                const AOpts: AnsiString = '';
                const AReadPrefs: AnsiString = ''); overload; virtual;
    procedure Find(
                const AOnDocument: TOnDocumentRefProc;
                const AContext: Pointer;
                const ADatabaseName: AnsiString;
                const ACollectionName: AnsiString;
                const Afilter: AnsiString;
                const AOpts: AnsiString = '';
                const AReadPrefs: AnsiString = ''); overload;
    function Find(
               const ADatabaseName: AnsiString;
               const ACollectionName: AnsiString;
               const Afilter: AnsiString;
               const AOpts: AnsiString = '';
               const AReadPrefs: AnsiString = ''): TALJsonNodeA; overload;
    function FindAndModify(
               const ADatabaseName: AnsiString;
               const ACollectionName: AnsiString;
               const AQuery: AnsiString;
               const ASort: AnsiString;
               const AUpdate: AnsiString;
               const AFields: AnsiString;
               const ARemove: ByteBool;
               const AUpsert: ByteBool;
               const ANew: ByteBool): TALJsonNodeA; overload; inline;
    function FindAndModify(
               const ADatabaseName: AnsiString;
               const ACollectionName: AnsiString;
               const AQuery: AnsiString;
               const ASort: AnsiString;
               const AUpdate: TBytes;
               const AFields: AnsiString;
               const ARemove: ByteBool;
               const AUpsert: ByteBool;
               const ANew: ByteBool): TALJsonNodeA; overload; inline;
    function InsertOne(
               const ADatabaseName: AnsiString;
               const ACollectionName: AnsiString;
               const ADocument: AnsiString;
               const AOpts: AnsiString = ''): TALJsonNodeA; overload; inline;
    function InsertOne(
               const ADatabaseName: AnsiString;
               const ACollectionName: AnsiString;
               const ADocument: TBytes;
               const AOpts: AnsiString = ''): TALJsonNodeA; overload; inline;
    function InsertMany(
               const ADatabaseName: AnsiString;
               const ACollectionName: AnsiString;
               const ADocuments: TArray<AnsiString>;
               const AOpts: AnsiString = ''): TALJsonNodeA; overload; inline;
    function InsertMany(
               const ADatabaseName: AnsiString;
               const ACollectionName: AnsiString;
               const ADocuments: TArray<TBytes>;
               const AOpts: AnsiString = ''): TALJsonNodeA; overload; inline;
    function UpdateOne(
               const ADatabaseName: AnsiString;
               const ACollectionName: AnsiString;
               const ASelector: AnsiString;
               const AUpdate: AnsiString;
               const AOpts: AnsiString = ''): TALJsonNodeA; overload; inline;
    function UpdateOne(
               const ADatabaseName: AnsiString;
               const ACollectionName: AnsiString;
               const ASelector: AnsiString;
               const AUpdate: TBytes;
               const AOpts: AnsiString = ''): TALJsonNodeA; overload; inline;
    function UpdateMany(
               const ADatabaseName: AnsiString;
               const ACollectionName: AnsiString;
               const ASelector: AnsiString;
               const AUpdate: AnsiString;
               const AOpts: AnsiString = ''): TALJsonNodeA; overload; inline;
    function UpdateMany(
               const ADatabaseName: AnsiString;
               const ACollectionName: AnsiString;
               const ASelector: AnsiString;
               const AUpdate: TBytes;
               const AOpts: AnsiString = ''): TALJsonNodeA; overload; inline;
    function ReplaceOne(
               const ADatabaseName: AnsiString;
               const ACollectionName: AnsiString;
               const ASelector: AnsiString;
               const AReplacement: AnsiString;
               const AOpts: AnsiString = ''): TALJsonNodeA; overload; inline;
    function ReplaceOne(
               const ADatabaseName: AnsiString;
               const ACollectionName: AnsiString;
               const ASelector: AnsiString;
               const AReplacement: TBytes;
               const AOpts: AnsiString = ''): TALJsonNodeA; overload; inline;
    function DeleteOne(
               const ADatabaseName: AnsiString;
               const ACollectionName: AnsiString;
               const ASelector: AnsiString;
               const AOpts: AnsiString = ''): TALJsonNodeA; virtual;
    function DeleteMany(
               const ADatabaseName: AnsiString;
               const ACollectionName: AnsiString;
               const ASelector: AnsiString;
               const AOpts: AnsiString = ''): TALJsonNodeA; virtual;
  end;

  {---------------------------------------------}
  TALMongoDBChangeStreamListener = class(TThread)
  public
    type
      TChangeEvent = procedure(Sender: TObject; const AChangeDoc: TALJsonNodeA) of object;
      TErrorEvent = procedure(Sender: TObject; const AError: Exception) of object;
  private
    FUri: AnsiString;
    FDatabaseName: AnsiString;
    FCollectionName: AnsiString;
    FPipeline: AnsiString;
    FOpts: AnsiString;
    FOnChange: TChangeEvent;
    FOnError: TErrorEvent;
  protected
    procedure Execute; override;
    procedure DoChange(const AChangeDoc: TALJsonNodeA); virtual;
    procedure DoError(const AError: Exception); virtual;
  public
    constructor Create; virtual;
    property Uri: AnsiString read FUri write FUri;
    property DatabaseName: AnsiString read FDatabaseName write FDatabaseName;
    property CollectionName: AnsiString read FCollectionName write FCollectionName;
    property Pipeline: AnsiString read FPipeline write FPipeline;
    property Opts: AnsiString read FOpts write FOpts;
    property Onchange: TChangeEvent read FOnChange write FOnChange;
    property OnError: TErrorEvent read FOnError write FOnError;
  end;

implementation

uses
  Winapi.Windows;

{*****************************************************************************************************************}
constructor EALMongoDBException.Create(const ADomain: uint32_t; const ACode: uint32_t; const AMessage: AnsiString);
begin
  inherited create(AMessage);
  FDomain := ADomain;
  FCode := ACode;
end;

{*************************************************************************************************************}
constructor EALMongoDBException.Create(const ADomain: uint32_t; const ACode: uint32_t; const AMessage: String);
begin
  inherited create(AMessage);
  FDomain := ADomain;
  FCode := ACode;
end;

{************************************************************************************************************************************************}
constructor EALMongoDBException.CreateFmt(const ADomain: uint32_t; const ACode: uint32_t; const AMessage: ansistring; const Args: array of const);
begin
  inherited CreateFmt(AMessage, Args);
  FDomain := ADomain;
  FCode := ACode;
end;

{********************************************************************************************************************************************}
constructor EALMongoDBException.CreateFmt(const ADomain: uint32_t; const ACode: uint32_t; const AMessage: string; const Args: array of const);
begin
  inherited CreateFmt(AMessage, Args);
  FDomain := ADomain;
  FCode := ACode;
end;

{********************************************************}
procedure ALRaiseMongoDBError(const AError: bson_error_t);
begin
  var LMsg: AnsiString := AnsiString(PAnsiChar(@AError.message[0]));
  if LMsg = '' then LMsg := 'MongoDB operation failed';
  raise EALMongoDBException.CreateFmt(
          AError.domain,
          AError.code,
          '%s (domain=%d, code=%d)',
          [LMsg, AError.domain, AError.code])
end;

{**********************************}
function ALMakeBsonPtrFromBSONBytes(
           const ASource: TBytes;
           const ABufferBson: Pbson_t;
           out AIsAllocatedResult: Boolean;
           const AReadOnly: Boolean = True): Pbson_t;
begin
  if length(ASource) > 0 then begin
    if AReadOnly then begin
      if not bson_init_static(
               ABufferBson, // b: Pbson_t;
               @ASource[0], // data: Puint8_t;
               size_t(Length(ASource))) then // length: size_t
        raise Exception.Create('bson_init_static failed');
      Result := ABufferBson;
      AIsAllocatedResult := False;
    end
    else begin
      Result := bson_new_from_data(
                  @ASource[0], // data: Puint8_t;
                  size_t(Length(ASource))); // length: size_t
      if Result = nil then raise Exception.Create('bson_new_from_data failed');
      AIsAllocatedResult := True;
    end;
  end
  else begin
    bson_init(ABufferBson);
    Result := ABufferBson;
    AIsAllocatedResult := False;
  end;
end;

{***********************************}
function ALMakeBsonPtrFromJSONString(
           const ASource: AnsiString;
           const ABufferBson: Pbson_t;
           out AIsAllocatedResult: Boolean;
           const AReadOnly: Boolean = True): Pbson_t;
begin
  if length(ASource) > 0 then begin
    var LError: bson_error_t;
    Result := bson_new_from_json(
                @ASource[low(ASource)], // data: Puint8_t;
                ssize_t(Length(ASource)), // len: ssize_t;
                @LError); // error: Pbson_error_t
    if Result = nil then ALRaiseMongoDBError(LError);
    AIsAllocatedResult := True;
  end
  else begin
    bson_init(ABufferBson);
    Result := ABufferBson;
    AIsAllocatedResult := False;
  end;
end;

{**********************************************************************************}
function ALMakeReadPrefsFromJSonDoc(const ADoc: TALJsonNodeA): Pmongoc_read_prefs_t;
begin

  //
  // {
  //   "mode": "primary_preferred",
  //   "tags": [
  //     { "dc": "paris", "use": "reporting" },
  //     { "dc": "paris" },
  //     { }
  //   ],
  //   "maxStalenessSeconds": 120
  // }
  //

  // mode
  var LMode := ADoc.GetChildValueText('mode', 'primary');
  if LMode = 'primary' then Result := mongoc_read_prefs_new(MONGOC_READ_PRIMARY)
  else if LMode = 'primary_preferred' then Result := mongoc_read_prefs_new(MONGOC_READ_PRIMARY_PREFERRED)
  else if LMode = 'secondary' then Result := mongoc_read_prefs_new(MONGOC_READ_SECONDARY)
  else if LMode = 'secondary_preferred' then Result := mongoc_read_prefs_new(MONGOC_READ_SECONDARY_PREFERRED)
  else if LMode = 'nearest' then Result := mongoc_read_prefs_new(MONGOC_READ_NEAREST)
  else raise Exception.CreateFmt('Invalid read preference mode: %s', [LMode]);
  if Result = nil then raise Exception.Create('mongoc_read_prefs_new failed');

  try

    // tags
    var LTagsNode := ADoc.ChildNodes.FindNode('tags');
    if (LTagsNode <> nil) then begin
      if not (LTagsNode is TALJSONArrayNodeA) then
        raise Exception.Create('Tags must be an array');
      for var I := 0 to LTagsNode.ChildNodes.Count - 1 do begin
        var LBsonRec: bson_t;
        var LBsonIsAllocated: Boolean;
        var LBsonBytes: TBytes := LTagsNode.ChildNodes[I].BSON;
        var LBsonPtr := ALMakeBsonPtrFromBSONBytes(LBsonBytes, @LBsonRec, LBsonIsAllocated);
        try
          mongoc_read_prefs_add_tag(Result, LBsonPtr);
        finally
          if LBsonIsAllocated then
            bson_destroy(LBsonPtr);
        end;
      end;
    end;

    // maxStalenessSeconds
    var LMaxStalenessNode := ADoc.ChildNodes.FindNode('maxStalenessSeconds');
    if LMaxStalenessNode <> nil then
      mongoc_read_prefs_set_max_staleness_seconds(Result, LMaxStalenessNode.int64);

  except
    mongoc_read_prefs_destroy(Result);
    raise;
  end;

end;

{**********************************************************************************}
function ALMakeReadPrefsFromJSONString(const ASource: AnsiString): Pmongoc_read_prefs_t;
begin
  var LDoc := TALJSONDocumentA.Create;
  try
    if ASource <> '' then LDoc.LoadFromJSONString(ASource);
    result := ALMakeReadPrefsFromJSonDoc(LDoc);
  finally
    ALFreeAndNil(LDoc);
  end;
end;

{**************************************************************************************}
function ALMakeReadConcernFromJSonDoc(const ADoc: TALJsonNodeA): Pmongoc_read_concern_t;
begin

  //
  // {
  //   "level": "majority"
  // }
  //

  Result := mongoc_read_concern_new;
  if Result = nil then raise Exception.Create('mongoc_read_concern_new failed');
  try

    // level
    var LLevelNode := ADoc.ChildNodes.FindNode('level');
    if LLevelNode <> nil then begin
      var LLevel: AnsiString := LLevelNode.Text;
      if not mongoc_read_concern_set_level(Result, PAnsiChar(LLevel)) then
        raise Exception.CreateFmt('Invalid read concern level: %s', [LLevel]);
    end;

  except
    mongoc_read_concern_destroy(Result);
    raise;
  end;

end;

{**************************************************************************************}
function ALMakeReadConcernFromJSONString(const ASource: AnsiString): Pmongoc_read_concern_t;
begin
  var LDoc := TALJSONDocumentA.Create;
  try
    if ASource <> '' then LDoc.LoadFromJSONString(ASource);
    result := ALMakeReadConcernFromJSonDoc(LDoc);
  finally
    ALFreeAndNil(LDoc);
  end;
end;

{****************************************************************************************}
function ALMakeWriteConcernFromJSonDoc(const ADoc: TALJsonNodeA): Pmongoc_write_concern_t;
begin

  //
  // {
  //   "journal": true,
  //   "w": 3,
  //   "wtag": "dc_paris",
  //   "wtimeout": 5000,
  //   "wmajority": 5000
  // }
  //

  Result := mongoc_write_concern_new;
  if Result = nil then raise Exception.Create('mongoc_write_concern_new failed');
  try

    // journal
    var LJournalNode := ADoc.ChildNodes.FindNode('journal');
    if LJournalNode <> nil then
      mongoc_write_concern_set_journal(Result, LJournalNode.bool);

    // w
    var LWNode := ADoc.ChildNodes.FindNode('w');
    if LWNode <> nil then
      mongoc_write_concern_set_w(Result, LWNode.int32);

    // wtag
    var LWTagNode := ADoc.ChildNodes.FindNode('wtag');
    if LWTagNode <> nil then begin
      var LW: AnsiString := LWTagNode.Text;
      mongoc_write_concern_set_wtag(Result, PAnsiChar(LW));
    end;

    // wtimeout
    var LWTimeoutNode := ADoc.ChildNodes.FindNode('wtimeout');
    if LWTimeoutNode <> nil then
      mongoc_write_concern_set_wtimeout_int64(Result, LWTimeoutNode.int64);

    // wmajority
    var LWMajorityNode := ADoc.ChildNodes.FindNode('wmajority');
    if LWMajorityNode <> nil then
      mongoc_write_concern_set_wmajority(Result, LWMajorityNode.int32);

  except
    mongoc_write_concern_destroy(Result);
    raise;
  end;

end;

{****************************************************************************************}
function ALMakeWriteConcernFromJSONString(const ASource: AnsiString): Pmongoc_write_concern_t;
begin
  var LDoc := TALJSONDocumentA.Create;
  try
    if ASource <> '' then LDoc.LoadFromJSONString(ASource);
    result := ALMakeWriteConcernFromJSonDoc(LDoc);
  finally
    ALFreeAndNil(LDoc);
  end;
end;

{*********************************************************************************************}
function ALMakeTransactionOptsFromJSonDoc(const ADoc: TALJsonNodeA): Pmongoc_transaction_opt_t;
begin

  //
  // {
  //   "readConcern": { "level": "snapshot" },
  //   "writeConcern": { "w": 3, "wtimeout": 5000 },
  //   "readPrefs": { "mode": "primary" },
  //   "maxCommitTimeMS": 30000
  // }
  //

  Result := mongoc_transaction_opts_new();
  if Result = nil then raise Exception.Create('mongoc_transaction_opts_new failed');
  try

    // readConcern
    var LReadConcernNode := ADoc.ChildNodes.FindNode('readConcern');
    if LReadConcernNode <> nil then begin
      var LReadConcern := ALMakeReadConcernFromJSonDoc(LReadConcernNode);
      if LReadConcern = nil then raise Exception.Create('ALMakeReadConcernFromJSonDoc failed');
      try
        mongoc_transaction_opts_set_read_concern(Result, LReadConcern);
      finally
        mongoc_read_concern_destroy(LReadConcern);
      end;
    end;

    // writeConcern
    var LWriteConcernNode := ADoc.ChildNodes.FindNode('writeConcern');
    if LWriteConcernNode <> nil then begin
      var LWriteConcern := ALMakeWriteConcernFromJSonDoc(LWriteConcernNode);
      if LWriteConcern = nil then raise Exception.Create('ALMakeWriteConcernFromJSonDoc failed');
      try
        mongoc_transaction_opts_set_write_concern(Result, LWriteConcern);
      finally
        mongoc_write_concern_destroy(LWriteConcern);
      end;
    end;

    // readPrefs
    var LReadPrefsNode := ADoc.ChildNodes.FindNode('readPrefs');
    if LReadPrefsNode <> nil then begin
      var LReadPrefs := ALMakeReadPrefsFromJSonDoc(LReadPrefsNode);
      if LReadPrefs = nil then raise Exception.Create('ALMakeReadPrefsFromJSonDoc failed');
      try
        mongoc_transaction_opts_set_read_prefs(Result, LReadPrefs);
      finally
        mongoc_read_prefs_destroy(LReadPrefs);
      end;
    end;

    // maxCommitTimeMS
    var LMaxCommitTimeNode := ADoc.ChildNodes.FindNode('maxCommitTimeMS');
    if LMaxCommitTimeNode <> nil then
      mongoc_transaction_opts_set_max_commit_time_ms(Result, LMaxCommitTimeNode.int64);

  except
    mongoc_transaction_opts_destroy(Result);
    raise;
  end;

end;

{*********************************************************************************************}
function ALMakeTransactionOptsFromJSONString(const ASource: AnsiString): Pmongoc_transaction_opt_t;
begin
  var LDoc := TALJSONDocumentA.Create;
  try
    if ASource <> '' then LDoc.LoadFromJSONString(ASource);
    result := ALMakeTransactionOptsFromJSonDoc(LDoc);
  finally
    ALFreeAndNil(LDoc);
  end;
end;

{*************************************************************************************}
function ALMakeSessionOptsFromJSonDoc(const ADoc: TALJsonNodeA): Pmongoc_session_opt_t;
begin

  //
  // {
  //   "causalConsistency": true,
  //   "snapshot": false,
  //   "defaultTransactionOpts": {
  //     "readConcern": { "level": "majority" },
  //     "writeConcern": { "w": 3 },
  //     "readPrefs": { "mode": "primary" }
  //   }
  // }
  //

  Result := mongoc_session_opts_new();
  if Result = nil then raise Exception.Create('mongoc_session_opts_new failed');
  try

    // causalConsistency
    var LCausalConsistencyNode := ADoc.ChildNodes.FindNode('causalConsistency');
    if LCausalConsistencyNode <> nil then
      mongoc_session_opts_set_causal_consistency(Result, LCausalConsistencyNode.Bool);

    // snapshot
    var LSnapshotNode := ADoc.ChildNodes.FindNode('snapshot');
    if LSnapshotNode <> nil then
      mongoc_session_opts_set_snapshot(Result, LSnapshotNode.Bool);

    // defaultTransactionOpts
    var LDefaultTransactionOptsNode := ADoc.ChildNodes.FindNode('defaultTransactionOpts');
    if LDefaultTransactionOptsNode <> nil then begin
      var LDefaultTransactionOpts := ALMakeTransactionOptsFromJSonDoc(LDefaultTransactionOptsNode);
      if LDefaultTransactionOpts = nil then raise Exception.Create('ALMakeTransactionOptsFromJSonDoc failed');
      try
        mongoc_session_opts_set_default_transaction_opts(Result, LDefaultTransactionOpts);
      finally
        mongoc_transaction_opts_destroy(LDefaultTransactionOpts);
      end;
    end;

  except
    mongoc_session_opts_destroy(Result);
    raise;
  end;

end;

{*************************************************************************************}
function ALMakeSessionOptsFromJSONString(const ASource: AnsiString): Pmongoc_session_opt_t;
begin
  var LDoc := TALJSONDocumentA.Create;
  try
    if ASource <> '' then LDoc.LoadFromJSONString(ASource);
    result := ALMakeSessionOptsFromJSonDoc(LDoc);
  finally
    ALFreeAndNil(LDoc);
  end;
end;

{**********************************************************}
constructor TALMongoDBClient.Create(const AUri: AnsiString);
begin
  inherited create;
  var LError: bson_error_t;
  var LMongoUri := mongoc_uri_new_with_error(
                     PAnsiChar(AUri), // uri_string: PAnsiChar;
                     @LError); // error: Pbson_error_t
  If LMongoUri = nil then ALRaiseMongoDBError(LError);
  Try
    FPool := mongoc_client_pool_new_with_error(LMongoUri, @LError);
    if FPool = nil then ALRaiseMongoDBError(LError);
  Finally
    mongoc_uri_destroy(LMongoUri);
  End;
end;

{**********************************}
destructor TALMongoDBClient.Destroy;
begin
  // All mongoc_client_t objects obtained from mongoc_client_pool_pop() from
  // pool must be pushed back onto the pool with mongoc_client_pool_push() prior
  // to calling mongoc_client_pool_destroy().
  // This method is NOT thread safe, and must only be called by one thread. It
  // should be called once the application is shutting down, and after all
  // other threads that use clients have been joined.
  mongoc_client_pool_destroy(FPool);
  inherited;
end;

{***************************************************************************************************************}
procedure TALMongoDBClient.OnDocumentToRefProc(Sender: TObject; var ADoc: TALJsonNodeA; const AContext: Pointer);
begin
  TOnDocumentToRefProcContext(AContext).OnDocumentRefProc(Sender, ADoc, TOnDocumentToRefProcContext(AContext).Context);
end;

{************************************************************************************************************}
procedure TALMongoDBClient.OnDocumentToJSon(Sender: TObject; var ADoc: TALJsonNodeA; const AContext: Pointer);
begin
  TALJSONArrayNodeA(AContext).ChildNodes.Add(ADoc);
  ADoc := Nil;
end;

{******************************************}
procedure TALMongoDBClient.StartTransaction(
            const ASessionOpts: AnsiString = '';
            const AtransactionOpts: AnsiString = '');
begin
  If (FClient <> nil) or
     (FSession <> nil) then
    Raise Exception.Create('Active transaction in progress. please commit it or abord it first');

  var LSession: Pmongoc_client_session_t;
  var LClient := mongoc_client_pool_try_pop(FPool);
  if LClient = nil then begin
    ALLog('TALMongoDBClient', 'Pool at max capacity; waiting for a free client.', TALLogType.WARN);
    LClient := mongoc_client_pool_pop(FPool);
    if LClient = nil then raise Exception.Create('mongoc_client_pool_pop failed');
  end;

  Try

    var LMongoSessionOpts: Pmongoc_session_opt_t;
    if ASessionOpts <> '' then LMongoSessionOpts := ALMakeSessionOptsFromJSONString(ASessionOpts)
    else LMongoSessionOpts := nil;
    try

      var LError: bson_error_t;
      LSession := mongoc_client_start_session (
                    LClient, // client: Pmongoc_client_t;
                    LMongoSessionOpts, // opts: Pmongoc_session_opt_t;
                    @LError); // error: Pbson_error_t
      if LSession = nil then ALRaiseMongoDBError(LError);

      try

        var LMongoTransactionOpts: Pmongoc_transaction_opt_t;
        if ATransactionOpts <> '' then LMongoTransactionOpts := ALMakeTransactionOptsFromJSONString(ATransactionOpts)
        else LMongoTransactionOpts := nil;
        try

          if not mongoc_client_session_start_transaction(
                   LSession, // session: Pmongoc_client_session_t;
                   LMongoTransactionOpts, // opts: Pmongoc_transaction_opt_t;
                   @LError) then
            ALRaiseMongoDBError(LError);

        finally
          if LMongoTransactionOpts <> nil then
            mongoc_transaction_opts_destroy(LMongoTransactionOpts);
        end;

      except
        mongoc_client_session_destroy(LSession);
        Raise;
      end;

    finally
      if LMongoSessionOpts <> nil then
        mongoc_session_opts_destroy(LMongoSessionOpts);
    end;

    FClient := LClient;
    FSession := LSession;

  Except
    mongoc_client_pool_push(FPool, LClient);
    Raise;
  End;
end;

{********************************************************}
function TALMongoDBClient.CommitTransaction: TALJsonNodeA;
begin
  var LClient := FClient;
  try
    var LSession := FSession;
    Try

      If (LClient = nil) or
         (LSession = nil) then
        Raise Exception.Create('No active transaction');

      var LReply: bson_t;
      bson_init(@LReply);
      var LPReply: Pbson_t := @LReply;
      var LError: bson_error_t;
      var LSuccess := mongoc_client_session_commit_transaction(
                        LSession, // session: Pmongoc_client_session_t;
                        LPReply, // reply: Pbson_t;
                        @LError); // error: Pbson_error_t
      try

        if not LSuccess then
          ALRaiseMongoDBError(LError);

        var LStream := TPointerStream.Create(bson_get_data(LPReply){Ptr}, LPReply^.len{Size}, true{ReadOnly});
        try
          Result := TALJSONDocumentA.CreateFromBSONStream(LStream);
        finally
          ALFreeAndNil(LStream);
        end;

      finally
        bson_destroy(LPReply);
      end;

    finally
      FSession := Nil;
      if LSession <> nil then
        mongoc_client_session_destroy(LSession);
    end;
  finally
    FClient := Nil;
    if LClient <> nil then
      mongoc_client_pool_push(FPool, LClient);
  end;
end;

{******************************************}
procedure TALMongoDBClient.AbortTransaction;
begin

  var LSession := FSession;
  if LSession <> nil then begin
    Try
      Try
        // Returns true if the transaction was aborted. Returns false and sets
        // error if there are invalid arguments, such as a session with no
        // transaction in progress. Network or server errors are ignored.
        var LError: bson_error_t;
        If not mongoc_client_session_abort_transaction(
                 LSession, // session: Pmongoc_client_session_t;
                 @LError) then // error: Pbson_error_t
          ALRaiseMongoDBError(LError);
      finally
        mongoc_client_session_destroy(LSession);
      end;
    except
      On E: Exception do begin
        // best-effort; do not raise on failure
        ALLog('TALMongoDBClient', E);
      end;
    end;
  end;

  var LClient := FClient;
  if LClient <> nil then begin
    try
      mongoc_client_pool_push(FPool, LClient);
    except
      On E: Exception do begin
        // best-effort; do not raise on failure
        ALLog('TALMongoDBClient', E);
      end;
    end;
  end;

  FClient := Nil;
  FSession := Nil;

end;

{******************************}
procedure TALMongoDBClient.Find(
            const AOnDocument: TOnDocumentObjProc;
            const AContext: Pointer;
            const ADatabaseName: AnsiString;
            const ACollectionName: AnsiString;
            const Afilter: AnsiString;
            const AOpts: AnsiString = '';
            const AReadPrefs: AnsiString = '');
begin

  var LOwnsClient: Boolean := False;
  var LClient := FClient;

  var LFilterBsonRec: bson_t;
  var LFilterBsonIsAllocated: Boolean;
  var LFilterBsonPtr := ALMakeBsonPtrFromJSONString(Afilter, @LFilterBsonRec, LFilterBsonIsAllocated);
  try

    var LOptsBsonRec: bson_t;
    var LOptsBsonIsAllocated := False;
    var LOptsBsonPtr: Pbson_t;
    if (AOpts <> '') or (LClient <> nil) then LOptsBsonPtr := ALMakeBsonPtrFromJSONString(AOpts, @LOptsBsonRec, LOptsBsonIsAllocated, LClient = nil{AReadOnly})
    else LOptsBsonPtr := nil;
    try

      var LMongoReadPrefs: Pmongoc_read_prefs_t;
      if AReadPrefs <> '' then LMongoReadPrefs := ALMakeReadPrefsFromJSONString(AReadPrefs)
      else LMongoReadPrefs := nil;
      try

        if LClient = nil then begin
          LClient := mongoc_client_pool_try_pop(FPool);
          if LClient = nil then begin
            ALLog('TALMongoDBClient', 'Pool at max capacity; waiting for a free client.', TALLogType.WARN);
            LClient := mongoc_client_pool_pop(FPool);
            if LClient = nil then raise Exception.Create('mongoc_client_pool_pop failed');
          end;
          LOwnsClient := True;
        end
        else begin
          var LSession := FSession;
          if LSession <> nil then begin
            var LError: bson_error_t;
            if not mongoc_client_session_append(
                     FSession, // client_session: Pmongoc_client_session_t;
                     LOptsBsonPtr, // opts: Pbson_t;
                     @LError) then // error: Pbson_error_t
              ALRaiseMongoDBError(LError);
            LOptsBsonIsAllocated := True;
          end;
        end;
        Try

          var LCollection := mongoc_client_get_collection(
                               LClient, // client: Pmongoc_client_t;
                               PAnsiChar(ADatabaseName), // db: PAnsiChar;
                               PAnsiChar(ACollectionName)); // collection: PAnsiChar
          if LCollection = nil then raise Exception.Create('mongoc_client_get_collection failed');
          try

            var LCursor := mongoc_collection_find_with_opts(
                             LCollection, // collection: Pmongoc_collection_t;
                             LFilterBsonPtr, // filter: Pbson_t;
                             LOptsBsonPtr, // opts: Pbson_t;
                             LMongoReadPrefs); // read_prefs: Pmongoc_read_prefs_t
            if LCursor = nil then raise Exception.Create('mongoc_collection_find_with_opts failed');
            try

              var LStream := TPointerStream.Create(nil{Ptr}, 0{Size}, true{ReadOnly});
              try

                var LPbson: Pbson_t;
                while mongoc_cursor_next(LCursor, @LPbson) do begin
                  if LPbson = nil then raise Exception.Create('mongoc_cursor_next failed');
                  LStream.SetPointer(bson_get_data(LPbson), LPbson^.len);
                  var LDoc := TALJSONDocumentA.CreateFromBSONStream(LStream);
                  try
                    AOnDocument(Self, LDoc, AContext);
                  finally
                    ALFreeAndNil(LDoc);
                  end;
                end;

                var LError: bson_error_t;
                if mongoc_cursor_error(LCursor, @LError) then
                  ALRaiseMongoDBError(LError);

              finally
                ALFreeAndNil(LStream);
              end;

            finally
              mongoc_cursor_destroy(LCursor);
            end;

          finally
            mongoc_collection_destroy(LCollection);
          end;

        Finally
          if LOwnsClient then
            mongoc_client_pool_push(FPool, LClient);
        End;

      finally
        if LMongoReadPrefs <> nil then
          mongoc_read_prefs_destroy(LMongoReadPrefs);
      end;

    finally
      if LOptsBsonIsAllocated then
        bson_destroy(LOptsBsonPtr);
    end;

  finally
    if LFilterBsonIsAllocated then
      bson_destroy(LFilterBsonPtr);
  end;

end;

{******************************}
procedure TALMongoDBClient.Find(
            const AOnDocument: TOnDocumentRefProc;
            const AContext: Pointer;
            const ADatabaseName: AnsiString;
            const ACollectionName: AnsiString;
            const Afilter: AnsiString;
            const AOpts: AnsiString = '';
            const AReadPrefs: AnsiString = '');
begin
  var LOnDocumentToRefProcContext := TOnDocumentToRefProcContext.Create;
  try
    LOnDocumentToRefProcContext.OnDocumentRefProc := AOnDocument;
    LOnDocumentToRefProcContext.Context := AContext;
    Find(
      OnDocumentToRefProc, // const OnDocument: TOnDocumentObjProc;
      LOnDocumentToRefProcContext, // const AContext: Pointer;
      ADatabaseName, // const ADatabaseName: AnsiString;
      ACollectionName, // const ACollectionName: AnsiString;
      Afilter, // const Afilter: AnsiString;
      AOpts, // const AOpts: AnsiString = '';
      AReadPrefs); // const AReadPrefs: AnsiString = ''
  finally
    ALFreeAndNil(LOnDocumentToRefProcContext);
  end;
end;

{*****************************}
function TALMongoDBClient.Find(
           const ADatabaseName: AnsiString;
           const ACollectionName: AnsiString;
           const Afilter: AnsiString;
           const AOpts: AnsiString = '';
           const AReadPrefs: AnsiString = ''): TALJsonNodeA;
begin
  Result := TALJSONArrayNodeA.Create;
  try

    Find(
      OnDocumentToJSon, // const OnDocument: TOnDocumentObjProc;
      Result, // const AContext: Pointer;
      ADatabaseName, // const ADatabaseName: AnsiString;
      ACollectionName, // const ACollectionName: AnsiString;
      Afilter, // const Afilter: AnsiString;
      AOpts, // const AOpts: AnsiString = '';
      AReadPrefs); // const AReadPrefs: AnsiString = ''

  Except
    ALFreeAndNil(Result);
    Raise;
  end;
end;

{**************************************}
function TALMongoDBClient.FindAndModify(
           const ADatabaseName: AnsiString;
           const ACollectionName: AnsiString;
           const AQuery: AnsiString;
           const ASort: AnsiString;
           const AUpdateJSON: AnsiString;
           const AUpdateBSON: TBytes;
           const AFields: AnsiString;
           const ARemove: ByteBool;
           const AUpsert: ByteBool;
           const ANew: ByteBool): TALJsonNodeA;
begin

  var LOwnsClient: Boolean := False;
  var LClient := FClient;

  var LOpts := mongoc_find_and_modify_opts_new();
  if LOpts = nil then raise Exception.Create('mongoc_find_and_modify_opts_new failed');
  try

    var LQueryBsonRec: bson_t;
    var LQueryBsonIsAllocated: Boolean;
    var LQueryBsonPtr := ALMakeBsonPtrFromJSONString(AQuery, @LQueryBsonRec, LQueryBsonIsAllocated);
    try

      if ASort <> '' then begin
        var LSortBsonRec: bson_t;
        var LSortBsonIsAllocated: Boolean;
        var LSortBsonPtr := ALMakeBsonPtrFromJSONString(ASort, @LSortBsonRec, LSortBsonIsAllocated);
        try
          if not mongoc_find_and_modify_opts_set_sort(LOpts, LSortBsonPtr) then
            raise Exception.Create('mongoc_find_and_modify_opts_set_sort failed');
        finally
          if LSortBsonIsAllocated then
            bson_destroy(LSortBsonPtr);
        end;
      end;

      if Length(AUpdateJSON) > 0 then begin
        var LUpdateBsonRec: bson_t;
        var LUpdateBsonIsAllocated: Boolean;
        var LUpdateBsonPtr := ALMakeBsonPtrFromJSONString(AUpdateJSON, @LUpdateBsonRec, LUpdateBsonIsAllocated);
        try
          if not mongoc_find_and_modify_opts_set_update(LOpts, LUpdateBsonPtr) then
            raise Exception.Create('mongoc_find_and_modify_opts_set_update failed');
        finally
          if LUpdateBsonIsAllocated then
            bson_destroy(LUpdateBsonPtr);
        end;
      end
      else if Length(AUpdateBSON) > 0 then begin
        var LUpdateBsonRec: bson_t;
        var LUpdateBsonIsAllocated: Boolean;
        var LUpdateBsonPtr := ALMakeBsonPtrFromBSONBytes(AUpdateBSON, @LUpdateBsonRec, LUpdateBsonIsAllocated);
        try
          if not mongoc_find_and_modify_opts_set_update(LOpts, LUpdateBsonPtr) then
            raise Exception.Create('mongoc_find_and_modify_opts_set_update failed');
        finally
          if LUpdateBsonIsAllocated then
            bson_destroy(LUpdateBsonPtr);
        end;
      end;

      if AFields <> '' then begin
        var LFieldsBsonRec: bson_t;
        var LFieldsBsonIsAllocated: Boolean;
        var LFieldsBsonPtr := ALMakeBsonPtrFromJSONString(AFields, @LFieldsBsonRec, LFieldsBsonIsAllocated);
        try
          if not mongoc_find_and_modify_opts_set_fields(LOpts, LFieldsBsonPtr) then
            raise Exception.Create('mongoc_find_and_modify_opts_set_fields failed');
        finally
          if LFieldsBsonIsAllocated then
            bson_destroy(LFieldsBsonPtr);
        end;
      end;

      var LFlags: UInt32 := UInt32(ord(MONGOC_FIND_AND_MODIFY_NONE));
      if ARemove then LFlags := LFlags or UInt32(ord(MONGOC_FIND_AND_MODIFY_REMOVE));
      if AUpsert then LFlags := LFlags or UInt32(ord(MONGOC_FIND_AND_MODIFY_UPSERT));
      if ANew then LFlags := LFlags or UInt32(ord(MONGOC_FIND_AND_MODIFY_RETURN_NEW));
      if not mongoc_find_and_modify_opts_set_flags(LOpts, LFlags) then
        raise Exception.Create('mongoc_find_and_modify_opts_set_flags failed');

      if LClient = nil then begin
        LClient := mongoc_client_pool_try_pop(FPool);
        if LClient = nil then begin
          ALLog('TALMongoDBClient', 'Pool at max capacity; waiting for a free client.', TALLogType.WARN);
          LClient := mongoc_client_pool_pop(FPool);
          if LClient = nil then raise Exception.Create('mongoc_client_pool_pop failed');
        end;
        LOwnsClient := True;
      end
      else begin
        var LSession := FSession;
        if LSession <> nil then begin
          var LOptsBsonRec: bson_t;
          var LOptsBsonIsAllocated: Boolean;
          var LOptsBsonPtr := ALMakeBsonPtrFromJSONString('', @LOptsBsonRec, LOptsBsonIsAllocated, False{AReadOnly});
          try
            var LError: bson_error_t;
            if not mongoc_client_session_append(
                     FSession, // client_session: Pmongoc_client_session_t;
                     LOptsBsonPtr, // opts: Pbson_t;
                     @LError) then // error: Pbson_error_t
              ALRaiseMongoDBError(LError);
            LOptsBsonIsAllocated := True;
            if not mongoc_find_and_modify_opts_append(LOpts, LOptsBsonPtr) then
              raise Exception.Create('mongoc_find_and_modify_opts_append failed');
          finally
            if LOptsBsonIsAllocated then
              bson_destroy(LOptsBsonPtr);
          end;
        end;
      end;
      Try

        var LCollection := mongoc_client_get_collection(
                             LClient, // client: Pmongoc_client_t;
                             PAnsiChar(ADatabaseName), // db: PAnsiChar;
                             PAnsiChar(ACollectionName)); // collection: PAnsiChar
        if LCollection = nil then raise Exception.Create('mongoc_client_get_collection failed');
        try

          var LReply: bson_t;
          bson_init(@LReply);
          var LPReply: Pbson_t := @LReply;
          var LError: bson_error_t;
          var LSuccess := mongoc_collection_find_and_modify_with_opts(
                            LCollection, // collection: Pmongoc_collection_t;
                            LQueryBsonPtr, // query: Pbson_t;
                            LOpts, // opts: Pmongoc_find_and_modify_opts_t;
                            LPReply, // reply: Pbson_t;
                            @LError); // error: Pbson_error_t)
          try

            if not LSuccess then
              ALRaiseMongoDBError(LError);

            var LStream := TPointerStream.Create(bson_get_data(LPReply){Ptr}, LPReply^.len{Size}, true{ReadOnly});
            try
              Result := TALJSONDocumentA.CreateFromBSONStream(LStream);
            finally
              ALFreeAndNil(LStream);
            end;

          finally
            bson_destroy(LPReply);
          end;

        finally
          mongoc_collection_destroy(LCollection);
        end;

      Finally
        if LOwnsClient then
          mongoc_client_pool_push(FPool, LClient);
      End;

    finally
      if LQueryBsonIsAllocated then
        bson_destroy(LQueryBsonPtr);
    end;

  finally
    mongoc_find_and_modify_opts_destroy(LOpts);
  end;

end;

{**********************************}
function TALMongoDBClient.FindAndModify(
           const ADatabaseName: AnsiString;
           const ACollectionName: AnsiString;
           const AQuery: AnsiString;
           const ASort: AnsiString;
           const AUpdate: AnsiString;
           const AFields: AnsiString;
           const ARemove: ByteBool;
           const AUpsert: ByteBool;
           const ANew: ByteBool): TALJsonNodeA;
begin
  Result := FindAndModify(
              ADatabaseName, // const ADatabaseName: AnsiString;
              ACollectionName, // const ACollectionName: AnsiString;
              AQuery, // const AQuery: AnsiString;
              ASort, // const ASort: AnsiString;
              AUpdate, // const AUpdateJSON: AnsiString;
              nil, // const AUpdateBSON: TBytes;
              AFields, // const AFields: AnsiString;
              ARemove, // const ARemove: ByteBool;
              AUpsert, // const AUpsert: ByteBool;
              ANew); // const ANew: ByteBool): TALJsonNodeA;
end;

{**********************************}
function TALMongoDBClient.FindAndModify(
           const ADatabaseName: AnsiString;
           const ACollectionName: AnsiString;
           const AQuery: AnsiString;
           const ASort: AnsiString;
           const AUpdate: TBytes;
           const AFields: AnsiString;
           const ARemove: ByteBool;
           const AUpsert: ByteBool;
           const ANew: ByteBool): TALJsonNodeA;
begin
  Result := FindAndModify(
              ADatabaseName, // const ADatabaseName: AnsiString;
              ACollectionName, // const ACollectionName: AnsiString;
              AQuery, // const AQuery: AnsiString;
              ASort, // const ASort: AnsiString;
              '', // const AUpdateJSON: AnsiString;
              AUpdate, // const AUpdateBSON: TBytes;
              AFields, // const AFields: AnsiString;
              ARemove, // const ARemove: ByteBool;
              AUpsert, // const AUpsert: ByteBool;
              ANew); // const ANew: ByteBool): TALJsonNodeA;
end;

{**********************************}
function TALMongoDBClient.InsertOne(
           const ADatabaseName: AnsiString;
           const ACollectionName: AnsiString;
           const ADocumentJSON: AnsiString;
           const ADocumentBSON: TBytes;
           const AOpts: AnsiString = ''): TALJsonNodeA;
begin

  var LOwnsClient: Boolean := False;
  var LClient := FClient;

  var LDocumentBsonRec: bson_t;
  var LDocumentBsonIsAllocated: Boolean;
  var LDocumentBsonPtr: Pbson_t;
  if length(ADocumentJSON) > 0 then LDocumentBsonPtr := ALMakeBsonPtrFromJSONString(ADocumentJSON, @LDocumentBsonRec, LDocumentBsonIsAllocated)
  else LDocumentBsonPtr := ALMakeBsonPtrFromBSONBytes(ADocumentBSON, @LDocumentBsonRec, LDocumentBsonIsAllocated);
  try

    var LOptsBsonRec: bson_t;
    var LOptsBsonIsAllocated := False;
    var LOptsBsonPtr: Pbson_t;
    if (AOpts <> '') or (LClient <> nil) then LOptsBsonPtr := ALMakeBsonPtrFromJSONString(AOpts, @LOptsBsonRec, LOptsBsonIsAllocated, LClient = nil{AReadOnly})
    else LOptsBsonPtr := nil;
    try

      if LClient = nil then begin
        LClient := mongoc_client_pool_try_pop(FPool);
        if LClient = nil then begin
          ALLog('TALMongoDBClient', 'Pool at max capacity; waiting for a free client.', TALLogType.WARN);
          LClient := mongoc_client_pool_pop(FPool);
          if LClient = nil then raise Exception.Create('mongoc_client_pool_pop failed');
        end;
        LOwnsClient := True;
      end
      else begin
        var LSession := FSession;
        if LSession <> nil then begin
          var LError: bson_error_t;
          if not mongoc_client_session_append(
                   FSession, // client_session: Pmongoc_client_session_t;
                   LOptsBsonPtr, // opts: Pbson_t;
                   @LError) then // error: Pbson_error_t
            ALRaiseMongoDBError(LError);
          LOptsBsonIsAllocated := True;
        end;
      end;
      Try

        var LCollection := mongoc_client_get_collection(
                             LClient, // client: Pmongoc_client_t;
                             PAnsiChar(ADatabaseName), // db: PAnsiChar;
                             PAnsiChar(ACollectionName)); // collection: PAnsiChar
        if LCollection = nil then raise Exception.Create('mongoc_client_get_collection failed');
        try

          var LReply: bson_t;
          bson_init(@LReply);
          var LPReply: Pbson_t := @LReply;
          var LError: bson_error_t;
          var LSuccess := mongoc_collection_insert_one(
                            LCollection, // collection: Pmongoc_collection_t;
                            LDocumentBsonPtr, // Document: Pbson_t;
                            LOptsBsonPtr, // opts: Pbson_t;
                            LPReply, // reply: Pbson_t;
                            @LError); // error: Pbson_error_t
          try

            if not LSuccess then
              ALRaiseMongoDBError(LError);

            var LStream := TPointerStream.Create(bson_get_data(LPReply){Ptr}, LPReply^.len{Size}, true{ReadOnly});
            try
              Result := TALJSONDocumentA.CreateFromBSONStream(LStream);
            finally
              ALFreeAndNil(LStream);
            end;

          finally
            bson_destroy(LPReply);
          end;

        finally
          mongoc_collection_destroy(LCollection);
        end;

      Finally
        if LOwnsClient then
          mongoc_client_pool_push(FPool, LClient);
      End;

    finally
      if LOptsBsonIsAllocated then
        bson_destroy(LOptsBsonPtr);
    end;

  finally
    if LDocumentBsonIsAllocated then
      bson_destroy(LDocumentBsonPtr);
  end;

end;

{***********************************}
function TALMongoDBClient.InsertOne(
           const ADatabaseName: AnsiString;
           const ACollectionName: AnsiString;
           const ADocument: AnsiString;
           const AOpts: AnsiString = ''): TALJsonNodeA;
begin
  Result := InsertOne(
              ADatabaseName, // const ADatabaseName: AnsiString;
              ACollectionName, // const ACollectionName: AnsiString;
              ADocument, // const ADocumentJSON: AnsiString;
              nil, // const ADocumentBSON: TBytes;
              AOpts); // const AOpts: AnsiString = ''): TALJsonNodeA;
end;

{***********************************}
function TALMongoDBClient.InsertOne(
           const ADatabaseName: AnsiString;
           const ACollectionName: AnsiString;
           const ADocument: TBytes;
           const AOpts: AnsiString = ''): TALJsonNodeA;
begin
  Result := InsertOne(
              ADatabaseName, // const ADatabaseName: AnsiString;
              ACollectionName, // const ACollectionName: AnsiString;
              '', // const ADocumentJSON: AnsiString;
              ADocument, // const ADocumentBSON: TBytes;
              AOpts); // const AOpts: AnsiString = ''): TALJsonNodeA;
end;

{***********************************}
function TALMongoDBClient.InsertMany(
           const ADatabaseName: AnsiString;
           const ACollectionName: AnsiString;
           const ADocumentJSONs: TArray<AnsiString>;
           const ADocumentBSONs: TArray<TBytes>;
           const AOpts: AnsiString = ''): TALJsonNodeA;
begin

  var LOwnsClient: Boolean := False;
  var LClient := FClient;

  var LDocumentBsonRecs: TArray<bson_t>;
  var LDocumentBsonPtrs: TArray<Pbson_t>;
  var LDocumentBsonAllocated: TArray<Boolean>;
  if length(ADocumentJSONs) > 0 then begin
    SetLength(LDocumentBsonRecs, Length(ADocumentJSONs));
    SetLength(LDocumentBsonPtrs, Length(ADocumentJSONs));
    SetLength(LDocumentBsonAllocated, Length(ADocumentJSONs));
  end
  else if length(ADocumentBSONs) > 0 then begin
    SetLength(LDocumentBsonRecs, Length(ADocumentBSONs));
    SetLength(LDocumentBsonPtrs, Length(ADocumentBSONs));
    SetLength(LDocumentBsonAllocated, Length(ADocumentBSONs));
  end
  else
    raise Exception.Create('No documents to insert');

  for var I := Low(LDocumentBsonAllocated) to High(LDocumentBsonAllocated) do
    LDocumentBsonAllocated[I] := False;

  try

    if length(ADocumentJSONs) > 0 then begin
      for var I := low(ADocumentJSONs) to High(ADocumentJSONs) do
        LDocumentBsonPtrs[I] := ALMakeBsonPtrFromJSONString(ADocumentJSONs[I], @LDocumentBsonRecs[I], LDocumentBsonAllocated[I]);
    end
    else begin
      for var I := low(ADocumentBSONs) to High(ADocumentBSONs) do
        LDocumentBsonPtrs[I] := ALMakeBsonPtrFromBSONBytes(ADocumentBSONs[I], @LDocumentBsonRecs[I], LDocumentBsonAllocated[I]);
    end;

    var LOptsBsonRec: bson_t;
    var LOptsBsonIsAllocated := False;
    var LOptsBsonPtr: Pbson_t;
    if (AOpts <> '') or (LClient <> nil) then LOptsBsonPtr := ALMakeBsonPtrFromJSONString(AOpts, @LOptsBsonRec, LOptsBsonIsAllocated, LClient = nil{AReadOnly})
    else LOptsBsonPtr := nil;
    try

      if LClient = nil then begin
        LClient := mongoc_client_pool_try_pop(FPool);
        if LClient = nil then begin
          ALLog('TALMongoDBClient', 'Pool at max capacity; waiting for a free client.', TALLogType.WARN);
          LClient := mongoc_client_pool_pop(FPool);
          if LClient = nil then raise Exception.Create('mongoc_client_pool_pop failed');
        end;
        LOwnsClient := True;
      end
      else begin
        var LSession := FSession;
        if LSession <> nil then begin
          var LError: bson_error_t;
          if not mongoc_client_session_append(
                   FSession, // client_session: Pmongoc_client_session_t;
                   LOptsBsonPtr, // opts: Pbson_t;
                   @LError) then // error: Pbson_error_t
            ALRaiseMongoDBError(LError);
          LOptsBsonIsAllocated := True;
        end;
      end;
      Try

        var LCollection := mongoc_client_get_collection(
                             LClient, // client: Pmongoc_client_t;
                             PAnsiChar(ADatabaseName), // db: PAnsiChar;
                             PAnsiChar(ACollectionName)); // collection: PAnsiChar
        if LCollection = nil then raise Exception.Create('mongoc_client_get_collection failed');
        try

          var LReply: bson_t;
          bson_init(@LReply);
          var LPReply: Pbson_t := @LReply;
          var LError: bson_error_t;
          var LSuccess := mongoc_collection_insert_many(
                            LCollection, // collection: Pmongoc_collection_t;
                            @LDocumentBsonPtrs[0], // documents: PPbson_t;
                            size_t(Length(LDocumentBsonPtrs)), // n_documents: size_t;
                            LOptsBsonPtr, // opts: Pbson_t;
                            LPReply, // reply: Pbson_t;
                            @LError); // error: Pbson_error_t
          try

            if not LSuccess then
              ALRaiseMongoDBError(LError);

            var LStream := TPointerStream.Create(bson_get_data(LPReply){Ptr}, LPReply^.len{Size}, true{ReadOnly});
            try
              Result := TALJSONDocumentA.CreateFromBSONStream(LStream);
            finally
              ALFreeAndNil(LStream);
            end;

          finally
            bson_destroy(LPReply);
          end;

        finally
          mongoc_collection_destroy(LCollection);
        end;

      Finally
        if LOwnsClient then
          mongoc_client_pool_push(FPool, LClient);
      End;

    finally
      if LOptsBsonIsAllocated then
        bson_destroy(LOptsBsonPtr);
    end;

  finally
    for var I := low(LDocumentBsonPtrs) to High(LDocumentBsonPtrs) do
      if LDocumentBsonAllocated[I] then
        bson_destroy(LDocumentBsonPtrs[I]);
  end;

end;

{**********************************}
function TALMongoDBClient.InsertMany(
           const ADatabaseName: AnsiString;
           const ACollectionName: AnsiString;
           const ADocuments: TArray<AnsiString>;
           const AOpts: AnsiString = ''): TALJsonNodeA;
begin
  Result := InsertMany(
              ADatabaseName, // const ADatabaseName: AnsiString;
              ACollectionName, // const ACollectionName: AnsiString;
              ADocuments, // const ADocumentJSONs: TArray<AnsiString>;
              nil, // const ADocumentBSONs: TArray<TBytes>;
              AOpts); // const AOpts: AnsiString = ''): TALJsonNodeA;
end;

{**********************************}
function TALMongoDBClient.InsertMany(
           const ADatabaseName: AnsiString;
           const ACollectionName: AnsiString;
           const ADocuments: TArray<TBytes>;
           const AOpts: AnsiString = ''): TALJsonNodeA;
begin
  Result := InsertMany(
              ADatabaseName, // const ADatabaseName: AnsiString;
              ACollectionName, // const ACollectionName: AnsiString;
              nil, // const ADocumentJSONs: TArray<AnsiString>;
              ADocuments, // const ADocumentBSONs: TArray<TBytes>;
              AOpts); // const AOpts: AnsiString = ''): TALJsonNodeA;
end;

{**********************************}
function TALMongoDBClient.UpdateOne(
           const ADatabaseName: AnsiString;
           const ACollectionName: AnsiString;
           const ASelector: AnsiString;
           const AUpdateJSON: AnsiString;
           const AUpdateBSON: TBytes;
           const AOpts: AnsiString = ''): TALJsonNodeA;
begin

  var LOwnsClient: Boolean := False;
  var LClient := FClient;

  var LSelectorBsonRec: bson_t;
  var LSelectorBsonIsAllocated: Boolean;
  var LSelectorBsonPtr := ALMakeBsonPtrFromJSONString(ASelector, @LSelectorBsonRec, LSelectorBsonIsAllocated);
  try

    var LUpdateBsonRec: bson_t;
    var LUpdateBsonIsAllocated: Boolean;
    var LUpdateBsonPtr: Pbson_t;
    if length(AUpdateJSON) > 0 then LUpdateBsonPtr := ALMakeBsonPtrFromJSONString(AUpdateJSON, @LUpdateBsonRec, LUpdateBsonIsAllocated)
    else LUpdateBsonPtr := ALMakeBsonPtrFromBSONBytes(AUpdateBSON, @LUpdateBsonRec, LUpdateBsonIsAllocated);
    try

      var LOptsBsonRec: bson_t;
      var LOptsBsonIsAllocated := False;
      var LOptsBsonPtr: Pbson_t;
      if (AOpts <> '') or (LClient <> nil) then LOptsBsonPtr := ALMakeBsonPtrFromJSONString(AOpts, @LOptsBsonRec, LOptsBsonIsAllocated, LClient = nil{AReadOnly})
      else LOptsBsonPtr := nil;
      try

        if LClient = nil then begin
          LClient := mongoc_client_pool_try_pop(FPool);
          if LClient = nil then begin
            ALLog('TALMongoDBClient', 'Pool at max capacity; waiting for a free client.', TALLogType.WARN);
            LClient := mongoc_client_pool_pop(FPool);
            if LClient = nil then raise Exception.Create('mongoc_client_pool_pop failed');
          end;
          LOwnsClient := True;
        end
        else begin
          var LSession := FSession;
          if LSession <> nil then begin
            var LError: bson_error_t;
            if not mongoc_client_session_append(
                     FSession, // client_session: Pmongoc_client_session_t;
                     LOptsBsonPtr, // opts: Pbson_t;
                     @LError) then // error: Pbson_error_t
              ALRaiseMongoDBError(LError);
            LOptsBsonIsAllocated := True;
          end;
        end;
        Try

          var LCollection := mongoc_client_get_collection(
                               LClient, // client: Pmongoc_client_t;
                               PAnsiChar(ADatabaseName), // db: PAnsiChar;
                               PAnsiChar(ACollectionName)); // collection: PAnsiChar
          if LCollection = nil then raise Exception.Create('mongoc_client_get_collection failed');
          try

            var LReply: bson_t;
            bson_init(@LReply);
            var LPReply: Pbson_t := @LReply;
            var LError: bson_error_t;
            var LSuccess := mongoc_collection_update_one(
                              LCollection, // collection: Pmongoc_collection_t;
                              LSelectorBsonPtr, // selector: Pbson_t;
                              LUpdateBsonPtr, // update: Pbson_t;
                              LOptsBsonPtr, // opts: Pbson_t;
                              LPReply, // reply: Pbson_t;
                              @LError); // error: Pbson_error_t
            try

              if not LSuccess then
                ALRaiseMongoDBError(LError);

              var LStream := TPointerStream.Create(bson_get_data(LPReply){Ptr}, LPReply^.len{Size}, true{ReadOnly});
              try
                Result := TALJSONDocumentA.CreateFromBSONStream(LStream);
              finally
                ALFreeAndNil(LStream);
              end;

            finally
              bson_destroy(LPReply);
            end;

          finally
            mongoc_collection_destroy(LCollection);
          end;

        Finally
          if LOwnsClient then
            mongoc_client_pool_push(FPool, LClient);
        End;

      finally
        if LOptsBsonIsAllocated then
          bson_destroy(LOptsBsonPtr);
      end;

    finally
      if LUpdateBsonIsAllocated then
        bson_destroy(LUpdateBsonPtr);
    end;

  finally
    if LSelectorBsonIsAllocated then
      bson_destroy(LSelectorBsonPtr);
  end;

end;

{***********************************}
function TALMongoDBClient.UpdateOne(
           const ADatabaseName: AnsiString;
           const ACollectionName: AnsiString;
           const ASelector: AnsiString;
           const AUpdate: AnsiString;
           const AOpts: AnsiString = ''): TALJsonNodeA;
begin
  Result := UpdateOne(
              ADatabaseName, // const ADatabaseName: AnsiString;
              ACollectionName, // const ACollectionName: AnsiString;
              ASelector, // const ASelector: AnsiString;
              AUpdate, // const AUpdateJSON: AnsiString;
              nil, // const AUpdateBSON: TBytes;
              AOpts); // const AOpts: AnsiString = ''): TALJsonNodeA;
end;

{***********************************}
function TALMongoDBClient.UpdateOne(
           const ADatabaseName: AnsiString;
           const ACollectionName: AnsiString;
           const ASelector: AnsiString;
           const AUpdate: TBytes;
           const AOpts: AnsiString = ''): TALJsonNodeA;
begin
  Result := UpdateOne(
              ADatabaseName, // const ADatabaseName: AnsiString;
              ACollectionName, // const ACollectionName: AnsiString;
              ASelector, // const ASelector: AnsiString;
              '', // const AUpdateJSON: AnsiString;
              AUpdate, // const AUpdateBSON: TBytes;
              AOpts); // const AOpts: AnsiString = ''): TALJsonNodeA;
end;

{***********************************}
function TALMongoDBClient.UpdateMany(
           const ADatabaseName: AnsiString;
           const ACollectionName: AnsiString;
           const ASelector: AnsiString;
           const AUpdateJSON: AnsiString;
           const AUpdateBSON: TBytes;
           const AOpts: AnsiString = ''): TALJsonNodeA;
begin

  var LOwnsClient: Boolean := False;
  var LClient := FClient;

  var LSelectorBsonRec: bson_t;
  var LSelectorBsonIsAllocated: Boolean;
  var LSelectorBsonPtr := ALMakeBsonPtrFromJSONString(ASelector, @LSelectorBsonRec, LSelectorBsonIsAllocated);
  try

    var LUpdateBsonRec: bson_t;
    var LUpdateBsonIsAllocated: Boolean;
    var LUpdateBsonPtr: Pbson_t;
    if length(AUpdateJSON) > 0 then LUpdateBsonPtr := ALMakeBsonPtrFromJSONString(AUpdateJSON, @LUpdateBsonRec, LUpdateBsonIsAllocated)
    else LUpdateBsonPtr := ALMakeBsonPtrFromBSONBytes(AUpdateBSON, @LUpdateBsonRec, LUpdateBsonIsAllocated);
    try

      var LOptsBsonRec: bson_t;
      var LOptsBsonIsAllocated := False;
      var LOptsBsonPtr: Pbson_t;
      if (AOpts <> '') or (LClient <> nil) then LOptsBsonPtr := ALMakeBsonPtrFromJSONString(AOpts, @LOptsBsonRec, LOptsBsonIsAllocated, LClient = nil{AReadOnly})
      else LOptsBsonPtr := nil;
      try

        if LClient = nil then begin
          LClient := mongoc_client_pool_try_pop(FPool);
          if LClient = nil then begin
            ALLog('TALMongoDBClient', 'Pool at max capacity; waiting for a free client.', TALLogType.WARN);
            LClient := mongoc_client_pool_pop(FPool);
            if LClient = nil then raise Exception.Create('mongoc_client_pool_pop failed');
          end;
          LOwnsClient := True;
        end
        else begin
          var LSession := FSession;
          if LSession <> nil then begin
            var LError: bson_error_t;
            if not mongoc_client_session_append(
                     FSession, // client_session: Pmongoc_client_session_t;
                     LOptsBsonPtr, // opts: Pbson_t;
                     @LError) then // error: Pbson_error_t
              ALRaiseMongoDBError(LError);
            LOptsBsonIsAllocated := True;
          end;
        end;
        Try

          var LCollection := mongoc_client_get_collection(
                               LClient, // client: Pmongoc_client_t;
                               PAnsiChar(ADatabaseName), // db: PAnsiChar;
                               PAnsiChar(ACollectionName)); // collection: PAnsiChar
          if LCollection = nil then raise Exception.Create('mongoc_client_get_collection failed');
          try

            var LReply: bson_t;
            bson_init(@LReply);
            var LPReply: Pbson_t := @LReply;
            var LError: bson_error_t;
            var LSuccess := mongoc_collection_update_many(
                              LCollection, // collection: Pmongoc_collection_t;
                              LSelectorBsonPtr, // selector: Pbson_t;
                              LUpdateBsonPtr, // update: Pbson_t;
                              LOptsBsonPtr, // opts: Pbson_t;
                              LPReply, // reply: Pbson_t;
                              @LError); // error: Pbson_error_t
            try

              if not LSuccess then
                ALRaiseMongoDBError(LError);

              var LStream := TPointerStream.Create(bson_get_data(LPReply){Ptr}, LPReply^.len{Size}, true{ReadOnly});
              try
                Result := TALJSONDocumentA.CreateFromBSONStream(LStream);
              finally
                ALFreeAndNil(LStream);
              end;

            finally
              bson_destroy(LPReply);
            end;

          finally
            mongoc_collection_destroy(LCollection);
          end;

        Finally
          if LOwnsClient then
            mongoc_client_pool_push(FPool, LClient);
        End;

      finally
        if LOptsBsonIsAllocated then
          bson_destroy(LOptsBsonPtr);
      end;

    finally
      if LUpdateBsonIsAllocated then
        bson_destroy(LUpdateBsonPtr);
    end;

  finally
    if LSelectorBsonIsAllocated then
      bson_destroy(LSelectorBsonPtr);
  end;

end;

{***********************************}
function TALMongoDBClient.UpdateMany(
           const ADatabaseName: AnsiString;
           const ACollectionName: AnsiString;
           const ASelector: AnsiString;
           const AUpdate: AnsiString;
           const AOpts: AnsiString = ''): TALJsonNodeA;
begin
  Result := UpdateMany(
              ADatabaseName, // const ADatabaseName: AnsiString;
              ACollectionName, // const ACollectionName: AnsiString;
              ASelector, // const ASelector: AnsiString;
              AUpdate, // const AUpdateJSON: AnsiString;
              nil, // const AUpdateBSON: TBytes;
              AOpts); // const AOpts: AnsiString = ''): TALJsonNodeA;
end;

{***********************************}
function TALMongoDBClient.UpdateMany(
           const ADatabaseName: AnsiString;
           const ACollectionName: AnsiString;
           const ASelector: AnsiString;
           const AUpdate: TBytes;
           const AOpts: AnsiString = ''): TALJsonNodeA;
begin
  Result := UpdateMany(
              ADatabaseName, // const ADatabaseName: AnsiString;
              ACollectionName, // const ACollectionName: AnsiString;
              ASelector, // const ASelector: AnsiString;
              '', // const AUpdateJSON: AnsiString;
              AUpdate, // const AUpdateBSON: TBytes;
              AOpts); // const AOpts: AnsiString = ''): TALJsonNodeA;
end;

{***********************************}
function TALMongoDBClient.ReplaceOne(
           const ADatabaseName: AnsiString;
           const ACollectionName: AnsiString;
           const ASelector: AnsiString;
           const AReplacementJSON: AnsiString;
           const AReplacementBSON: TBytes;
           const AOpts: AnsiString = ''): TALJsonNodeA;
begin

  var LOwnsClient: Boolean := False;
  var LClient := FClient;

  var LSelectorBsonRec: bson_t;
  var LSelectorBsonIsAllocated: Boolean;
  var LSelectorBsonPtr := ALMakeBsonPtrFromJSONString(ASelector, @LSelectorBsonRec, LSelectorBsonIsAllocated);
  try

    var LReplacementBsonRec: bson_t;
    var LReplacementBsonIsAllocated: Boolean;
    var LReplacementBsonPtr: Pbson_t;
    if Length(AReplacementJSON) > 0 then LReplacementBsonPtr := ALMakeBsonPtrFromJSONString(AReplacementJSON, @LReplacementBsonRec, LReplacementBsonIsAllocated)
    else LReplacementBsonPtr := ALMakeBsonPtrFromBSONBytes(AReplacementBSON, @LReplacementBsonRec, LReplacementBsonIsAllocated);
    try

      var LOptsBsonRec: bson_t;
      var LOptsBsonIsAllocated := False;
      var LOptsBsonPtr: Pbson_t;
      if (AOpts <> '') or (LClient <> nil) then LOptsBsonPtr := ALMakeBsonPtrFromJSONString(AOpts, @LOptsBsonRec, LOptsBsonIsAllocated, LClient = nil{AReadOnly})
      else LOptsBsonPtr := nil;
      try

        if LClient = nil then begin
          LClient := mongoc_client_pool_try_pop(FPool);
          if LClient = nil then begin
            ALLog('TALMongoDBClient', 'Pool at max capacity; waiting for a free client.', TALLogType.WARN);
            LClient := mongoc_client_pool_pop(FPool);
            if LClient = nil then raise Exception.Create('mongoc_client_pool_pop failed');
          end;
          LOwnsClient := True;
        end
        else begin
          var LSession := FSession;
          if LSession <> nil then begin
            var LError: bson_error_t;
            if not mongoc_client_session_append(
                     FSession, // client_session: Pmongoc_client_session_t;
                     LOptsBsonPtr, // opts: Pbson_t;
                     @LError) then // error: Pbson_error_t
              ALRaiseMongoDBError(LError);
            LOptsBsonIsAllocated := True;
          end;
        end;
        Try

          var LCollection := mongoc_client_get_collection(
                               LClient, // client: Pmongoc_client_t;
                               PAnsiChar(ADatabaseName), // db: PAnsiChar;
                               PAnsiChar(ACollectionName)); // collection: PAnsiChar
          if LCollection = nil then raise Exception.Create('mongoc_client_get_collection failed');
          try

            var LReply: bson_t;
            bson_init(@LReply);
            var LPReply: Pbson_t := @LReply;
            var LError: bson_error_t;
            var LSuccess := mongoc_collection_replace_one(
                              LCollection, // collection: Pmongoc_collection_t;
                              LSelectorBsonPtr, // selector: Pbson_t;
                              LReplacementBsonPtr, // Replacement: Pbson_t;
                              LOptsBsonPtr, // opts: Pbson_t;
                              LPReply, // reply: Pbson_t;
                              @LError); // error: Pbson_error_t
            try

              if not LSuccess then
                ALRaiseMongoDBError(LError);

              var LStream := TPointerStream.Create(bson_get_data(LPReply){Ptr}, LPReply^.len{Size}, true{ReadOnly});
              try
                Result := TALJSONDocumentA.CreateFromBSONStream(LStream);
              finally
                ALFreeAndNil(LStream);
              end;

            finally
              bson_destroy(LPReply);
            end;

          finally
            mongoc_collection_destroy(LCollection);
          end;

        Finally
          if LOwnsClient then
            mongoc_client_pool_push(FPool, LClient);
        End;

      finally
        if LOptsBsonIsAllocated then
          bson_destroy(LOptsBsonPtr);
      end;

    finally
      if LReplacementBsonIsAllocated then
        bson_destroy(LReplacementBsonPtr);
    end;

  finally
    if LSelectorBsonIsAllocated then
      bson_destroy(LSelectorBsonPtr);
  end;

end;

{**********************************}
function TALMongoDBClient.ReplaceOne(
           const ADatabaseName: AnsiString;
           const ACollectionName: AnsiString;
           const ASelector: AnsiString;
           const AReplacement: AnsiString;
           const AOpts: AnsiString = ''): TALJsonNodeA;
begin
  Result := ReplaceOne(
              ADatabaseName, // const ADatabaseName: AnsiString;
              ACollectionName, // const ACollectionName: AnsiString;
              ASelector, // const ASelector: AnsiString;
              AReplacement, // const AReplacementJSON: AnsiString;
              nil, // const AReplacementBSON: TBytes;
              AOpts); // const AOpts: AnsiString = ''): TALJsonNodeA;
end;

{**********************************}
function TALMongoDBClient.ReplaceOne(
           const ADatabaseName: AnsiString;
           const ACollectionName: AnsiString;
           const ASelector: AnsiString;
           const AReplacement: TBytes;
           const AOpts: AnsiString = ''): TALJsonNodeA;
begin
  Result := ReplaceOne(
              ADatabaseName, // const ADatabaseName: AnsiString;
              ACollectionName, // const ACollectionName: AnsiString;
              ASelector, // const ASelector: AnsiString;
              '', // const AReplacementJSON: AnsiString;
              AReplacement, // const AReplacementBSON: TBytes;
              AOpts); // const AOpts: AnsiString = ''): TALJsonNodeA;
end;

{**********************************}
function TALMongoDBClient.DeleteOne(
           const ADatabaseName: AnsiString;
           const ACollectionName: AnsiString;
           const ASelector: AnsiString;
           const AOpts: AnsiString = ''): TALJsonNodeA;
begin

  var LOwnsClient: Boolean := False;
  var LClient := FClient;

  var LSelectorBsonRec: bson_t;
  var LSelectorBsonIsAllocated: Boolean;
  var LSelectorBsonPtr := ALMakeBsonPtrFromJSONString(ASelector, @LSelectorBsonRec, LSelectorBsonIsAllocated);
  try

    var LOptsBsonRec: bson_t;
    var LOptsBsonIsAllocated := False;
    var LOptsBsonPtr: Pbson_t;
    if (AOpts <> '') or (LClient <> nil) then LOptsBsonPtr := ALMakeBsonPtrFromJSONString(AOpts, @LOptsBsonRec, LOptsBsonIsAllocated, LClient = nil{AReadOnly})
    else LOptsBsonPtr := nil;
    try

      if LClient = nil then begin
        LClient := mongoc_client_pool_try_pop(FPool);
        if LClient = nil then begin
          ALLog('TALMongoDBClient', 'Pool at max capacity; waiting for a free client.', TALLogType.WARN);
          LClient := mongoc_client_pool_pop(FPool);
          if LClient = nil then raise Exception.Create('mongoc_client_pool_pop failed');
        end;
        LOwnsClient := True;
      end
      else begin
        var LSession := FSession;
        if LSession <> nil then begin
          var LError: bson_error_t;
          if not mongoc_client_session_append(
                   FSession, // client_session: Pmongoc_client_session_t;
                   LOptsBsonPtr, // opts: Pbson_t;
                   @LError) then // error: Pbson_error_t
            ALRaiseMongoDBError(LError);
          LOptsBsonIsAllocated := True;
        end;
      end;
      Try

        var LCollection := mongoc_client_get_collection(
                             LClient, // client: Pmongoc_client_t;
                             PAnsiChar(ADatabaseName), // db: PAnsiChar;
                             PAnsiChar(ACollectionName)); // collection: PAnsiChar
        if LCollection = nil then raise Exception.Create('mongoc_client_get_collection failed');
        try

          var LReply: bson_t;
          bson_init(@LReply);
          var LPReply: Pbson_t := @LReply;
          var LError: bson_error_t;
          var LSuccess := mongoc_collection_delete_one(
                            LCollection, // collection: Pmongoc_collection_t;
                            LSelectorBsonPtr, // selector: Pbson_t;
                            LOptsBsonPtr, // opts: Pbson_t;
                            LPReply, // reply: Pbson_t;
                            @LError); // error: Pbson_error_t
          try

            if not LSuccess then
              ALRaiseMongoDBError(LError);

            var LStream := TPointerStream.Create(bson_get_data(LPReply){Ptr}, LPReply^.len{Size}, true{ReadOnly});
            try
              Result := TALJSONDocumentA.CreateFromBSONStream(LStream);
            finally
              ALFreeAndNil(LStream);
            end;

          finally
            bson_destroy(LPReply);
          end;

        finally
          mongoc_collection_destroy(LCollection);
        end;

      Finally
        if LOwnsClient then
          mongoc_client_pool_push(FPool, LClient);
      End;

    finally
      if LOptsBsonIsAllocated then
        bson_destroy(LOptsBsonPtr);
    end;

  finally
    if LSelectorBsonIsAllocated then
      bson_destroy(LSelectorBsonPtr);
  end;

end;

{***********************************}
function TALMongoDBClient.DeleteMany(
           const ADatabaseName: AnsiString;
           const ACollectionName: AnsiString;
           const ASelector: AnsiString;
           const AOpts: AnsiString = ''): TALJsonNodeA;
begin

  var LOwnsClient: Boolean := False;
  var LClient := FClient;

  var LSelectorBsonRec: bson_t;
  var LSelectorBsonIsAllocated: Boolean;
  var LSelectorBsonPtr := ALMakeBsonPtrFromJSONString(ASelector, @LSelectorBsonRec, LSelectorBsonIsAllocated);
  try

    var LOptsBsonRec: bson_t;
    var LOptsBsonIsAllocated := False;
    var LOptsBsonPtr: Pbson_t;
    if (AOpts <> '') or (LClient <> nil) then LOptsBsonPtr := ALMakeBsonPtrFromJSONString(AOpts, @LOptsBsonRec, LOptsBsonIsAllocated, LClient = nil{AReadOnly})
    else LOptsBsonPtr := nil;
    try

      if LClient = nil then begin
        LClient := mongoc_client_pool_try_pop(FPool);
        if LClient = nil then begin
          ALLog('TALMongoDBClient', 'Pool at max capacity; waiting for a free client.', TALLogType.WARN);
          LClient := mongoc_client_pool_pop(FPool);
          if LClient = nil then raise Exception.Create('mongoc_client_pool_pop failed');
        end;
        LOwnsClient := True;
      end
      else begin
        var LSession := FSession;
        if LSession <> nil then begin
          var LError: bson_error_t;
          if not mongoc_client_session_append(
                   FSession, // client_session: Pmongoc_client_session_t;
                   LOptsBsonPtr, // opts: Pbson_t;
                   @LError) then // error: Pbson_error_t
            ALRaiseMongoDBError(LError);
          LOptsBsonIsAllocated := True;
        end;
      end;
      Try

        var LCollection := mongoc_client_get_collection(
                             LClient, // client: Pmongoc_client_t;
                             PAnsiChar(ADatabaseName), // db: PAnsiChar;
                             PAnsiChar(ACollectionName)); // collection: PAnsiChar
        if LCollection = nil then raise Exception.Create('mongoc_client_get_collection failed');
        try

          var LReply: bson_t;
          bson_init(@LReply);
          var LPReply: Pbson_t := @LReply;
          var LError: bson_error_t;
          var LSuccess := mongoc_collection_delete_many(
                            LCollection, // collection: Pmongoc_collection_t;
                            LSelectorBsonPtr, // selector: Pbson_t;
                            LOptsBsonPtr, // opts: Pbson_t;
                            LPReply, // reply: Pbson_t;
                            @LError); // error: Pbson_error_t
          try

            if not LSuccess then
              ALRaiseMongoDBError(LError);

            var LStream := TPointerStream.Create(bson_get_data(LPReply){Ptr}, LPReply^.len{Size}, true{ReadOnly});
            try
              Result := TALJSONDocumentA.CreateFromBSONStream(LStream);
            finally
              ALFreeAndNil(LStream);
            end;

          finally
            bson_destroy(LPReply);
          end;

        finally
          mongoc_collection_destroy(LCollection);
        end;

      Finally
        if LOwnsClient then
          mongoc_client_pool_push(FPool, LClient);
      End;

    finally
      if LOptsBsonIsAllocated then
        bson_destroy(LOptsBsonPtr);
    end;

  finally
    if LSelectorBsonIsAllocated then
      bson_destroy(LSelectorBsonPtr);
  end;

end;

{************************************************}
constructor TALMongoDBChangeStreamListener.Create;
begin
  inherited Create(True{CreateSuspended});
  FUri := '';
  FDatabaseName := '';
  FCollectionName := '';
  FPipeline := '';
  FOpts := '';
  FOnChange := nil;
  FOnError := nil;
end;

{***********************************************}
procedure TALMongoDBChangeStreamListener.Execute;
begin
  While not terminated do begin

    try

      var LError: bson_error_t;
      var LMongoUri := mongoc_uri_new_with_error(
                         PAnsiChar(FUri), // uri_string: PAnsiChar;
                         @LError); // error: Pbson_error_t
      If LMongoUri = nil then ALRaiseMongoDBError(LError);
      Try

        var LClient := mongoc_client_new_from_uri_with_error(
                         LMongoUri, // uri: Pmongoc_uri_t;
                         @LError); // error: Pbson_error_t
        If LClient = nil then ALRaiseMongoDBError(LError);
        try

          {$REGION 'mongoc_collection_watch'}
          if FCollectionName <> '' then begin

            var LCollection := mongoc_client_get_collection(
                                 LClient, // client: Pmongoc_client_t;
                                 PAnsiChar(FDatabaseName), // db: PAnsiChar;
                                 PAnsiChar(FCollectionName)); // collection: PAnsiChar
            if LCollection = nil then raise Exception.Create('mongoc_client_get_collection failed');
            try

              var LPipelineBsonRec: bson_t;
              var LPipelineBsonIsAllocated: Boolean;
              var LPipelineBsonPtr := ALMakeBsonPtrFromJSONString(FPipeline, @LPipelineBsonRec, LPipelineBsonIsAllocated);
              try

                var LOptsBsonRec: bson_t;
                var LOptsBsonIsAllocated := False;
                var LOptsBsonPtr: Pbson_t;
                if FOpts <> '' then LOptsBsonPtr := ALMakeBsonPtrFromJSONString(FOpts, @LOptsBsonRec, LOptsBsonIsAllocated)
                else LOptsBsonPtr := nil;
                try

                  var LChangeStream := mongoc_collection_watch(
                                         LCollection, // coll: Pmongoc_collection_t;
                                         LPipelineBsonPtr, // pipeline: Pbson_t;
                                         LOptsBsonPtr); // opts: Pbson_t);
                  if LChangeStream = nil then raise Exception.Create('mongoc_collection_watch failed');
                  try

                    var LStream := TPointerStream.Create(nil{Ptr}, 0{Size}, true{ReadOnly});
                    try

                      var LPbson: Pbson_t;
                      while not terminated do begin
                        if mongoc_change_stream_next(LChangeStream, @LPbson) then begin
                          if LPbson = nil then raise Exception.Create('mongoc_change_stream_next failed');
                          LStream.SetPointer(bson_get_data(LPbson), LPbson^.len);
                          var LChangeDoc := TALJSONDocumentA.CreateFromBSONStream(LStream);
                          try
                            DoChange(LChangeDoc);
                          finally
                            ALFreeAndNil(LChangeDoc);
                          end;
                        end
                        else if (not terminated) and (mongoc_change_stream_error_document(LChangeStream, @LError, @LPbson)) then
                          ALRaiseMongoDBError(LError);
                      end;

                    finally
                      ALFreeAndNil(LStream);
                    end;

                  finally
                    mongoc_change_stream_destroy(LChangeStream);
                  end;

                finally
                  if LOptsBsonIsAllocated then
                    bson_destroy(LOptsBsonPtr);
                end;

              finally
                if LPipelineBsonIsAllocated then
                  bson_destroy(LPipelineBsonPtr);
              end;

            finally
              mongoc_collection_destroy(LCollection);
            end;

          end
          {$ENDREGION}

          {$REGION 'mongoc_database_watch'}
          else if FDatabaseName <> '' then begin

            var LDatabase := mongoc_client_get_database(
                               LClient, // client: Pmongoc_client_t;
                               PAnsiChar(FDatabaseName)); // name: PAnsiChar
            if LDatabase = nil then raise Exception.Create('mongoc_client_get_database failed');
            try

              var LPipelineBsonRec: bson_t;
              var LPipelineBsonIsAllocated: Boolean;
              var LPipelineBsonPtr := ALMakeBsonPtrFromJSONString(FPipeline, @LPipelineBsonRec, LPipelineBsonIsAllocated);
              try

                var LOptsBsonRec: bson_t;
                var LOptsBsonIsAllocated := False;
                var LOptsBsonPtr: Pbson_t;
                if FOpts <> '' then LOptsBsonPtr := ALMakeBsonPtrFromJSONString(FOpts, @LOptsBsonRec, LOptsBsonIsAllocated)
                else LOptsBsonPtr := nil;
                try

                  var LChangeStream := mongoc_database_watch(
                                         LDatabase, // db: Pmongoc_database_t;
                                         LPipelineBsonPtr, // pipeline: Pbson_t;
                                         LOptsBsonPtr); // opts: Pbson_t
                  if LChangeStream = nil then raise Exception.Create('mongoc_database_watch failed');
                  try

                    var LStream := TPointerStream.Create(nil{Ptr}, 0{Size}, true{ReadOnly});
                    try

                      var LPbson: Pbson_t;
                      while not terminated do begin
                        if mongoc_change_stream_next(LChangeStream, @LPbson) then begin
                          if LPbson = nil then raise Exception.Create('mongoc_change_stream_next failed');
                          LStream.SetPointer(bson_get_data(LPbson), LPbson^.len);
                          var LChangeDoc := TALJSONDocumentA.CreateFromBSONStream(LStream);
                          try
                            DoChange(LChangeDoc);
                          finally
                            ALFreeAndNil(LChangeDoc);
                          end;
                        end
                        else if (not terminated) and (mongoc_change_stream_error_document(LChangeStream, @LError, @LPbson)) then
                          ALRaiseMongoDBError(LError);
                      end;

                    finally
                      ALFreeAndNil(LStream);
                    end;

                  finally
                    mongoc_change_stream_destroy(LChangeStream);
                  end;

                finally
                  if LOptsBsonIsAllocated then
                    bson_destroy(LOptsBsonPtr);
                end;

              finally
                if LPipelineBsonIsAllocated then
                  bson_destroy(LPipelineBsonPtr);
              end;

            finally
               mongoc_database_destroy(LDatabase);
            end;

          end
          {$ENDREGION}

          {$REGION 'mongoc_client_watch'}
          else begin

            var LPipelineBsonRec: bson_t;
            var LPipelineBsonIsAllocated: Boolean;
            var LPipelineBsonPtr := ALMakeBsonPtrFromJSONString(FPipeline, @LPipelineBsonRec, LPipelineBsonIsAllocated);
            try

              var LOptsBsonRec: bson_t;
              var LOptsBsonIsAllocated := False;
              var LOptsBsonPtr: Pbson_t;
              if FOpts <> '' then LOptsBsonPtr := ALMakeBsonPtrFromJSONString(FOpts, @LOptsBsonRec, LOptsBsonIsAllocated)
              else LOptsBsonPtr := nil;
              try

                var LChangeStream := mongoc_client_watch(
                                       LClient, // client: Pmongoc_client_t;
                                       LPipelineBsonPtr, // pipeline: Pbson_t;
                                       LOptsBsonPtr); // opts: Pbson_t
                if LChangeStream = nil then raise Exception.Create('mongoc_client_watch failed');
                try

                  var LStream := TPointerStream.Create(nil{Ptr}, 0{Size}, true{ReadOnly});
                  try

                    var LPbson: Pbson_t;
                    while not terminated do begin
                      if mongoc_change_stream_next(LChangeStream, @LPbson) then begin
                        if LPbson = nil then raise Exception.Create('mongoc_change_stream_next failed');
                        LStream.SetPointer(bson_get_data(LPbson), LPbson^.len);
                        var LChangeDoc := TALJSONDocumentA.CreateFromBSONStream(LStream);
                        try
                          DoChange(LChangeDoc);
                        finally
                          ALFreeAndNil(LChangeDoc);
                        end;
                      end
                      else if (not terminated) and (mongoc_change_stream_error_document(LChangeStream, @LError, @LPbson)) then
                        ALRaiseMongoDBError(LError);
                    end;

                  finally
                    ALFreeAndNil(LStream);
                  end;

                finally
                  mongoc_change_stream_destroy(LChangeStream);
                end;

              finally
                if LOptsBsonIsAllocated then
                  bson_destroy(LOptsBsonPtr);
              end;

            finally
              if LPipelineBsonIsAllocated then
                bson_destroy(LPipelineBsonPtr);
            end;

          end;
          {$ENDREGION}

        finally
          mongoc_client_destroy(LClient);
        end;

      Finally
        mongoc_uri_destroy(LMongoUri);
      End;

    except
      On E: Exception do begin
        DoError(E);
        Sleep(1000);
      end;
    end;

  end;
end;

{********************************************************************************}
procedure TALMongoDBChangeStreamListener.DoChange(const AChangeDoc: TALJsonNodeA);
begin
  If Assigned(FOnChange) then
    FOnChange(Self, AChangeDoc);
end;

{************************************************************************}
procedure TALMongoDBChangeStreamListener.DoError(const AError: Exception);
begin
  If Assigned(FOnError) then
    FOnError(Self, AError)
  else
    ALLog('TALMongoDBChangeStreamListener', AError);
end;

end.