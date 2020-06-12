unit PerfTestCases;

interface

{$I Synopse.inc}

// enable/disable third-party libraries
{.$define USENEXUSDB}
{.$define USEBDE}
{.$define USEUNIDAC}
{.$define USEZEOS}
{.$define USEFIREDAC}

// enable/disable database engines
{.$define USEJET}
{.$define USEFIREBIRDEMB}
{.$define ODBCSQLITEFIREBIRD}
{.$define USELOCALMSSQLEXPRESS}    // SQL Server 2008 R2 Express locally
{.$define USELOCALDBMSSQLEXPRESS}  // SQL Server 2012 LocalDB edition
{.$define USELOCALDB2}
{.$define USELOCALPOSTGRESQL}
{.$define USELOCALMYSQL}
{.$define USEMONGODB}

{$ifdef CPU64}
  {$undef USENEXUSDB} // official NexusDB is not yet 64 bit ready :(
  {$undef USEJET}     // MS Access / JET is not available under Win64
{$endif}

// if defined, will create two "stored false" properties, to test UNIQUE columns
{.$define UNIK}


uses
  SysUtils,
  SynCommons,
  SynTable,
  SynLog,
  SynTests,
  mORMot,
  SynSQLite3, SynSQLite3Static,
  SynDB, SynDBSQLite3, SynDBOracle, SynDBPostgres, SynDBRemote,
  mORMotSQLite3,
  {$ifdef MSWINDOWS}
  SynOleDB,
  SynDBDataSet,
  {$endif}
  SynDBODBC,
  {$ifdef USENEXUSDB}
    SynDBNexusDB,
  {$endif}
  {$ifdef USEBDE}
    SynDBBDE,
  {$endif}
  {$ifdef USEUNIDAC}
    SynDBUniDAC,
    SQLiteUniProvider, InterbaseUniProvider, OracleUniProvider, DB2UniProvider,
    SQLServerUniProvider, PostgreSQLUniProvider, MySqlUniProvider,
  {$endif}
  {$ifdef USEZEOS}
    SynDBZeos,
  {$endif}
  {$ifdef USEFIREDAC}
    SynDBFireDAC,
    {$ifdef ISDELPHIXE5}
    FireDAC.Phys.Oracle, FireDAC.Phys.MSAcc, FireDAC.Phys.MSSQL, FireDAC.Phys.MySQL,
    FireDAC.Phys.SQLite, FireDAC.Phys.IB, FireDAC.Phys.PG, FireDAC.Phys.DB2,
    {$else}
    uADPhysOracle, uADPhysMSAcc, uADPhysMSSQL,
    uADPhysSQLite, uADPhysIB, uADPhysPG, uADPhysDB2, uADPhysMySQL,
    {$endif}
  {$endif}
  {$ifdef USEMONGODB}
    SynMongoDB, mORMotMongoDB,
  {$endif}
  mORMotDB;

type
  TStat = class(TSynPersistent)
  private
    fCreateTable: RawUTF8;
    fNumberOfElements: integer;
    fInsertTime: RawUTF8;
    fEngine: RawUTF8;
    fClientCloseTime: RawUTF8;
    fInsertRate: integer;
    fReadOneByOneTime: RawUTF8;
    fReadOneByOneRate: integer;
    fInsertBatchTransactionRate: integer;
    fInsertTransactionRate: integer;
    fInsertBatchRate: integer;
    fInsertBatchTransactionTime: RawUTF8;
    fInsertTransactionTime: RawUTF8;
    fInsertBatchTime: RawUTF8;
    fReadAllVirtualRate: integer;
    fReadAllDirectRate: integer;
    fReadAllDirectTime: RawUTF8;
    fReadAllVirtualTime: RawUTF8;
    {$ifdef UNIK}
    fReadOneByNameRate: integer;
    fReadOneByNameTime: RawUTF8;
    {$endif}
  published
    property Engine: RawUTF8 read fEngine;
    property CreateTableTime: RawUTF8 read fCreateTable write fCreateTable;
    property NumberOfElements: integer read fNumberOfElements write fNumberOfElements;
    property InsertTime: RawUTF8 read fInsertTime;
    property InsertRate: integer read fInsertRate;
    property InsertBatchTime: RawUTF8 read fInsertBatchTime;
    property InsertBatchRate: integer read fInsertBatchRate;
    property InsertTransactionTime: RawUTF8 read fInsertTransactionTime;
    property InsertTransactionRate: integer read fInsertTransactionRate;
    property InsertBatchTransactionTime: RawUTF8 read fInsertBatchTransactionTime;
    property InsertBatchTransactionRate: integer read fInsertBatchTransactionRate;
    property ReadOneByOneTime: RawUTF8 read fReadOneByOneTime;
    property ReadOneByOneRate: integer read fReadOneByOneRate;
    {$ifdef UNIK}
    property ReadOneByNameTime: RawUTF8 read fReadOneByNameTime;
    property ReadOneByNameRate: integer read fReadOneByNameRate;
    {$endif}
    property ReadAllVirtualTime: RawUTF8 read fReadAllVirtualTime;
    property ReadAllVirtualRate: integer read fReadAllVirtualRate;
    property ReadAllDirectTime: RawUTF8 read fReadAllDirectTime;
    property ReadAllDirectRate: integer read fReadAllDirectRate;
    property ClientCloseTime: RawUTF8 read fClientCloseTime;
  end;

  TSQLRecordSample = class(TSQLRecord)
  private
    fFirstName: RawUTF8;
    fLastName: RawUTF8;
    fAmount: Currency;
    fBirthDate: TDateTime;
    fLastChange: TModTime;
    fCreatedAt: TCreateTime;
  published
    property FirstName: RawUTF8 index 40 read fFirstName write fFirstName
      {$ifdef UNIK}stored AS_UNIQUE{$endif};
    property LastName: RawUTF8 index 40 read fLastName write fLastName
      {$ifdef UNIK}stored AS_UNIQUE{$endif};
    property Amount: Currency read fAmount write fAmount;
    property BirthDate: TDateTime read fBirthDate write fBirthDate;
    property LastChange: TModTime read fLastChange write fLastChange;
    property CreatedAt: TCreateTime read fCreatedAt write fCreatedAt;
  end;


  TTestDatabaseBenchmark = class(TSynTestsLogged)
  public
    Ini: RawUTF8;
    Stats: TSynObjectList;
    constructor Create(const Ident: string = ''); override;
    destructor Destroy; override;
    procedure SaveStats;
  published
    procedure DirectDatabaseAccess;
    procedure ExternalDatabaseAccess;
  end;

  TTestDatabaseAbstract = class(TSynTestCase)
  protected
    Main: TTestDatabaseBenchmark;
    Value: TSQLRecordSample;
    Stat: TStat;
    Namee, Num: RawUTF8;
    ChangeStart: TTimeLog;
    RunTimer: TPrecisionTimer;
    SQlite3Mode: TSQLSynchronousMode;
    SQlite3Lock: TSQLLockingMode;
    Client: TSQLRestClientDB;
    DBFileName: TFileName;
    DBPassword: RawUTF8;
    ValueLastName, ValueFirstName: TRawUTF8DynArray;
    Res: TIDDynArray;
    Flags: set of (dbIsFile, dbInMemory, dbInMemoryVirtual, dbPropIsMemory,
      dbPropUntouched, dbDropTable);
    procedure Setup; override;
    procedure Cleanup; override;
    procedure MethodSetup; override;
    procedure MethodCleanup; override;
    procedure RunTests; virtual;
    function ModelCreate: TSQLModel; virtual;
    procedure ClientCreate; virtual;
    procedure ClientFree; virtual;
    procedure RunWrites(UseTransactions, UseBatch: boolean); virtual;
    procedure ValueCheck(i: PtrInt);
    procedure RunModeLock(Mode: TSQLSynchronousMode; Lock: TSQLLockingMode); virtual;
  end;

  TTestDirectSqliteEngine = class(TTestDatabaseAbstract)
  protected
    procedure RunModeLock(Mode: TSQLSynchronousMode; Lock: TSQLLockingMode); override;
  published
    procedure SqliteFileFull;
    procedure SqliteFileOff;
    procedure SqliteFileOffExc;
    procedure SqliteFileOffExcAes;
    procedure SqliteInMemory;
  end;

  TTestInMemoryEngine = class(TTestDatabaseAbstract)
  protected
    function ModelCreate: TSQLModel; override;
    procedure ClientCreate; override;
  published
    procedure InMemoryStatic;
    procedure InMemoryVirtual;
  end;

  TTestDatabaseExternalAbstract = class(TTestDatabaseAbstract)
  protected
    Props: TSQLDBConnectionProperties;
    function ModelCreate: TSQLModel; override;
    procedure ClientCreate; override;
    procedure ClientFree; override;
    procedure RunExternal(P: TSQLDBConnectionProperties); virtual;
  public
  end;

  TTestSqliteExternal = class(TTestDatabaseExternalAbstract)
  protected
    procedure ClientCreate; override;
  published
    procedure ExternalSqliteFileFull;
    procedure ExternalSqliteFileOff;
    procedure ExternalSqliteFileOffExc;
    procedure ExternalSqliteInMemory;
  end;

  TTestSqliteRemote = class(TTestDatabaseExternalAbstract)
  protected
    RemoteProps: TSQLDBSQLite3ConnectionProperties;
    RemoteServer: TSQLDBServerRemote;
    RemoteClient: TSQLDBConnectionPropertiesClass;
    procedure ClientCreate; override;
    procedure ClientFree; override;
  published
    {$ifdef MSWINDOWS}
    procedure RemoteSqliteWinHTTP;
    {$endif}
    procedure RemoteSqliteSocket;
  end;

  {$ifdef USELOCALPOSTGRESQL}
  TTestPostgresql = class(TTestDatabaseExternalAbstract)
  published
    procedure _SynDBPostgres;
    {$ifdef USEZEOS}
    procedure ZeosPostgres;
    {$endif}
  end;
  {$endif USELOCALPOSTGRESQL}

implementation

{ TTestDatabaseAbstract }

procedure TTestDatabaseAbstract.Setup;
begin
  EnsureDirectoryExists(ExeVersion.ProgramFilePath + 'db');
  Main := Owner as TTestDatabaseBenchmark;
  Value := TSQLRecordSample.Create;
  Namee := 'Name/ ';
  UniqueRawUTF8(Namee); // FPC does not call it
  PWord(@Namee[4])^ := $a9c3;  // some 'e'acute to test UTF-8 encoding
  SQlite3Mode := smOff; // fastest mode by default
  SQlite3Lock := lmExclusive;
end;

procedure TTestDatabaseAbstract.Cleanup;
begin // warning: this method could be called several times after a single Setup
  FreeAndNil(Value);
end;

procedure TTestDatabaseAbstract.MethodSetup;
begin
  Flags := [];
  Stat := TStat.Create;
  Stat.fEngine := ToUTF8(Owner.CurrentMethodInfo^.TestName);
end;

procedure TTestDatabaseAbstract.MethodCleanup;
begin
  if Stat.Engine <> '' then
    Main.Stats.Add(Stat)
  else
    FreeAndNil(Stat);
  ClientFree;
end;

procedure TTestDatabaseAbstract.RunTests;
const
  _ID: array['1'..'4'] of RawUTF8 = ('', ' batch', ' trans', ' batch trans');
var
  trans, batch, UseDirect: boolean;
  time: RawUTF8;
  rate: QWord;
  i: PtrInt;
  log: ISynLog;
begin
  // Insertion tests
  Num := '1';
  for trans := false to true do
    for batch := false to true do
    begin
      log := TSynLog.Enter('% Insert%',[Owner.CurrentMethodInfo^.IdentTestName,
        _ID[Num[1]]],self);
      RunWrites(trans, batch);
      NotifyTestSpeed('insert%',[_ID[Num[1]]],Stat.NumberOfElements,0,@RunTimer);
      time := RunTimer.LastTime;
      rate := RunTimer.PerSec(Stat.NumberOfElements);
      log := nil;
      case Num[1] of
      '1':
        begin
          Stat.fInsertTime := time;
          Stat.fInsertRate := rate;
        end;
      '2':
        begin
          Stat.fInsertBatchTime := time;
          Stat.fInsertBatchRate := rate;
        end;
      '3':
        begin
          Stat.fInsertTransactionTime := time;
          Stat.fInsertTransactionRate := rate;
        end;
      '4':
        begin
          Stat.fInsertBatchTransactionTime := time;
          Stat.fInsertBatchTransactionRate := rate;
          break; // still need Client for Read tests
        end;
      end;
      inc(Num[1]);
      ClientFree;
    end;
  // Read tests
  log := TSynLog.Enter('% Read One',[Owner.CurrentMethodInfo^.IdentTestName],self);
  Value.ClearProperties;
  RunTimer.Start;
  for i := 0 to Stat.NumberOfElements-1 do
  begin
    Client.Retrieve(Res[i],Value);
    ValueCheck(i);
  end;
  NotifyTestSpeed('read one',Stat.NumberOfElements,0,@RunTimer);
  Stat.fReadOneByOneTime := RunTimer.LastTime;
  Stat.fReadOneByOneRate := RunTimer.PerSec(Stat.NumberOfElements);
  log := nil;
  {$ifdef UNIK}
  // one by one retrieve values using Name property
  log := TSynLog.Enter('% Read Unique',[Owner.CurrentMethodInfo^.IdentTestName],self);
  RunTimer.Start;
  for i := 0 to Stat.NumberOfElements-1 do
  begin
    Client.Retrieve('LastName=?',[],[ValueLastName[i]],Value);
    Check((Value.IDValue=Res[i]) and
      (PInt64(@Value.Amount)^=(i+1)*100) and
      (Value.LastChange>=ChangeStart));
  end;
  NotifyTestSpeed('read unique',Stat.NumberOfElements,0,@RunTimer);
  Stat.fReadOneByNameTime := RunTimer.LastTime;
  Stat.fReadOneByNameRate := RunTimer.PerSec(Stat.NumberOfElements);
  {$endif UNIK}
  // retrieve all rows with or without the virtual module
  for UseDirect := false to true do
  begin
    log := nil;
    log := TSynLog.Enter('% Read Direct=%',
      [Owner.CurrentMethodInfo^.IdentTestName, BOOL_STR[UseDirect]],self);
    with Client.Server do
    begin
      Cache.Flush; // fair benchmark
      DB.CacheFlush; // fair benchmark (16100 rows/s->456000 with cache!)
      StaticVirtualTableDirect := UseDirect;
    end;
    RunTimer.Start;
    Value.ClearProperties;
    Check(Value.FillPrepare(Client,'order by RowId'), 'FillPrepare');
    //FileFromString((Value.FillTable as TSQLTableJSON).PrivateInternalCopy,Stat.Engine+'.json');
    i := 0;
    while Value.FillOne do
    begin
      ValueCheck(i);
      inc(i);
    end;
    CheckEqual(i,Stat.NumberOfElements,'FillOne');
    if UseDirect then
    begin
      NotifyTestSpeed('read direct',Stat.NumberOfElements,0,@RunTimer);
      Stat.fReadAllDirectTime := RunTimer.LastTime;
      Stat.fReadAllDirectRate := RunTimer.PerSec(Stat.NumberOfElements);
    end else
    begin
      NotifyTestSpeed('read virtual',Stat.NumberOfElements,0,@RunTimer);
      Stat.fReadAllVirtualTime := RunTimer.LastTime;
      Stat.fReadAllVirtualRate := RunTimer.PerSec(Stat.NumberOfElements);
    end;
  end;
end;

function TTestDatabaseAbstract.ModelCreate: TSQLModel;
begin
  result := TSQLModel.Create([TSQLRecordSample]);
end;

procedure TTestDatabaseAbstract.ClientCreate;
var
  fn: TFileName;
begin
  if dbIsFile in Flags then
    fn := DBFileName
  else
    fn := SQLITE_MEMORY_DATABASE_NAME;
  Client := TSQLRestClientDB.Create(ModelCreate, nil, fn, TSQLRestServerDB,
    {auth=}false, DBPassword);
  Client.Model.Owner := Client;
  Client.Server.DB.Synchronous := SQlite3Mode;
  Client.Server.DB.LockingMode := SQlite3Lock;
end;

procedure TTestDatabaseAbstract.ClientFree;
begin
  FreeAndNil(Client);
end;

procedure TTestDatabaseAbstract.RunWrites(UseTransactions, UseBatch: boolean);
var
  i: PtrInt;
  forceID: boolean;
begin
  DBFileName := FormatString('%db%%.%.db',[ExeVersion.ProgramFilePath, PathDelim,
    Owner.CurrentMethodInfo^.MethodName, Num]);
  if FileExists(DBFileName) then
    DeleteFile(DBFileName);
  RunTimer.Start;
  ClientCreate;
  if CheckFailed(Client <> nil,'Client?') then
    exit; // avoid GPF
  Client.Server.CreateMissingTables;
  ChangeStart := Client.ServerTimestamp; // use by ValueCheck
  if Stat.CreateTableTime='' then
    Stat.CreateTableTime := RunTimer.Stop;
  if (SQlite3Mode = smFull) and not UseTransactions then // full synch is slow
    Stat.NumberOfElements := 500
  else
    Stat.NumberOfElements := 5000;
  SetLength(ValueLastName, Stat.NumberOfElements);
  SetLength(ValueFirstName, Stat.NumberOfElements);
  for i := 0 to Stat.NumberOfElements - 1 do
    if ValueLastName[i] = '' then
    begin
      UInt32ToUtf8(i + 1, ValueLastName[i]);
      {$ifndef UNIK}
      if i <> 100 then // test https://synopse.info/fossil/info/e8c211062e
      {$endif}
        ValueFirstName[i] := Namee + ValueLastName[i];
    end;
  RunTimer.Start;
  if UseTransactions then
    Client.TransactionBegin(TSQLRecordSample);
  if UseBatch then
    Client.BatchStart(TSQLRecordSample)
  else
    if length(Res)<Stat.NumberOfElements then
      SetLength(Res,Stat.NumberOfElements);
  for i := 0 to Stat.NumberOfElements-1 do
  begin
    Value.Amount := (i+1)*0.01;
    Value.LastName := ValueLastName[i];
    Value.FirstName := ValueFirstName[i];
    Value.BirthDate := i+1;
    forceID := i and 3=1;
    if forceID then
      if {$ifdef UNIK}(dbInMemory in Flags) or {$endif} (Res[i-1] = 0) then
        forceID := false // not yet in TSQLRestStorageInMemory.AddOne
      else
        Value.IDValue := Res[i-1]+1;
    if UseBatch then
      Check(Client.BatchAdd(Value,true,forceID)>=0)
    else
    begin
      Res[i] := Client.Add(Value,true,forceID);
      Check(Res[i]>0,'Add');
    end;
  end;
  if UseBatch then
    Check(Client.BatchSend(Res)=HTTP_SUCCESS);
  if UseTransactions then
    Client.Commit;
  Value.ClearProperties;
  Check(Client.Retrieve(Res[1],Value),'One Retrieve after Add');
  ValueCheck(1);
end;

procedure TTestDatabaseAbstract.ValueCheck(i: PtrInt);
begin
  CheckEqual(Value.IDValue, Res[i], 'ID');
  CheckEqual(PInt64(@Value.Amount)^, (i+1)*100, 'Amount');
  Check(Value.LastChange >= ChangeStart, 'LastChange');
  CheckEqual(Value.FirstName, ValueFirstName[i], 'FirstName');
  Value.IDValue := 0;
  Value.Amount := 0;
  Value.FirstName := '';
  Value.LastChange := 0;
end;

procedure TTestDatabaseAbstract.RunModeLock(Mode: TSQLSynchronousMode;
  Lock: TSQLLockingMode);
begin
  SQlite3Mode := Mode;
  SQlite3Lock := Lock;
  RunTests;
end;


{ TTestDirectSqliteEngine }

procedure TTestDirectSqliteEngine.RunModeLock(Mode: TSQLSynchronousMode; Lock: TSQLLockingMode);
begin
  Flags := [dbIsFile];
  inherited RunModeLock(Mode, Lock);
end;

procedure TTestDirectSqliteEngine.SqliteFileFull;
begin
  RunModeLock(smFull, lmNormal);
end;

procedure TTestDirectSqliteEngine.SqliteFileOff;
begin
  RunModeLock(smOff, lmNormal);
end;

procedure TTestDirectSqliteEngine.SqliteFileOffExc;
begin
  RunModeLock(smOff, lmExclusive);
end;

procedure TTestDirectSqliteEngine.SqliteFileOffExcAes;
begin
  DBPassword := 'password';
  RunModeLock(smOff, lmExclusive);
  DBPassword := '';
end;

procedure TTestDirectSqliteEngine.SqliteInMemory;
begin
  Flags := [];
  RunTests;
end;


{ TTestInMemoryEngine }

function TTestInMemoryEngine.ModelCreate: TSQLModel;
begin
  result := inherited ModelCreate;
  // registration should be done BEFORE Client is initialized
  if dbInMemoryVirtual in Flags then
    result.VirtualTableRegister(TSQLRecordSample, TSQLVirtualTableBinary)
end;

procedure TTestInMemoryEngine.ClientCreate;
begin
  inherited ClientCreate;
  if not (dbInMemoryVirtual in Flags) then
    Client.Server.StaticDataCreate(TSQLRecordSample, DBFileName, {binary=}true);
end;

procedure TTestInMemoryEngine.InMemoryStatic;
begin
  Flags := [dbInMemory];
  RunTests;
end;

procedure TTestInMemoryEngine.InMemoryVirtual;
begin
  Flags := [dbInMemory, dbInMemoryVirtual];
  RunTests;
end;


{ TTestDatabaseExternalAbstract }

function TTestDatabaseExternalAbstract.ModelCreate: TSQLModel;
begin
  result := inherited ModelCreate;
  // registration should be done BEFORE Client is initialized
  VirtualTableExternalRegister(result, TSQLRecordSample, Props, 'SampleRecord');
end;

procedure TTestDatabaseExternalAbstract.ClientCreate;
begin
  if (Props <> nil) and (dbDropTable in Flags) then
    Props.ExecuteNoResult('drop table SAMPLERECORD', []);
  inherited ClientCreate;
end;

procedure TTestDatabaseExternalAbstract.ClientFree;
begin
  inherited ClientFree;
  if not(dbPropUntouched in Flags) then
    FreeAndNil(Props);
end;

procedure TTestDatabaseExternalAbstract.RunExternal(P: TSQLDBConnectionProperties);
begin
  Flags := [dbPropUntouched, dbDropTable];
  Props := P;
  try
    Props.ThreadSafeConnection.Connect;
    Check(Props.ThreadSafeConnection.Connected, 'connected');
    RunTests;
  finally
    FreeAndNil(Props);
  end;
end;


{ TTestSqliteExternal }

procedure TTestSqliteExternal.ClientCreate;
begin
  if dbPropIsMemory in Flags then
    DBFileName := SQLITE_MEMORY_DATABASE_NAME;
  if not (dbPropUntouched in Flags) then
    Props := TSQLDBSQLite3ConnectionProperties.Create(DBFileName, '', '', '');
  with TSQLDBSQLite3Connection(Props.MainConnection) do
  begin
    Synchronous := SQlite3Mode;
    LockingMode := SQlite3Lock;
  end;
  inherited ClientCreate;
end;

procedure TTestSqliteExternal.ExternalSqliteFileFull;
begin
  RunModeLock(smFull, lmNormal);
end;

procedure TTestSqliteExternal.ExternalSqliteFileOff;
begin
  RunModeLock(smOff, lmNormal);
end;

procedure TTestSqliteExternal.ExternalSqliteFileOffExc;
begin
  RunModeLock(smOff, lmExclusive);
end;

procedure TTestSqliteExternal.ExternalSqliteInMemory;
begin
  Flags := [dbPropIsMemory];
  RunTests;
end;


{ TTestSqliteRemote }

procedure TTestSqliteRemote.ClientCreate;
begin
  RemoteProps := TSQLDBSQLite3ConnectionProperties.Create(SQLITE_MEMORY_DATABASE_NAME, '', '', '');
  RemoteProps.MainSQLite3DB.Synchronous := SQlite3Mode;
  RemoteProps.MainSQLite3DB.LockingMode := SQlite3Lock;
  RemoteServer := TSQLDBServerRemote.Create(RemoteProps, 'root', '8888', 'user', 'password');
  Props := RemoteClient.Create('localhost:8888', 'root', 'user', 'password');
  inherited ClientCreate;
end;

procedure TTestSqliteRemote.ClientFree;
begin
  inherited ClientFree;
  RemoteServer.Free;
  RemoteProps.Free;
end;

procedure TTestSqliteRemote.RemoteSqliteSocket;
begin
  RemoteClient := TSQLDBSocketConnectionProperties;
  RunTests;
end;

{$ifdef MSWINDOWS}
procedure TTestSqliteRemote.RemoteSqliteWinHTTP;
begin
  RemoteClient := TSQLDBWinHTTPConnectionProperties;
  RunTests;
end;
{$endif MSWINDOWS}


{$ifdef USELOCALPOSTGRESQL}

{ TTestPostgresql }

procedure TTestPostgresql._SynDBPostgres;
begin
  RunExternal(TSQLDBPostgresConnectionProperties.Create(
    'localhost','postgres','postgres','docker'));
end;


{$ifdef USEZEOS}
procedure TTestPostgresql.ZeosPostgres;
begin
  RunExternal(TSQLDBZEOSConnectionProperties.Create(TSQLDBZEOSConnectionProperties.URI(
    dPostgreSQL,'localhost','libpq.so.5',false),'postgres','postgres','docker'));
end;
{$endif}

{$endif USELOCALPOSTGRESQL}


{ TTestDatabaseBenchmark }

constructor TTestDatabaseBenchmark.Create(const Ident: string);
var
  fn: TFileName;
begin
  Stats := TSynObjectList.Create;
  fn := ChangeFileExt(ExeVersion.ProgramFileName, '.ini');
  if FileExists(fn) then
    Ini := StringFromFile(fn)
  else
    FileFromString('', fn);
  inherited Create(Ident);
end;

destructor TTestDatabaseBenchmark.Destroy;
begin
  inherited Destroy;
  SaveStats;
  Stats.Free;
end;

procedure TTestDatabaseBenchmark.SaveStats;
type TStatArray = array[0..1000] of TStat;
var Stat: ^TStatArray;
    mode,s,txt: RawUTF8;
    m,nCat, col1len: integer;
    max,Cat1,Cat2,Eng1,Eng2,Eng: RawUTF8;
    Rows: TRawUTF8DynArray;
    Doc, Cons: RawUTF8;
  procedure SetCategories(const Title: RawUTF8; const Cat: array of RawUTF8);
  var i: integer;
  begin
    mode := UrlEncode(Title);
    s := s+'<h1>'+copy(Title,1,posEx(' (',Title)-1)+'</h1>'#13#10;
    max := Int32ToUtf8(m);
    nCat := length(Cat);
    Cat1 := '';
    Cat2 := '';
    SetLength(Rows,Stats.Count+1);
    Rows[0] := '<td>&nbsp;</td>';
    cons := cons+#13#10+Title+#13#10+StringOfChar(' ',col1len+2);
    for i := 0 to high(Cat) do begin
      Rows[0] := Rows[0]+'<td><b>'+Cat[i]+'</b></td>';
      Cat1 := Cat1+UrlEncode(Cat[i])+'|';
      Cat2 := Cat2+UrlEncode(Cat[high(Cat)-i])+'|';
      cons := cons+Cat[i];
      if i<>high(Cat) then
        cons := cons+StringOfChar(' ',12-length(Cat[i]));
    end;
    cons := cons+#13#10;
    SetLength(Cat1,length(Cat1)-1);
    SetLength(Cat2,length(Cat2)-1);
    Eng1 := '';
    Eng2 := '';
    for i := 0 to Stats.Count-1 do begin
      Eng := Stat[i].Engine;
     { j := PosEx(' ',Eng);
      if j>0 then begin
        Delete(Eng,j,1);
        insert('<br>',Eng,j);
      end;}
      Rows[i+1] := '<td><b>'+Eng+'</b></td>';
      Eng1 := Eng1+UrlEncode(Stat[i].Engine)+'|';
      Eng2 := Eng2+UrlEncode(Stat[Stats.Count-1-i].Engine)+'|';
    end;
    SetLength(Eng1,length(Eng1)-1);
    SetLength(Eng2,length(Eng2)-1);
  end;
  procedure Pic1(const Leg: RawUTF8; n: integer);
  var i: integer;
  begin
    txt := 'http://chart.apis.google.com/chart?chtt='+mode+'&chxl=1:|'+Leg+
      '&chxt=x,y&chbh=a&chs=600x500&cht=bhg&chco=';
  //  for i := 1 to 5 do txt := txt+IntToHex($309F30+i*$010101,3)+',';
  //  txt[length(txt)] := '&';
  //    '3D7930,3D8930,309F30,6070F0,5070E0,40C355,65D055,80C1A2,F05050,F0A280'+
    txt := txt+'3D7930,3D8930,309F30,40C355&';//,6070F0,5070E0,65D055,80C1A2,3D7930,3D8930,F05050,F04050,F04040,F01040,F0A280&';
    txt := txt+'chxr=0,0,'+max+'&chds=';
    for i := 1 to n do
      txt := txt+'0,'+max+',';
    txt[length(txt)] := '&';
    txt := txt+'chd=t:';
  end;
  procedure PicEnd(const Legend: RawUTF8);
  begin
    txt[length(txt)] := '&';
    s := s+'<p><img src='+txt+'chdl='+Legend+'></p>'#13#10;
    txt := '';
  end;
  procedure SetValues(var Rows: RawUTF8; const eng: RawUTF8; const v: array of const);
  var j: integer;
      fmt,s: RawUTF8;
  begin
    for j := 2 to length(v) do
      fmt := fmt+'%,';
    fmt := fmt+'%|';
    txt := txt+FormatUTF8(fmt,v);
    fmt := '';
    for j := 1 to length(v) do
      fmt := fmt+'<td>%</td>';
    Rows := Rows+FormatUTF8(fmt,v);
    fmt := eng+StringOfChar(' ',col1len-length(eng)+2);
    for j := 0 to high(v) do begin
      VarRecToUTF8(v[j],s);
      if j<>high(v) then
        s := s+StringOfChar(' ',12-length(s));
      fmt := fmt+s;
    end;
    cons := cons+fmt+#13#10;
  end;
  procedure Table;
  var i: integer;
  begin
    s := s+'<p><table>';
    for i := 0 to High(Rows) do
      s := s+'<tr align=center>'+Rows[i]+'</tr>'#13#10;
    s := s+'</table></p>';
    Doc := Doc+'|%30';
    for i := 1 to nCat do
      Doc := Doc+'%15';
    Doc := Doc+#13#10;
    for i := 0 to High(Rows) do begin
      Doc := Doc+StringReplaceAll(Rows[i], ['</td>','', '</tr>','', '<tr align=center>','',
        '</b>','}', '</td>','', '<b>','{\b ', '<td>','|', '&nbsp;',''])+#13#10;
    end;
    Doc := Doc+'|%'#13#10;
  end;
var i,j: integer;
begin
  // introducting text
  Stat := pointer(Stats.List);
  s := FormatUTF8('Running tests using Synopse mORMot framework %, '+
    'compiled with %, against SQLite %, on %, at %.',
    [SYNOPSE_FRAMEWORK_VERSION, GetDelphiCompilerVersion, SQLite3.libversion,
     OSVersionText, NowToString]);
  cons := '[code]'#13#10+s+#13#10#13#10;
  s := '<p>'+s+'</p>';
  // compute max Insertion rate value for charts
  m := 0;
  col1len := 0;
  for i := 0 to Stats.Count-1 do
    with Stat[i] do begin
      if InsertRate>m then m := InsertRate;
      if InsertBatchRate>m then m := InsertBatchRate;
      if InsertTransactionRate>m then m := InsertTransactionRate;
      if InsertBatchTransactionRate>m then m := InsertBatchTransactionRate;
      j := length(Engine);
      if j>col1len then
        col1len := j;
    end;
  // Insertion Categories
  SetCategories('Insertion speed (rows/second)',['Direct','Batch','Trans','Batch Trans']);
  // Insertion per-Engine Values and Chart
  Pic1(Cat2,5);
  for i := 0 to Stats.Count-1 do
  with Stat[i] do
    SetValues(Rows[i+1], Engine,
      [InsertRate,InsertBatchRate,InsertTransactionRate,InsertBatchTransactionRate]);
  Table;
  PicEnd(Eng1);
  // Insertion per-Category Chart
  Pic1(Eng2,Stats.Count);
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].InsertRate)+',';
  txt[length(txt)] := '|';
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].InsertBatchRate)+',';
  txt[length(txt)] := '|';
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].InsertTransactionRate)+',';
  txt[length(txt)] := '|';
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].InsertBatchTransactionRate)+',';
  PicEnd(Cat1);
  // compute max Reading rate value for charts
  m := 0;
  for i := 0 to Stats.Count-1 do
    with Stat[i] do begin
      if ReadOneByOneRate>m then m := ReadOneByOneRate;
      {$ifdef UNIK}
      if ReadOneByNameRate>m then m := ReadOneByNameRate;
      {$endif}
      if ReadAllVirtualRate>m then m := ReadAllVirtualRate;
      if ReadAllDirectRate>m then m := ReadAllDirectRate;
    end;
  // Reading Categories
  SetCategories('Read speed (rows/second)',['By one',
    {$ifdef UNIK}'By name',{$endif}'All Virtual','All Direct']);
  // Reading per-Engine Values and Chart
  Pic1(Cat2,{$ifdef UNIK}4{$else}3{$endif});
  for i := 0 to Stats.Count-1 do
    with Stat[i] do
      SetValues(Rows[i+1], Engine,
        [ReadOneByOneRate,{$ifdef UNIK}ReadOneByNameRate,{$endif}
          ReadAllVirtualRate,ReadAllDirectRate]);
  Table;
  PicEnd(Eng1);
  // Reading per-Category Chart
  Pic1(Eng2,Stats.Count);
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].ReadOneByOneRate)+',';
  txt[length(txt)] := '|';
  {$ifdef UNIK}
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].ReadOneByNameRate)+',';
  txt[length(txt)] := '|';
  {$endif}
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].ReadAllVirtualRate)+',';
  txt[length(txt)] := '|';
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].ReadAllDirectRate)+',';
  PicEnd(Cat1);
  // save to local files
  FileFromString(Doc,ChangeFileExt(ExeVersion.ProgramFileName,'.doc'));
  FileFromString(cons+'[/code]',ChangeFileExt(ExeVersion.ProgramFileName,'.txt'));
  FileFromString('<html><body>'#13#10+s,ChangeFileExt(ExeVersion.ProgramFileName,'.htm'));
end;

procedure TTestDatabaseBenchmark.DirectDatabaseAccess;
begin
  //exit;
  AddCase(TTestDirectSqliteEngine);
  AddCase(TTestInMemoryEngine);
end;

procedure TTestDatabaseBenchmark.ExternalDatabaseAccess;
begin
  //exit;
  AddCase(TTestSqliteExternal);
  AddCase(TTestSqliteRemote);
  {$ifdef USELOCALPOSTGRESQL}
  AddCase(TTestPostgresql);
  {$endif}
end;


end.
