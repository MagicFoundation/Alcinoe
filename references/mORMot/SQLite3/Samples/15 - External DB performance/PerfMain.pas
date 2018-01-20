unit PerfMain;

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

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Contnrs, ShellApi,
  SynCommons,
  mORMot, mORMotSQLite3, mORMotDB,
  SynDB, SynDBSQLite3, SynDBOracle, SynOleDB, SynDBODBC, SynDBDataSet,
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
  SynSQLite3, SynSQLite3Static,
  SynDBRemote;

type
  TMainForm = class(TForm)
    LogMemo: TMemo;
    OraTNSName: TEdit;
    OraUser: TEdit;
    OraPass: TEdit;
    Label1: TLabel;
    BtnRunTests: TButton;
    Label2: TLabel;
    Label3: TLabel;
    btnReport: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnRunTestsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnReportClick(Sender: TObject);
  private
    Ini: RawUTF8;
    Stats: TObjectList;
    procedure SaveStats;
  public
    procedure Test(PropsClass: TSQLDBConnectionPropertiesClass;
      const aServerName, aDatabaseName, aUserID, aPassWord, aTrailDesc: RawUTF8;
      DBIsFile: boolean; Mode: TSQLSynchronousMode=smNormal; Lock: TSQLLockingMode=lmNormal);
  end;

var
  MainForm: TMainForm;

implementation

uses DateUtils;

{$R *.dfm}

// if defined, will create two "stored false" properties, to test UNIQUE columns
{.$define UNIK}

type
  TStat = class(TPersistent)
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
    property CreateTableTime: RawUTF8 read fCreateTable;
    property NumberOfElements: integer read fNumberOfElements;
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

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Ini := StringFromFile(ChangeFileExt(ExeVersion.ProgramFileName,'.ini'));
  OraTNSName.Text := UTF8ToString(FindIniEntry(Ini,'Oracle','TNSName'));
  OraUser.Text := UTF8ToString(FindIniEntry(Ini,'Oracle','User'));
  OraPass.Text := UTF8ToString(FindIniEntry(Ini,'Oracle','Password'));
  Stats := TObjectList.Create;
end;

const
  FIREBIRD_LIB = 'Firebird'+{$ifdef CPU64}'64'+{$endif=}'\fbembed.dll';

procedure TMainForm.BtnRunTestsClick(Sender: TObject);
var T,U,P: RawUTF8;
    props: TSQLDBSQLite3ConnectionProperties;
    server: TSQLDBServerAbstract;
begin
  //SynDBLog.Family.Level := LOG_VERBOSE;  // for debugging
  T := StringToUTF8(OraTNSName.Text);
  U := StringToUTF8(OraUser.Text);
  P := StringToUTF8(OraPass.Text);
  UpdateIniEntry(Ini,'Oracle','TNSName',T);
  UpdateIniEntry(Ini,'Oracle','User',U);
  UpdateIniEntry(Ini,'Oracle','Password',P);
  FileFromString(Ini,ChangeFileExt(ExeVersion.ProgramFileName,'.ini'));
  LogMemo.Clear;
{  FreeAndNil(sqlite3); sqlite3 := TSQLite3LibraryDynamic.Create('sqlite3.dll'); }
  // if false then
  try
  try
    // -------- SQlite3
    //(*
    Test(nil,'','','','','SQLite3 (file full)',true,smFull);
    Test(nil,'','','','','SQLite3 (file off)',true,smOff);
    Test(nil,'','','','','SQLite3 (file off exc)',true,smOff,lmExclusive);
    Test(nil,SQLITE_MEMORY_DATABASE_NAME,'','','','SQLite3 (mem)',false);
    Test(nil,'static','','','','TObjectList (static)',true);
    Test(nil,'SQL','','','','TObjectList (virtual)',true);
    Test(TSQLDBSQLite3ConnectionProperties,'','','','',' (ext full)',true,smFull);
    Test(TSQLDBSQLite3ConnectionProperties,'','','','',' (ext off)',true,smOff);
    Test(TSQLDBSQLite3ConnectionProperties,'','','','',' (ext off exc)',true,smOff,lmExclusive);
    Test(TSQLDBSQLite3ConnectionProperties,SQLITE_MEMORY_DATABASE_NAME,'','','',' (ext mem)',true);
    DeleteFile('SQLite3 (http).db3');
    props := TSQLDBSQLite3ConnectionProperties.Create('sqlite3 (http).db3','','','');
    server := {TSQLDBServerSockets}TSQLDBServerHttpApi.Create(props,'root','888','user','password');
    try
      props.MainSQLite3DB.Synchronous := smOff;
      props.MainSQLite3DB.LockingMode := lmExclusive;
      Test(TSQLDBWinHTTPConnectionProperties,
        'localhost:888','root','user','password',' SQLite3 (off exc)',false);
      Test(TSQLDBSocketConnectionProperties,
        'localhost:888','root','user','password',' SQLite3 (off exc)',false);
    finally
      server.Free;
      props.Free;
    end;
    //*)
    {$ifdef USEMONGODB}
    Test(nil,'MongoDB','','','','MongoDB (ack)',false);
    Test(nil,'MongoDB','','','','MongoDB (no ack)',false);
    {$endif}
    {$ifdef ODBCSQLITEFIREBIRD}
    // download driver from http://www.ch-werner.de/sqliteodbc
    Test(TODBCConnectionProperties,'','DRIVER=SQLite3 ODBC Driver','','',' SQLite3',true);
    {$endif}
    {$ifndef CPU64} // latest ZDBC Sqlite3 driver expects column_origin_name()
    {$ifdef USEZEOS} // smFull -> only 50 rows if no batch mode
    Test(TSQLDBZEOSConnectionProperties,TSQLDBZEOSConnectionProperties.URI(dSQLite,''
      {$ifdef CPU64},'sqlite3-64.dll'{$endif}),'','','',' SQlite3',true,smFull);
    {$endif}
    {$endif}
    {$ifdef USEFIREDAC}
    Test(TSQLDBFireDACConnectionProperties,FIREDAC_PROVIDER[dSQLite],'','','',' SQlite3',true,smFull);
    {$endif}
    {$ifdef USEUNIDAC} // smFull -> only 50 rows if no batch mode
    Test(TSQLDBUniDACConnectionProperties,UNIDAC_PROVIDER[dSQLite],'','','',' SQlite3',true,smFull);
    {$endif}
    //*)

    // -------- Firebird embedded
    {$ifdef USEFIREBIRDEMB}
    //Test(TSQLDBFirebirdEmbeddedConnectionProperties,'','','','','',false);
    {$ifdef USEZEOS} // Test() will use ZDBC to create the DB for ODBC Firebird
    // expects Firebird_ODBC_*_Win32.exe from http://www.firebirdsql.org/en/odbc-driver
    {$ifdef ODBCSQLITEFIREBIRD}
    // download driver from http://www.firebirdsql.org/en/odbc-driver
    Test(TODBCConnectionProperties,'','DRIVER=Firebird/InterBase(r) driver;CHARSET=UTF8;'+
      'CLIENT='+FIREBIRD_LIB,'','',' Firebird',true);
    {$endif}
    Test(TSQLDBZEOSConnectionProperties,TSQLDBZEOSConnectionProperties.URI(dFirebird,'',
      FIREBIRD_LIB),'','','',' Firebird',true);
    {$endif}
    //(*
    {$ifdef USEFIREDAC}
    TADPhysIBDriverLink.Create(Application).VendorLib := FIREBIRD_LIB;
    Test(TSQLDBFireDACConnectionProperties,FIREDAC_PROVIDER[dFirebird]+'?CreateDatabase=Yes',
      '','','',' Firebird',true);
    {$endif}
    {$ifdef USEUNIDAC}
    Test(TSQLDBUniDACConnectionProperties,TSQLDBUniDACConnectionProperties.URI(
      dFirebird,'',FIREBIRD_LIB),'','','',' Firebird',true);
    {$endif}
    {$endif}

    // -------- JET / MS Access
    {$ifdef USEJET}
    Test(TOleDBJetConnectionProperties,'','','','','',true);
    {$endif}

    // -------- NexusDB Free Embedded edition
    {$ifdef USENEXUSDB}
    Test(TSQLDBNexusDBConnectionProperties,'.\nexusDB','','','','',false);
    {$endif}

    // -------- Oracle 11g over network
    if T<>'' then begin
      {$ifdef CPU64}
      SynDBOracleOCIpath := 'oci64';
      {$endif}
      Test(TSQLDBOracleConnectionProperties,T,'',U,P,'',false);
      {$ifndef CPU64}
      Test(TODBCConnectionProperties,TSQLDBOracleConnectionProperties.ExtractTnsName(T),
        '',U,P,' Oracle',false);
      {$endif}
      {$ifdef USEBDE}
      Test(TSQLDBBDEConnectionProperties,TSQLDBOracleConnectionProperties.ExtractTnsName(T),
        '',U,P,' Oracle',false);
      {$endif}
      // current ZDBC 7.2 handles array binding!
      {$ifdef USEZEOS}
      Test(TSQLDBZEOSConnectionProperties,TSQLDBZEOSConnectionProperties.URI(dOracle,''
        {$ifdef CPU64},'oci64\oci.dll'{$endif}),T,U,P,' Oracle',false);
      {$endif}
      //(*
      {$ifdef USEFIREDAC}
      {$ifdef CPU64}
      TADPhysOracleDriverLink.Create(Application).VendorLib := 'oci64\oci.dll';
      {$endif}
      Test(TSQLDBFireDACConnectionProperties,FIREDAC_PROVIDER[dOracle],T,U,P,' Oracle',false);
      {$endif}
      {$ifdef USEUNIDAC}
      Test(TSQLDBUniDACConnectionProperties,TSQLDBUniDACConnectionProperties.URI(
        dOracle,''{$ifdef CPU64},'oci64\oci.dll'{$endif}),T,U,P,' Oracle',false);
      {$endif}
    end;

    // -------- MS SQL Server 2012 Express LocalDB edition
    // get LocalDB from SqlLocalDB.MSI then get native 11.0 client sqlncli.msi
    // from http://www.microsoft.com/en-us/download/details.aspx?id=29065
    {$ifdef USELOCALDBMSSQLEXPRESS}
    Test(TOleDBMSSQL2012ConnectionProperties,'(localdb)\v11.0','','','',' local',false);
    Test(TODBCConnectionProperties,'','DRIVER=SQL Server Native Client 11.0;UID=.;'+
      'server=(localdb)\v11.0;Trusted_Connection=Yes','','',' MSSQL2012',false);
    {$ifdef USEFIREDAC}
    Test(TSQLDBFireDACConnectionProperties,
      FIREDAC_PROVIDER[dMSSQL]+'?server=(localDB)\v11.0;OSAuthent=Yes','','.','',' MSSQL2012',false);
    {$endif}
    {$ifdef USEUNIDAC}
    Test(TSQLDBUniDACConnectionProperties,TSQLDBUniDACConnectionProperties.URI(
      dMSSQL,'(localdb)\v11.0'),'','',' MSSQL2012',false);
    {$endif}
    {$endif USELOCALDBMSSQLEXPRESS}
    {$if defined (USEZEOS) and defined(ZEOS73UP)}
    Test(TSQLDBZEOSConnectionProperties,TSQLDBZEOSConnectionProperties.URI('odbc_w[mssql]', '', ''),
      'DRIVER={SQL Server Native Client 11.0};Server=(localdb)\ProjectsV13;DataBase=zeoslib;Trusted_Connection=Yes;MARS_Connection=yes',
      '','',' ODBC_W MSSQL2012',false);
    Test(TSQLDBZEOSConnectionProperties,TSQLDBZEOSConnectionProperties.URI('odbc_a[mssql]', '', ''),
      'DRIVER={SQL Server Native Client 11.0};Server=(localdb)\ProjectsV13;DataBase=zeoslib;Trusted_Connection=Yes;MARS_Connection=yes',
      '','',' ODBC_A MSSQL2012',false);
    Test(TSQLDBZEOSConnectionProperties,TSQLDBZEOSConnectionProperties.URI('OleDB[mssql]', '', ''),
      'Provider=SQLNCLI11.1;Integrated Security=SSPI;Persist Security Info=False;User ID="";Initial Catalog=zeoslib;'+
      'Data Source=(localdb)\ProjectsV13;MarsConn=Yes;Initial File Name="";Server SPN=""','','',' OleDB MSSQL2012',false);
    {$ifend}

    // -------- (local) MS SQL Server 2008 R2
    // get native 10.0 client e.g. as ENU/x86/sqlncli.msi or ENU/x64/sqlncli.msi
    {$ifdef USELOCALMSSQLEXPRESS}
    {$ifndef CPU64} // OleDB is buggy under Win64 (and deprecated) -> use ODBC
    Test(TOleDBMSSQL2008ConnectionProperties,'.\SQLEXPRESS','','','',' local',false);
    {$endif}
    Test(TODBCConnectionProperties,'','DRIVER=SQL Server Native Client 10.0;UID=.;'+
      'server=.\SQLEXPRESS;Trusted_Connection=Yes;MARS_Connection=yes','','',' MSSQL2008',false);
    {$ifdef USEFIREDAC}
    Test(TSQLDBFireDACConnectionProperties,
      FIREDAC_PROVIDER[dMSSQL]+'?server=.\SQLEXPRESS;OSAuthent=Yes','','.','',' MSSQL2008',false);
    {$endif}
    {$ifdef USEUNIDAC}
    Test(TSQLDBUniDACConnectionProperties,TSQLDBUniDACConnectionProperties.URI(
      dMSSQL,'.\SQLExpress'),'','',' MSSQL2008',false);
    {$endif}
    {$endif USELOCALMSSQLEXPRESS}

    // -------- local IBM DB2 10.5
    {$ifdef USELOCALDB2}
    Test(TODBCConnectionProperties,'','Driver=IBM DB2 ODBC DRIVER;Database=SAMPLE;'+
      'Hostname=localhost;Port=50000;UID=db2admin;Pwd=db2Password','','',' DB2',false);
    {$ifdef USEFIREDAC}
    Test(TSQLDBFireDACConnectionProperties,'DB2?Server=localhost;Port=50000',
      'SAMPLE','db2admin','db2Password',' DB2',false);
    {$endif}
    {$ifdef USEUNIDAC}
    // UniDAC has a huge performance and stability issue for DB2 -> disable
{      Test(TSQLDBUniDACConnectionProperties,TSQLDBUniDACConnectionProperties.URI(
      dDB2,'localhost:50000'),'SAMPLE','db2admin','db2Password',' DB2',false); }
    {$endif}
    {$endif USELOCALDB2}

    // -------- local PostgreSQL 9.2
    {$ifdef USELOCALPOSTGRESQL}
    {$ifdef USEZEOS}
    // direct ZDBC driver needs only libpq.dll and libintl.dll e.g. from
    // http://www.enterprisedb.com/products-services-training/pgbindownload
    Test(TSQLDBZEOSConnectionProperties,TSQLDBZEOSConnectionProperties.URI(
      dPostgreSQL,'localhost:5433'),'postgres','postgres','postgresPassword',' PostgreSQL',false);
    {$endif}
    // ODBC driver e.g. from http://ftp.postgresql.org/pub/odbc/versions/msi
    Test(TODBCConnectionProperties,'','Driver=PostgreSQL Unicode'+
      {$ifdef CPU64}'(x64)'+{$endif}';Database=postgres;'+
      'Server=localhost;Port=5433;UID=postgres;Pwd=postgresPassword','','',' PostgreSQL',false);
    {$ifdef USEFIREDAC}
    {$ifdef CPU64} // 64-bit server installed locally
    TADPhysPGDriverLink.Create(Application).VendorLib :=
      'c:\Program Files\PostgreSQL\9.2\bin\libpq.dll';
    {$endif}
    // direct FireDAC driver needs only libpq.dll and libintl.dll
      Test(TSQLDBFireDACConnectionProperties,'PG?Server=localhost;Port=5433',
      'postgres','postgres','postgresPassword',' PostgreSQL',false);
    {$endif}
    {$ifdef USEUNIDAC}
    Test(TSQLDBUniDACConnectionProperties,TSQLDBUniDACConnectionProperties.URI(
      dPostgreSQL,'localhost:5433'),'postgres','postgres','postgresPassword',' PostgreSQL',false);
    {$endif}
    {$endif USELOCALPOSTGRESQL}

    // -------- local MySQL 5.6
    {$ifdef USELOCALMYSQL}
    // run mysqld + mysqladmin -u root create test
    // ODBC driver e.g. from https://dev.mysql.com/downloads/connector/odbc
    // warning: connector 5.2.6 and 5.3.1 are dead slow in ODBC.FreeHandle()
    // seems not to be tied to SynDB: it is visible on ODBC Data Source tool
    // -> use ZEOS/ZDBC instead until fixed
    Test(TODBCConnectionProperties,'','Driver=MySQL ODBC 5.3 UNICODE Driver;Database=test;'+
      'Server=localhost;Port=3306;UID=root;Pwd=','','',' MySQL',false);
    {$ifdef USEZEOS}
    // direct ZDBC driver needs only libmysql.dll downloaded e.g. from
    // http://cdn.mysql.com/Downloads/Connector-C/mysql-connector-c-*-win32.zip
    Test(TSQLDBZEOSConnectionProperties,TSQLDBZEOSConnectionProperties.URI(
      dMySQL,'localhost:3306'),'test','root','',' MySQL',false);
    {$endif}
    {$ifdef USEFIREDAC}
    {$ifdef CPU64} // 64-bit server installed locally
    TADPhysMySQLDriverLink.Create(Application).VendorLib :=
      'c:\Program Files\MySQL\MySQL Server 5.6\lib\';
    {$endif}
    // direct FireDAC driver needs only libmysql.dll
    Test(TSQLDBFireDACConnectionProperties,'MySQL?Server=localhost;Port=3306',
      'test','root','',' MySQL',false);
    {$endif}
    {$ifdef USEUNIDAC}
    Test(TSQLDBUniDACConnectionProperties,TSQLDBUniDACConnectionProperties.URI(
      dMySQL,'localhost:3306'),'test','root','',' MySQL',false);
    {$endif}
    {$endif}
    //*)
  except
    on E: Exception do
      LogMemo.Lines.Add(E.Message);
  end;
  finally
    Label3.Caption := '';
    T := ObjectToJSON(Stats,[woHumanReadable]);
    FileFromString(T,ChangeFileExt(ExeVersion.ProgramFileName,'.stats'));
    FileFromString(T,Ansi7ToString(NowToString(false))+'.log');
    SaveStats;
  end;
end;

type
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
    property LastChange: TModTime read fLastChange;
    property CreatedAt: TCreateTime read fCreatedAt write fCreatedAt;
  end;

procedure TMainForm.Test(PropsClass: TSQLDBConnectionPropertiesClass;
  const aServerName, aDatabaseName, aUserID, aPassWord, aTrailDesc: RawUTF8;
  DBIsFile: boolean; Mode: TSQLSynchronousMode; Lock: TSQLLockingMode);
var aUseBatch, aUseTransactions, aUseDirect: boolean;
    Props: TSQLDBConnectionProperties;
    Model: TSQLModel;
    Client: TSQLRestClientDB;
    Value: TSQLRecordSample;
    ValueLastName, ValueFirstName: TRawUTF8DynArray;
    Stat: TStat;
    Start: TTimeLog;
    Timer: TPrecisionTimer;
    Res: TIDDynArray;
    forceID: boolean;
    U, Server,DBName, MainDBName, Num, Time: RawUTF8;
    Rate, i: integer;
    {$ifdef USEMONGODB}
    MongoClient: TMongoClient;
    MongoDatabase: TMongoDatabase;
    {$endif}

  procedure ValueCheck;
  var err: RawUTF8;
  begin
    err := '';
    if Value.fID<>Res[i] then
      err := FormatUTF8('Value.fID=Res[i] %<>% ',[Value.fID,Res[i]]);
    if PInt64(@Value.Amount)^<>(i+1)*100 then
      err := FormatUTF8('%Value.Amount=(i+1)*0.01 %<>% ',
        [err,Value.Amount,(i+1)*0.01]);
    if Value.LastChange<Start then
      err := FormatUTF8('%Value.LastChange>=Start %>=%',
        [err,Value.LastChange,Start]);
    if Value.FirstName<>ValueFirstName[i] then
      err := FormatUTF8('%Value.FirstName="%" <> ValueFirstName[i]="%"',
        [err,Value.FirstName,ValueFirstName[i]]);
    assert(err='',string(Stat.fEngine+' read failure: '+err));
    Value.ClearProperties;
  end;
begin
  U := 'Namee ';
  UniqueRawUTF8(U); // FPC does not call it
  PWord(@U[4])^ := $a9c3;  // some 'e'acute to test UTF-8 encoding
  Stat := TStat.Create;
  Stat.fEngine := PropsClass.EngineName;
  if aTrailDesc<>'' then
    Stat.fEngine := Stat.fEngine+aTrailDesc;
  Model := TSQLModel.Create([TSQLRecordSample]);
  Value := TSQLRecordSample.Create;
  Num := '1';
  for aUseTransactions := false to true do
  for aUseBatch := false to true do begin
    // open connection and initialize mORMot Client-Server instance
    Label3.Caption := Format('Running tests phase #%s on %s...',[Num,Stat.fEngine]);
    Application.ProcessMessages;
    DBName := aDatabaseName;
    if (aServerName='') and ((PropsClass=nil) or
       (DBIsFile or not PropsClass.InheritsFrom(TODBCConnectionProperties))) then begin
      Server := LowerCaseU(Stat.Engine)+'.'+Num;
      if (PropsClass<>nil) and PropsClass.InheritsFrom(TODBCConnectionProperties) then
        if PosEx('Firebird/InterBase',DBName)>0 then begin
        Server := StringToUTF8(ExpandFileName(UTF8ToString(Server)));
        DBName := DBName+';DBNAME='+Server;
        {$ifdef USEZEOS} // ODBC is not able to create the DB file :(
        DeleteFile(UTF8ToString(Server));
        with TSQLDBZEOSConnectionProperties.Create(TSQLDBZEOSConnectionProperties.URI(
          dFirebird,'',FIREBIRD_LIB),Server,'','') do
        try
          ThreadSafeConnection.Connect; // will create the database file
        finally
          Free;
        end;
        {$else} // void an existing file
        with TODBCConnectionProperties.Create('',DBName,'','') do
        try
          ExecuteNoResult('drop table SAMPLERECORD',[]);
        finally
          Free;
        end;
        {$endif}
        Server := '';
      end else
      if PosEx('SQLite3 ODBC',DBName)>0 then begin
        Server := StringToUTF8(ExpandFileName(UTF8ToString(Server)));
        DBName := DBName+';DATABASE='+Server;
        DeleteFile(UTF8ToString(Server));
        Server := '';
      end else
        DeleteFile(UTF8ToString(Server)) else
        DeleteFile(UTF8ToString(Server));
    end else begin
      Server := aServerName;
      if DBIsFile and (DBName='') then begin
        DBName := LowerCaseU(Stat.Engine)+'.'+Num;
        DeleteFile(UTF8ToString(DBName));
      end;
    end;
    if PropsClass<>nil then begin
      MainDBName := SQLITE_MEMORY_DATABASE_NAME;
      Props := PropsClass.Create(Server,DBName,aUserID,aPassWord);
      {$ifdef USENEXUSDB}
      if PropsClass=TSQLDBNexusDBConnectionProperties then
        TSQLDBNexusDBConnectionProperties(Props).DeleteDatabase;
      {$endif}
    end else begin
      MainDBName := Server;
      Props := nil;
    end;
    {$ifdef USEMONGODB}
    MongoClient := nil;
    {$endif}
    try
      if Server='SQL' then begin
        MainDBName := SQLITE_MEMORY_DATABASE_NAME;
        Model.VirtualTableRegister(TSQLRecordSample,TSQLVirtualTableBinary);
      end else
      if Server='MongoDB' then
        MainDBName := SQLITE_MEMORY_DATABASE_NAME else
        // do nothing if Props=nil
        VirtualTableExternalRegister(Model,TSQLRecordSample,Props,'SampleRecord');
      Client := TSQLRestClientDB.Create(Model,nil,string(MainDBName),TSQLRestServerDB,false,'');
      if Server='static' then begin
        DeleteFile('static.data');
        Client.Server.StaticDataCreate(TSQLRecordSample,'static.data',true);
      {$ifdef USEMONGODB}
      end else
      if Server='MongoDB' then begin
        MongoClient := TMongoClient.Create('localhost',27017);
        if aTrailDesc='MongoDB (no ack)' then
          MongoClient.WriteConcern := wcUnacknowledged;
        MongoDatabase := MongoClient.Database['dbperf'];
        StaticMongoDBRegister(TSQLRecordSample,Client.Server,MongoDatabase,'perftest').
          Drop;
      {$endif}
      end;
      Client.Server.DB.Synchronous := Mode;
      Client.Server.DB.LockingMode := Lock;
      if PropsClass=TSQLDBSQLite3ConnectionProperties then
      with TSQLDBSQLite3Connection(Props.MainConnection) do begin
        Synchronous := Mode;
        LockingMode := Lock;
      end;
      try
        // huge insertion in virtual table, with 4 kinds of process
        Timer.Start;
        Client.Server.CreateMissingTables;
{        Props.ExecuteNoResult(
          'insert into SampleRecord (ID,FirstName,LastName,Amount,LastChange,CreatedAt) VALUES (?,?,?,?,?,?)',
          [1,U,'B',10.02,10,20]);
          //'insert into SampleRecord (ID,BirthDate) values (?,null)',[1.0]);
        U := Props.Execute('select * from samplerecord',[]).FetchAllAsJSON(true); }
        Start := Client.ServerTimeStamp-1;
        if Stat.CreateTableTime='' then
          Stat.fCreateTable := Timer.Stop;
        if (Mode=smFull) and not aUseTransactions then
          Stat.fNumberOfElements := 500 else // SQLite3 is dead slow without transactions
        {if (PropsClass=TOleDBJetConnectionProperties) or
           (PropsClass=TODBCConnectionProperties) then
          Stat.fNumberOfElements := 1000 else}
          Stat.fNumberOfElements := 5000;
        //  Stat.fNumberOfElements := 50;
        SetLength(ValueLastName,Stat.fNumberOfElements);
        SetLength(ValueFirstName,Stat.fNumberOfElements);
        for i := 0 to Stat.fNumberOfElements-1 do begin
          ValueLastName[i] := Int32ToUtf8(i+1);
          {$ifndef UNIK}
          if i<>100 then // test https://synopse.info/fossil/info/e8c211062e
          {$endif}
            ValueFirstName[i] := U+ValueLastName[i];
        end;
        Timer.Start;
        if aUseTransactions then
          Client.TransactionBegin(TSQLRecordSample);
        if aUseBatch then
          Client.BatchStart(TSQLRecordSample) else
          SetLength(Res,Stat.fNumberOfElements);
        Value.BirthDate := 0;
        for i := 0 to Stat.fNumberOfElements-1 do begin
          Value.Amount := (i+1)*0.01;
          Value.LastName := ValueLastName[i];
          Value.FirstName := ValueFirstName[i];
          forceID := i and 3=1;
          if forceID then
            Value.fID := Res[i-1]+1;
          {$ifdef UNIK}
          if (Server='static') or (Server='SQL') then
            forceID := false; // not yet in TSQLRestStorageInMemory.AddOne
          {$endif}
          if aUseBatch then
            Client.BatchAdd(Value,true,forceID) else
            Res[i] := Client.Add(Value,true,forceID);
          Value.BirthDate := Value.BirthDate+1;
        end;
        if aUseBatch then
          Client.BatchSend(Res);
        if aUseTransactions then
          Client.Commit;
        Time := Timer.Stop;
        i := 1;
        Value.ClearProperties;
        Client.Retrieve(Res[i],Value);
        ValueCheck;
        Rate := Timer.PerSec(Stat.fNumberOfElements);
        case Num[1] of
        '1': begin
          Stat.fInsertTime := Time;
          Stat.fInsertRate := Rate;
        end;
        '2': begin
          Stat.fInsertBatchTime := Time;
          Stat.fInsertBatchRate := Rate;
        end;
        '3': begin
          Stat.fInsertTransactionTime := Time;
          Stat.fInsertTransactionRate := Rate;
        end;
        '4': begin
          Stat.fInsertBatchTransactionTime := Time;
          Stat.fInsertBatchTransactionRate := Rate;
          Label3.Caption := Format('Running reading tests on %s...',[Stat.fEngine]);
          Application.ProcessMessages;
          // one by one retrieve values from server
          Timer.Start;
          for i := 0 to Stat.fNumberOfElements-1 do begin
            Client.Retrieve(Res[i],Value);
            ValueCheck;
          end;
          Stat.fReadOneByOneTime := Timer.Stop;
          Stat.fReadOneByOneRate := Timer.PerSec(Stat.fNumberOfElements);
          {$ifdef UNIK}
          // one by one retrieve values using Name property
          Timer.Start;
          for i := 0 to Stat.fNumberOfElements-1 do begin
            Client.Retrieve('LastName=?',[],[ValueLastName[i]],Value);
            assert((Value.fID=Res[i])and
              (PInt64(@Value.Amount)^=(i+1)*100)and(Value.LastChange>=Start));
          end;
          Stat.fReadOneByNameTime := Timer.Stop;
          Stat.fReadOneByNameRate := Timer.PerSec(Stat.fNumberOfElements);
          {$endif}
          // retrieve all rows with or without the virtual module
          for aUseDirect := false to true do begin
            with Client.Server do begin
              Cache.Flush; // fair benchmark
              DB.CacheFlush; // fair benchmark (16100 rows/s->456000 with cache!)
              StaticVirtualTableDirect := aUseDirect;
            end;
            Timer.Start;
            Value.ClearProperties;
            if Server='SQL' then
              Value.FillPrepare(Client,'') else
              Value.FillPrepare(Client,'order by RowId');
            //FileFromString((Value.FillTable as TSQLTableJSON).PrivateInternalCopy,Stat.Engine+'.json');
            i := 0;
            while Value.FillOne do begin
              ValueCheck;
              {if err<>'' then
                FileFromString((Value.FillTable as TSQLTableJSON).PrivateInternalCopy,
                  Stat.fEngine+'.json');}
              inc(i);
            end;
            assert(i=Stat.fNumberOfElements);
            if aUseDirect then begin
              Stat.fReadAllDirectTime := Timer.Stop;
              Stat.fReadAllDirectRate := Timer.PerSec(Stat.fNumberOfElements);
            end else begin
              Stat.fReadAllVirtualTime := Timer.Stop;
              Stat.fReadAllVirtualRate := Timer.PerSec(Stat.fNumberOfElements);
            end;
          end;
{          // backup (for testing purposes)
          if MainDBName<>SQLITE_MEMORY_DATABASE_NAME then
            Client.Server.BackupGZ(MainDBName+'.gz'); } 
        end;
        end;
      finally
        Timer.Start;
        try
          {$ifdef USEMONGODB}
          if Server='MongoDB' then
            (Client.Server.StaticDataServer[TSQLRecordSample] as
              TSQLRestStorageMongoDB).Drop else
          {$endif}
          if not DBIsFile then begin
            Client.Server.FlushStatementCache;
            Client.Server.ExecuteFmt('drop table %',[Value.SQLTableName]);
          end;
        finally
          Client.Free;
        end;
        Stat.fClientCloseTime := Timer.Stop;
      end;
      inc(Num[1]);
    finally
      Props.Free;
      {$ifdef USEMONGODB}
      if Server='MongoDB' then
        MongoClient.Free; // will also free MongoDatabase instance
      {$endif}
    end;
  end;
  Stats.Add(Stat);
  Model.Free;
  Value.Free;
  LogMemo.Lines.Add(UTF8ToString(ObjectToJSON(Stat,[woHumanReadable])));
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Stats.Free;
end;


procedure TMainForm.SaveStats;
type TStatArray = array[0..1000] of TStat;
var Stat: ^TStatArray;
    mode,s,txt: RawUTF8;
    m,nCat: integer;
    max,Cat1,Cat2,Eng1,Eng2,Eng: RawUTF8;
    Rows: TRawUTF8DynArray;
    Doc: RawUTF8;
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
  for i := 0 to high(Cat) do begin
    Rows[0] := Rows[0]+'<td><b>'+Cat[i]+'</b></td>';
    Cat1 := Cat1+UrlEncode(Cat[i])+'|';
    Cat2 := Cat2+UrlEncode(Cat[high(Cat)-i])+'|';
  end;
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
    Doc := Doc+StringReplaceAll(StringReplaceAll(StringReplaceAll(StringReplaceAll(
      StringReplaceAll(StringReplaceAll(StringReplaceAll(StringReplaceAll(
      Rows[i],'</td>',''),'</tr>',''),'<tr align=center>',''),
      '</b>','}'),'</td>',''),'<b>','{\b '),'<td>','|'),'&nbsp;','')+#13#10;
  end;
  Doc := Doc+'|%'#13#10;
end;
var i: integer;
begin
  Stat := pointer(Stats.List);
  s := FormatUTF8('<p>Running tests using Synopse mORMot framework %, '+
    'compiled with %, against SQLite %, at %.</p>',
    [SYNOPSE_FRAMEWORK_VERSION,GetDelphiCompilerVersion,SQLite3.libversion,NowToString]);
  m := 0;
  for i := 0 to Stats.Count-1 do
    with Stat[i] do begin
      if InsertRate>m then m := InsertRate;
      if InsertBatchRate>m then m := InsertBatchRate;
      if InsertTransactionRate>m then m := InsertTransactionRate;
      if InsertBatchTransactionRate>m then m := InsertBatchTransactionRate;
    end;
  SetCategories('Insertion speed (rows/second)',['Direct','Batch','Trans','Batch Trans']);
  Pic1(Cat2,5);
  for i := 0 to Stats.Count-1 do
  with Stat[i] do begin
    txt := FormatUTF8('%%,%,%,%|',
      [txt,InsertRate,InsertBatchRate,InsertTransactionRate,InsertBatchTransactionRate]);
    Rows[i+1] := FormatUTF8('%<td>%</td><td>%</td><td>%</td><td>%</td>',
      [Rows[i+1],InsertRate,InsertBatchRate,InsertTransactionRate,InsertBatchTransactionRate]);
  end;
  Table;
  PicEnd(Eng1);

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
  SetCategories('Read speed (rows/second)',['By one',
    {$ifdef UNIK}'By name',{$endif}'All Virtual','All Direct']);
  Pic1(Cat2,{$ifdef UNIK}4{$else}3{$endif});
  for i := 0 to Stats.Count-1 do
  with Stat[i] do begin
    txt := FormatUTF8({$ifdef UNIK}'%%,%,%,%|'{$else}'%%,%,%|'{$endif},
      [txt,ReadOneByOneRate,{$ifdef UNIK}ReadOneByNameRate,{$endif}
        ReadAllVirtualRate,ReadAllDirectRate]);
    Rows[i+1] := FormatUTF8('%<td>%</td>',[Rows[i+1],ReadOneByOneRate]);
    {$ifdef UNIK}
    Rows[i+1] := FormatUTF8('%<td>%</td>',[Rows[i+1],ReadOneByNameRate]);
    {$endif}
    Rows[i+1] := FormatUTF8('%<td>%</td><td>%</td>',
      [Rows[i+1],ReadAllVirtualRate,ReadAllDirectRate]);
  end;
  Table;
  PicEnd(Eng1);

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

  FileFromString(Doc,ChangeFileExt(ExeVersion.ProgramFileName,'.txt'));
  FileFromString('<html><body>'#13#10+s,ChangeFileExt(ExeVersion.ProgramFileName,'.htm'));
end;

procedure TMainForm.FormShow(Sender: TObject);
var Valid: boolean;
    S: RawUTF8;
begin
  btnReport.Visible := DebugHook=0;
  exit;
  S := StringFromFile('PerfTestBlog.stats');
  JSONToObject(Stats,pointer(S),Valid,TStat);
  if Valid then
    SaveStats;
  Close;
end;


procedure TMainForm.btnReportClick(Sender: TObject);
begin
  ShellExecute(0,'open',pointer(ChangeFileExt(ExeVersion.ProgramFileName,'.htm')),'','',SW_SHOWMAXIMIZED);
end;

end.
