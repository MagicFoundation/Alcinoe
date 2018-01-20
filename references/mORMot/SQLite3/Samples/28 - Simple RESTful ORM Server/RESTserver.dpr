/// minimal REST server for a list of Persons stored on PostgreSQL
program RESTserver;

// see https://synopse.info/forum/viewtopic.php?pid=10882#p10882

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc}  // use FastMM4 on older Delphi, or set FPC threads
  SynCommons,          // framework core
  SynLog,              // logging features
  mORMot,              // RESTful server & ORM
  mORMotSQLite3,       // SQLite3 engine as ORM core
  SynSQLite3Static,    // staticaly linked SQLite3 engine
  mORMotDB,            // ORM using external DB
  mORMotHttpServer,    // HTTP server for RESTful server
  SynDB,               // external DB core
  SynDBODBC,           // external DB access via ODBC
  RESTModel;           // data model unit, shared between server and client

var
  aModel: TSQLModel;
  aProps: TSQLDBConnectionProperties;
  aRestServer: TSQLRestServerDB;
  aHttpServer: TSQLHttpServer;
begin
  // set logging abilities
  SQLite3Log.Family.Level := LOG_VERBOSE;
  //SQLite3Log.Family.EchoToConsole := LOG_VERBOSE;
  SQLite3Log.Family.PerThreadLog := ptIdentifiedInOnFile;
  // ODBC driver e.g. from http://ftp.postgresql.org/pub/odbc/versions/msi
  aProps := TODBCConnectionProperties.Create('','Driver=PostgreSQL Unicode'+
      {$ifdef CPU64}'(x64)'+{$endif}';Database=postgres;'+
      'Server=localhost;Port=5433;UID=postgres;Pwd=postgresPassword','','');
  try
    // get the shared data model
    aModel := DataModel;
    // use PostgreSQL database for all tables
    VirtualTableExternalRegisterAll(aModel,aProps);
    try
      // create the main mORMot server
      aRestServer := TSQLRestServerDB.Create(aModel,':memory:',false); // authentication=false
      try
        // optionally execute all PostgreSQL requests in a single thread
        aRestServer.AcquireExecutionMode[execORMGet] := amBackgroundORMSharedThread;
        aRestServer.AcquireExecutionMode[execORMWrite] := amBackgroundORMSharedThread;
        // create tables or fields if missing
        aRestServer.CreateMissingTables;
        // serve aRestServer data over HTTP
        aHttpServer := TSQLHttpServer.Create(SERVER_PORT,[aRestServer],'+',useHttpApiRegisteringURI);
        try
          aHttpServer.AccessControlAllowOrigin := '*'; // allow cross-site AJAX queries
          writeln('Background server is running.'#10);
          write('Press [Enter] to close the server.');
          readln;
        finally
          aHttpServer.Free;
        end;
      finally
        aRestServer.Free;
      end;
    finally
      aModel.Free;
    end;
  finally
    aProps.Free;
  end;
end.

