/// this server will use TSQLRestServerDB over HTTP
program Project21HttpServer;

{$APPTYPE CONSOLE}

// first line of uses clause must be   {$I SynDprUses.inc}
uses
  {$I SynDprUses.inc}
  SysUtils,
  Classes,
  SynCommons,
  SynLog,
  mORMot,
  mORMotSQlite3,
  SynSQLite3,
  SynSQLite3Static,
  mORMotHttpServer;

type
  TSQLRecordPeople = class(TSQLRecord)
  private
    fFirstName: RawUTF8;
    fLastName: RawUTF8;
    fYearOfBirth: integer;
    fYearOfDeath: word;
  published
    property FirstName: RawUTF8 read fFirstName write fFirstName;
    property LastName: RawUTF8 read fLastName write fLastName;
    property YearOfBirth: integer read fYearOfBirth write fYearOfBirth;
    property YearOfDeath: word read fYearOfDeath write fYearOfDeath;
  end;

var
  aDatabaseFile: TFileName;
  aModel: TSQLModel;
  aServer: TSQLRestServerDB;
  aHTTPServer: TSQLHttpServer;
begin
  // define the log level
  with TSQLLog.Family do begin
    Level := LOG_STACKTRACE;
    EchoToConsole := LOG_VERBOSE; // events to the console
    PerThreadLog := ptIdentifiedInOnFile;
  end;
  // create a Data Model
  aModel := TSQLModel.Create([TSQLRecordPeople]);
  try
    aDatabaseFile := ChangeFileExt(ExeVersion.ProgramFileName,'.db3');
    DeleteFile(aDatabaseFile);
    aServer := TSQLRestServerDB.Create(aModel,aDatabaseFile);
    try
      aServer.AcquireWriteTimeOut := 15000;   // 15 seconds before write failure
      aServer.DB.Synchronous := smOff;
      aServer.DB.LockingMode := lmExclusive;  // off+exclusive = fastest SQLite3  
      aServer.NoAJAXJSON := true;
      aServer.CreateMissingTables;
      // launch the server
      aHTTPServer := TSQLHttpServer.Create('888',[aServer]);
      try
        writeln(#13#10'Background server is running at http://localhost:888'#13#10+
                #13#10'Press [Enter] to close the server.');
        ConsoleWaitForEnterKey;
        with TSQLLog.Family do
          if not (sllInfo in Level) then // let global server stats be logged
            Level := Level+[sllInfo];
      finally
        aHTTPServer.Free;
      end;
    finally
      aServer.Free;
    end;
  finally
    aModel.Free;
  end;
end.
